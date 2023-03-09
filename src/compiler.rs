use crate::{
    chunk::{Chunk, OpCode},
    scanner::{Scanner, Token, TokenKind},
    value::Value,
};

pub fn compile(source: &str, chunk: &mut Chunk) -> bool {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner, chunk);

    parser.advance();
    parser.expression();
    parser.consume(T![EOF], "Expect end of expression.");
    parser.end_compiler();

    !parser.had_error
}

// FIXME: try to make this a real
#[allow(non_snake_case)]
mod Precedence {
    pub const NONE: u8 = 0;
    pub const ASSIGNMENT: u8 = 1; // =
    pub const OR: u8 = 2; // or
    pub const AND: u8 = 3; // and
    pub const EQUALITY: u8 = 4; // == !=
    pub const COMPARISON: u8 = 5; // < > <= >=
    pub const TERM: u8 = 6; // + -
    pub const FACTOR: u8 = 7; // * /
    pub const UNARY: u8 = 8; // ! -
    pub const CALL: u8 = 9; // . ()
    pub const PRIMARY: u8 = 10;
}

#[derive(Clone, Copy)]
pub struct ParseRule {
    prefix: Option<fn(&mut Parser<'_>)>,
    infix: Option<fn(&mut Parser<'_>)>,
    precedence: u8,
}

impl ParseRule {
    const fn none() -> Self {
        Self {
            prefix: None,
            infix: None,
            precedence: 0,
        }
    }

    const fn prefix(prefix: fn(&mut Parser<'_>)) -> Self {
        Self {
            prefix: Some(prefix),
            infix: None,
            precedence: 0,
        }
    }

    const fn infix(infix: fn(&mut Parser<'_>), precedence: u8) -> Self {
        Self {
            prefix: None,
            infix: Some(infix),
            precedence,
        }
    }

    const fn new(prefix: fn(&mut Parser<'_>), infix: fn(&mut Parser<'_>), precedence: u8) -> Self {
        Self {
            prefix: Some(prefix),
            infix: Some(infix),
            precedence,
        }
    }
}

pub struct Parser<'a> {
    scanner: Scanner<'a>,
    current: Token<'a>,
    previous: Token<'a>,
    had_error: bool,
    panic_mode: bool,
    chunk: &'a mut Chunk,
}

impl<'a> Parser<'a> {
    fn new(scanner: Scanner<'a>, chunk: &'a mut Chunk) -> Self {
        Self {
            scanner,
            current: Token::dummy(),
            previous: Token::dummy(),
            had_error: false,
            panic_mode: false,
            chunk,
        }
    }

    fn advance(&mut self) {
        self.previous = self.current;

        loop {
            self.current = self.scanner.scan_token();
            if self.current.kind != T![ERR] {
                break;
            }
            self.error_at_current(self.current.lexeme);
        }
    }

    fn consume(&mut self, expected: TokenKind, msg: &str) {
        if self.current.kind == expected {
            self.advance();
        } else {
            self.error_at_current(msg)
        }
    }

    // FIXME: Can this take an opcode and not a byte?
    fn emit_byte(&mut self, byte: u8) {
        let line = self.previous.line;
        self.current_chunk().write_chunk(byte, line);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_constant(&mut self, value: Value) {
        let index = self.make_constant(value);
        self.emit_bytes(OpCode::OpConstant as u8, index);
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        let index = self.current_chunk().add_constant(value);
        if index > u8::MAX as usize {
            self.error("Too many constants in one chunk.");
            0
        } else {
            index as u8
        }
    }

    fn end_compiler(&mut self) {
        self.emit_return();

        #[cfg(debug_assertions)]
        {
            if !self.had_error {
                self.current_chunk().disassemble_chunk("code");
            }
        }
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::OpReturn as u8)
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        self.chunk
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::ASSIGNMENT)
    }

    fn get_rule(&self, kind: TokenKind) -> ParseRule {
        // Safety: RULES always covers all variants of TokenKind
        unsafe { *jump_table::RULES.get_unchecked(kind as usize) }
    }

    // FIXME: make precedence not usize
    fn parse_precedence(&mut self, precedence: u8) {
        self.advance();

        let Some(prefix_rule) = self.get_rule(self.previous.kind).prefix else {
            self.error("Expect expression.");
            return;
        };

        prefix_rule(self);

        while precedence <= self.get_rule(self.current.kind).precedence {
            self.advance();
            // FIXME: How to remove this unwrap?
            let infix_rule = self.get_rule(self.previous.kind).infix.unwrap();
            infix_rule(self);
        }
    }
}

// error handling
impl<'a> Parser<'a> {
    fn error_at_current(&mut self, msg: &str) {
        self.error_at(self.current, msg)
    }

    fn error(&mut self, msg: &str) {
        self.error_at(self.previous, msg)
    }

    fn error_at(&mut self, token: Token<'a>, msg: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        eprint!("[line {}] Error", token.line);

        match token.kind {
            T![EOF] => eprint!(" at the end"),
            T![ERR] => {}
            _ => eprint!(" at {}", token.lexeme),
        }

        eprintln!(": {msg}");
        self.had_error = true;
    }
}

mod jump_table {
    use std::mem;

    use crate::{
        chunk::OpCode,
        compiler::{ParseRule, Parser, Precedence},
        scanner::TokenKind,
        value::Value,
    };

    // FIXME: Replace this dumb thing with a simple match (check asm)
    pub static RULES: [ParseRule; mem::variant_count::<TokenKind>()] = {
        use Precedence::*;
        let mut rules = [ParseRule::none(); mem::variant_count::<TokenKind>()];

        rules[T!['('] as usize] = ParseRule::prefix(grouping);
        rules[T![-] as usize] = ParseRule::new(unary, binary, TERM);
        rules[T![+] as usize] = ParseRule::infix(binary, TERM);
        rules[T![/] as usize] = ParseRule::infix(binary, FACTOR);
        rules[T![*] as usize] = ParseRule::infix(binary, FACTOR);
        rules[T![number] as usize] = ParseRule::prefix(number);

        rules
    };

    pub fn number(parser: &mut Parser<'_>) {
        let value: Value = parser.previous.lexeme.parse().unwrap();
        parser.emit_constant(value);
    }

    pub fn grouping(parser: &mut Parser<'_>) {
        parser.expression();
        parser.consume(T![')'], "Expect ')' after expression.")
    }

    pub fn unary(parser: &mut Parser<'_>) {
        let operator = parser.previous.kind;

        // Compile the operand.
        parser.parse_precedence(Precedence::UNARY);

        // Emit the operator instruction.
        match operator {
            T![-] => parser.emit_byte(OpCode::OpNegate as u8),
            _ => {}
        }
    }

    pub fn binary(parser: &mut Parser<'_>) {
        let operator = parser.previous.kind;
        let rule = parser.get_rule(operator);
        parser.parse_precedence(rule.precedence + 1);

        match operator {
            T![+] => parser.emit_byte(OpCode::OpAdd as u8),
            T![-] => parser.emit_byte(OpCode::OpSubtract as u8),
            T![*] => parser.emit_byte(OpCode::OpMultiply as u8),
            T![/] => parser.emit_byte(OpCode::OpDivide as u8),
            _ => {}
        }
    }
}
