use crate::{
    chunk::{Chunk, OpCode},
    scanner::{Scanner, Token, TokenKind},
    value::Value,
    vm::VM,
};

pub fn compile(vm: &mut VM, source: &str, chunk: &mut Chunk) -> bool {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(vm, scanner, chunk);

    parser.advance();

    while !parser.matches(T![EOF]) {
        parser.declaration();
    }

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

type ParseFn = fn(&mut Parser<'_>, bool);

#[derive(Clone, Copy)]
pub struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: u8,
}

pub struct Parser<'a> {
    scanner: Scanner<'a>,
    current: Token<'a>,
    previous: Token<'a>,
    had_error: bool,
    panic_mode: bool,
    chunk: &'a mut Chunk,
    vm: &'a mut VM,
}

impl<'a> Parser<'a> {
    fn new(vm: &'a mut VM, scanner: Scanner<'a>, chunk: &'a mut Chunk) -> Self {
        Self {
            scanner,
            current: Token::dummy(),
            previous: Token::dummy(),
            had_error: false,
            panic_mode: false,
            chunk,
            vm,
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

    fn matches(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&mut self, kind: TokenKind) -> bool {
        self.current.kind == kind
    }

    fn emit_opcode(&mut self, op_code: OpCode) {
        self.emit_byte(op_code as u8)
    }

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
        self.emit_bytes(OpCode::OP_CONSTANT as u8, index);
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

        #[cfg(feature = "debug_prints")]
        {
            if !self.had_error {
                self.current_chunk().disassemble_chunk("code");
            }
        }
    }

    fn emit_return(&mut self) {
        self.emit_opcode(OpCode::OP_RETURN)
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        self.chunk
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::ASSIGNMENT)
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.matches(T![=]) {
            self.expression();
        } else {
            self.emit_opcode(OpCode::OP_NIL);
        }

        self.consume(T![;], "Expect ';' after variable declaration.");

        self.define_variable(global);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(T![;], "Expect ';' after expression.");
        self.emit_opcode(OpCode::OP_POP);
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(T![;], "Expect ';' after value.");
        self.emit_opcode(OpCode::OP_PRINT)
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while self.current.kind != T![EOF] {
            if self.previous.kind == T![;] {
                return;
            }

            if let T![class]
            | T![fun]
            | T![var]
            | T![for]
            | T![if]
            | T![while]
            | T![print]
            | T![return] = self.current.kind
            {
                return;
            }

            self.advance()
        }
    }

    fn declaration(&mut self) {
        if self.matches(T![var]) {
            self.var_declaration()
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn statement(&mut self) {
        if self.matches(T![print]) {
            self.print_statement();
        } else {
            self.expression_statement();
        }
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

        let can_assign = precedence <= Precedence::ASSIGNMENT;
        prefix_rule(self, can_assign);

        while precedence <= self.get_rule(self.current.kind).precedence {
            self.advance();
            // FIXME: How to remove this unwrap?
            let infix_rule = self.get_rule(self.previous.kind).infix.unwrap();
            infix_rule(self, can_assign);
        }

        if can_assign && self.matches(T![=]) {
            self.error("Invalid assignment target.");
        }
    }

    fn identifier_constant(&mut self, token: Token<'_>) -> u8 {
        let string = self.vm.copy_string(token.lexeme);
        self.make_constant(Value::Obj(string))
    }

    fn parse_variable(&mut self, msg: &str) -> u8 {
        self.consume(T![ident], msg);
        self.identifier_constant(self.previous)
    }

    fn define_variable(&mut self, global: u8) {
        self.emit_bytes(OpCode::OP_DEFINE_GLOBAL as u8, global)
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
    use crate::{
        chunk::OpCode,
        compiler::{ParseRule, Parser, Precedence},
        scanner::{Token, TokenKind},
        value::Value,
    };
    use std::mem;
    use Precedence::*;

    #[rustfmt::skip]
    macro_rules! rules_inner {
        ($rules:expr; $kind:expr => [None, $infix:tt, $precedence:tt]) => {
            $rules[$kind as usize] = ParseRule { prefix: None, infix: Some($infix), precedence: $precedence };
        };
        ($rules:expr; $kind:expr => [$prefix:tt, None, $precedence:tt]) => {
            $rules[$kind as usize] = ParseRule { prefix: Some($prefix), infix: None, precedence: $precedence };
        };
        ($rules:expr; $kind:expr => [$prefix:tt, $infix:tt, $precedence:tt]) => {
            $rules[$kind as usize] = ParseRule { prefix: Some($prefix), infix: Some($infix), precedence: $precedence};
        };
    }

    macro_rules! rules {
        ($([$kind:expr, $prefix:tt, $infix:tt, $precedence:tt])*) => {{
            let mut rules = [ParseRule {prefix: None, infix: None, precedence: NONE}; mem::variant_count::<TokenKind>()];
            $(
                rules_inner!(rules; $kind => [$prefix, $infix, $precedence]);
            )*

            rules
        }};
    }

    // FIXME: Replace this dumb thing with a simple match (check asm)
    pub static RULES: [ParseRule; mem::variant_count::<TokenKind>()] = rules! {
        [T![-],      unary,    binary, TERM]
        [T!['('],    grouping, None,   NONE]
        [T![number], number,   None,   NONE]
        [T![false],  literal,  None,   NONE]
        [T![true],   literal,  None,   NONE]
        [T![nil],    literal,  None,   NONE]
        [T![str],    string,   None,   NONE]
        [T![!],      unary,    None,   NONE]
        [T![ident],  variable, None,   NONE]
        [T![+],      None,     binary, TERM]
        [T![/],      None,     binary, FACTOR]
        [T![*],      None,     binary, FACTOR]
        [T![!=],     None,     binary, EQUALITY]
        [T![==],     None,     binary, EQUALITY]
        [T![>],      None,     binary, COMPARISON]
        [T![>=],     None,     binary, COMPARISON]
        [T![<],      None,     binary, COMPARISON]
        [T![<=],     None,     binary, COMPARISON]
    };

    fn string(parser: &mut Parser<'_>, _: bool) {
        let lexeme = parser.previous.lexeme;
        let obj = parser.vm.copy_string(&lexeme[1..][..lexeme.len() - 2]);
        parser.emit_constant(Value::Obj(obj))
    }

    fn named_variable(parser: &mut Parser<'_>, token: Token<'_>, can_assign: bool) {
        let arg = parser.identifier_constant(token);

        if can_assign && parser.matches(T![=]) {
            parser.expression();
            parser.emit_bytes(OpCode::OP_SET_GLOBAL as u8, arg);
        } else {
            parser.emit_bytes(OpCode::OP_GET_GLOBAL as u8, arg);
        }
    }

    fn variable(parser: &mut Parser<'_>, can_assign: bool) {
        named_variable(parser, parser.previous, can_assign)
    }

    fn literal(parser: &mut Parser<'_>, _: bool) {
        match parser.previous.kind {
            T![false] => parser.emit_opcode(OpCode::OP_FALSE),
            T![true] => parser.emit_opcode(OpCode::OP_TRUE),
            T![nil] => parser.emit_opcode(OpCode::OP_NIL),
            _ => {}
        }
    }

    fn number(parser: &mut Parser<'_>, _: bool) {
        let value: f64 = parser.previous.lexeme.parse().unwrap();
        parser.emit_constant(Value::Number(value));
    }

    fn grouping(parser: &mut Parser<'_>, _: bool) {
        parser.expression();
        parser.consume(T![')'], "Expect ')' after expression.")
    }

    fn unary(parser: &mut Parser<'_>, _: bool) {
        let operator = parser.previous.kind;

        // Compile the operand.
        parser.parse_precedence(Precedence::UNARY);

        // Emit the operator instruction.
        match operator {
            T![-] => parser.emit_opcode(OpCode::OP_NEGATE),
            T![!] => parser.emit_opcode(OpCode::OP_NOT),
            _ => {}
        }
    }

    fn binary(parser: &mut Parser<'_>, _: bool) {
        let operator = parser.previous.kind;
        let rule = parser.get_rule(operator);
        parser.parse_precedence(rule.precedence + 1);

        match operator {
            T![!=] => parser.emit_bytes(OpCode::OP_EQUAL as u8, OpCode::OP_NOT as u8),
            T![==] => parser.emit_opcode(OpCode::OP_EQUAL),
            T![>] => parser.emit_opcode(OpCode::OP_GREATER),
            T![>=] => parser.emit_bytes(OpCode::OP_LESS as u8, OpCode::OP_NOT as u8),
            T![<] => parser.emit_opcode(OpCode::OP_LESS),
            T![<=] => parser.emit_bytes(OpCode::OP_GREATER as u8, OpCode::OP_NOT as u8),
            T![+] => parser.emit_opcode(OpCode::OP_ADD),
            T![-] => parser.emit_opcode(OpCode::OP_SUBSTRACT),
            T![*] => parser.emit_opcode(OpCode::OP_MULTIPLY),
            T![/] => parser.emit_opcode(OpCode::OP_DIVIDE),
            _ => {}
        }
    }
}
