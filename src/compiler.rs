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

    return !parser.had_error;
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    NONE,
    ASSIGNMENT, // =
    OR,         // or
    AND,        // and
    EQUALITY,   // == !=
    COMPARISON, // < > <= >=
    TERM,       // + -
    FACTOR,     // * /
    UNARY,      // ! -
    CALL,       // . ()
    PRIMARY,
}

struct Parser<'a> {
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
        self.emit_return()
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

    fn parse_precedence(&mut self, precedence: Precedence) {}
}

// expression handlers
impl<'a> Parser<'a> {
    fn number(&mut self) {
        let value: Value = self.previous.lexeme.parse().unwrap();
        self.emit_constant(value);
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(T![')'], "Expect ')' after expression.")
    }

    fn unary(&mut self) {
        let operator = self.previous.kind;

        // Compile the operand.
        self.parse_precedence(Precedence::UNARY);

        // Emit the operator instruction.
        match operator {
            T![-] => self.emit_byte(OpCode::OpNegate as u8),
            _ => {}
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
