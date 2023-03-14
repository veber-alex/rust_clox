use std::cell::Cell;

use crate::{
    chunk::{Chunk, OpCode},
    scanner::{Scanner, Token, TokenKind},
    value::Value,
    vm::VM,
};

pub fn compile(vm: &mut VM, source: &str, chunk: &mut Chunk) -> bool {
    let scanner = Scanner::new(source);
    let mut compiler = Compiler::new();
    let mut parser = Parser::new(vm, scanner, chunk, &mut compiler);

    parser.advance();

    while !parser.matches(T![EOF]) {
        parser.declaration();
    }

    parser.end_compiler();

    !parser.had_error.get()
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
const U8_COUNT: usize = u8::MAX as usize + 1;

#[derive(Clone, Copy)]
pub struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: u8,
}

#[derive(Clone, Copy)]
struct Local<'a> {
    name: Token<'a>,
    depth: i32,
}

struct Compiler<'a> {
    locals: [Local<'a>; U8_COUNT],
    local_count: usize,
    scope_depth: i32,
}

impl<'a> Compiler<'a> {
    fn new() -> Self {
        Compiler {
            locals: [Local {
                name: Token::dummy(),
                depth: 0,
            }; U8_COUNT],
            local_count: 0,
            scope_depth: 0,
        }
    }
}

pub struct Parser<'a> {
    scanner: Scanner<'a>,
    current: Token<'a>,
    previous: Token<'a>,
    had_error: Cell<bool>,
    panic_mode: Cell<bool>,
    chunk: &'a mut Chunk,
    vm: &'a mut VM,
    compiler: &'a mut Compiler<'a>,
}

impl<'a> Parser<'a> {
    fn new(
        vm: &'a mut VM,
        scanner: Scanner<'a>,
        chunk: &'a mut Chunk,
        compiler: &'a mut Compiler<'a>,
    ) -> Self {
        Self {
            scanner,
            current: Token::dummy(),
            previous: Token::dummy(),
            had_error: Cell::new(false),
            panic_mode: Cell::new(false),
            chunk,
            vm,
            compiler,
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
            if !self.had_error.get() {
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

    fn block(&mut self) {
        while !self.check(T!['}']) && !self.check(T![EOF]) {
            self.declaration()
        }

        self.consume(T!['}'], "Expect '}' after block.")
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
        self.panic_mode.set(false);

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

        if self.panic_mode.get() {
            self.synchronize();
        }
    }

    fn statement(&mut self) {
        if self.matches(T![print]) {
            self.print_statement();
        } else if self.matches(T!['{']) {
            self.begin_scope();
            self.block();
            self.end_scope();
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

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;

        while self.compiler.local_count > 0
            && self.compiler.locals[self.compiler.local_count - 1].depth > self.compiler.scope_depth
        {
            // FIXME: Add a special OpCode::POP_POPN to pop multiple values from the stack at once
            self.emit_opcode(OpCode::OP_POP);
            self.compiler.local_count -= 1;
        }
    }

    fn add_local(&mut self, name: Token<'a>) {
        let Some(local) = self.compiler.locals.get_mut(self.compiler.local_count) else {
            self.error("Too many local variables in function.");
            return
        };

        *local = Local { name, depth: -1 };

        self.compiler.local_count += 1;
    }

    fn declare_variable(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }

        let name = self.previous;
        for local in self.compiler.locals[..self.compiler.local_count]
            .iter()
            .rev()
        {
            if local.depth != -1 && local.depth < self.compiler.scope_depth {
                break;
            }

            if name.lexeme == local.name.lexeme {
                self.error("Already a variable with this name in this scope.");
                // FIXME: This break should not be here, it's a workaround borrow checker errors
                break;
            }
        }

        self.add_local(name);
    }

    fn parse_variable(&mut self, msg: &str) -> u8 {
        self.consume(T![ident], msg);

        self.declare_variable();
        if self.compiler.scope_depth > 0 {
            return 0;
        }

        self.identifier_constant(self.previous)
    }

    fn mark_initialized(&mut self) {
        self.compiler.locals[self.compiler.local_count - 1].depth = self.compiler.scope_depth;
    }

    fn define_variable(&mut self, global: u8) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        self.emit_bytes(OpCode::OP_DEFINE_GLOBAL as u8, global)
    }

    fn named_variable(&mut self, token: Token<'_>, can_assign: bool) {
        let mut arg = resolve_local(&self.compiler, self, token);
        let (get_op, set_op) = if arg != -1 {
            (OpCode::OP_GET_LOCAL, OpCode::OP_SET_LOCAL)
        } else {
            arg = self.identifier_constant(token) as i32;
            (OpCode::OP_GET_GLOBAL, OpCode::OP_SET_GLOBAL)
        };

        if can_assign && self.matches(T![=]) {
            self.expression();
            self.emit_bytes(set_op as u8, arg as u8);
        } else {
            self.emit_bytes(get_op as u8, arg as u8);
        }
    }
}

// FIXME: This should return Option<u8>
fn resolve_local(compiler: &Compiler<'_>, parser: &Parser<'_>, name: Token<'_>) -> i32 {
    match compiler.locals[..compiler.local_count]
        .iter()
        .zip(0..compiler.local_count as i32)
        .rev()
        .find(|(local, _)| name.lexeme == local.name.lexeme)
    {
        Some((Local { depth: -1, .. }, i)) => {
            parser.error("Can't read local variable in its own initializer.");
            i
        }
        Some((_, i)) => i,
        None => -1,
    }
}

// error handling
impl<'a> Parser<'a> {
    fn error_at_current(&self, msg: &str) {
        self.error_at(self.current, msg)
    }

    fn error(&self, msg: &str) {
        self.error_at(self.previous, msg)
    }

    fn error_at(&self, token: Token<'a>, msg: &str) {
        if self.panic_mode.get() {
            return;
        }
        self.panic_mode.set(true);
        eprint!("[line {}] Error", token.line);

        match token.kind {
            T![EOF] => eprint!(" at the end"),
            T![ERR] => {}
            _ => eprint!(" at {}", token.lexeme),
        }

        eprintln!(": {msg}");
        self.had_error.set(true);
    }
}

mod jump_table {
    use crate::{
        chunk::OpCode,
        compiler::{ParseRule, Parser, Precedence},
        scanner::TokenKind,
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

    fn variable(parser: &mut Parser<'_>, can_assign: bool) {
        parser.named_variable(parser.previous, can_assign)
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
