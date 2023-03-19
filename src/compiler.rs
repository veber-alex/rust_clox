use std::{cell::Cell, ptr};

use crate::{
    chunk::{Chunk, OpCode},
    object::{ObjFunction, ObjPtr},
    scanner::{Scanner, Token, TokenKind},
    value::Value,
    vm::VM,
};

use OpCode::*;
use Precedence::*;

pub fn compile(vm: &mut VM, source: &str) -> *mut ObjFunction {
    let scanner = Scanner::new(source);
    let mut compiler = Compiler::new();
    let mut parser = Parser::new(vm, scanner, &mut compiler);

    parser.advance();

    while !parser.matches(T![EOF]) {
        parser.declaration();
    }

    let function = parser.end_compiler();

    if parser.had_error.get() {
        ptr::null_mut()
    } else {
        function
    }
}

// FIXME: try to make this a real
#[allow(non_snake_case)]
mod Precedence {
    pub const PREC_NONE: u8 = 0;
    pub const PREC_ASSIGNMENT: u8 = 1; // =
    pub const PREC_OR: u8 = 2; // or
    pub const PREC_AND: u8 = 3; // and
    pub const PREC_EQUALITY: u8 = 4; // == !=
    pub const PREC_COMPARISON: u8 = 5; // < > <= >=
    pub const PREC_TERM: u8 = 6; // + -
    pub const PREC_FACTOR: u8 = 7; // * /
    pub const PREC_UNARY: u8 = 8; // ! -
    pub const PREC_CALL: u8 = 9; // . ()
    pub const PREC_PRIMARY: u8 = 10;
}

type ParseFn = fn(&mut Parser<'_>, bool);
pub const U8_COUNT: usize = u8::MAX as usize + 1;

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
    is_captured: bool,
}

#[derive(Clone, Copy)]
struct UpValue {
    index: u8,
    is_local: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
enum FunctionKind {
    TYPE_FUNCTION,
    TYPE_SCRIPT,
}

struct Compiler<'a> {
    enclosing: *mut Compiler<'a>,
    function: *mut ObjFunction,
    kind: FunctionKind,
    locals: [Local<'a>; U8_COUNT],
    local_count: usize,
    upvalues: [UpValue; U8_COUNT],
    scope_depth: i32,
}

impl<'a> Compiler<'a> {
    fn new() -> Self {
        Compiler {
            enclosing: ptr::null_mut(),
            function: ptr::null_mut(),
            kind: FunctionKind::TYPE_SCRIPT,
            locals: [Local {
                name: Token::dummy(),
                depth: 0,
                is_captured: false,
            }; U8_COUNT],
            local_count: 0,
            upvalues: [UpValue {
                index: 0,
                is_local: false,
            }; U8_COUNT],
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
    vm: &'a mut VM,
    compiler: *mut Compiler<'a>,
}

impl<'a> Parser<'a> {
    fn new(vm: &'a mut VM, scanner: Scanner<'a>, compiler: *mut Compiler<'a>) -> Self {
        let mut parser = Self {
            scanner,
            current: Token::dummy(),
            previous: Token::dummy(),
            had_error: Cell::new(false),
            panic_mode: Cell::new(false),
            vm,
            compiler: ptr::null_mut(),
        };
        parser.init_compiler(compiler, FunctionKind::TYPE_SCRIPT);
        parser
    }

    fn init_compiler(&mut self, compiler: *mut Compiler<'a>, kind: FunctionKind) {
        unsafe {
            (*compiler).enclosing = self.compiler;
            (*compiler).function = self.vm.new_function();
            (*compiler).kind = kind;
            self.compiler = compiler;
            if kind != FunctionKind::TYPE_SCRIPT {
                (*(*self.compiler).function).name =
                    self.vm.copy_string(self.previous.lexeme).as_string();
            }

            let local = &mut (*compiler).locals[0];
            (*compiler).local_count += 1;
            local.name.lexeme = "";
            local.depth = 0;
            local.is_captured = false;
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
        self.current_chunk().push(byte, line);
    }

    // TODO: Replace this with emit_opcode_2
    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_jump(&mut self, op_code: OpCode) -> usize {
        self.emit_byte(op_code as u8);
        self.emit_byte(0xff);
        self.emit_byte(0xff);

        self.current_chunk().len() - 2
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_opcode(OP_LOOP);

        let offset = self.current_chunk().len() - loop_start + 2;
        if offset > u16::MAX as usize {
            self.error("Loop body too large.");
        }

        self.emit_byte(((offset >> 8) & 0xff) as u8);
        self.emit_byte((offset & 0xff) as u8);
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.current_chunk().len() - offset - 2;

        if jump > u16::MAX as usize {
            self.error("Too much code to jump over.");
        }

        self.current_chunk()
            .write_byte(offset, ((jump >> 8) & 0xff) as u8);
        self.current_chunk()
            .write_byte(offset + 1, (jump & 0xff) as u8);
    }

    fn emit_constant(&mut self, value: Value) {
        let index = self.make_constant(value);
        self.emit_bytes(OP_CONSTANT as u8, index);
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

    fn end_compiler(&mut self) -> *mut ObjFunction {
        self.emit_return();
        let function = unsafe { (*self.compiler).function };

        #[cfg(feature = "debug_prints")]
        {
            if !self.had_error.get() {
                let name = unsafe {
                    if (*function).name.is_null() {
                        "script"
                    } else {
                        ObjPtr::new((*function).name.cast()).as_string_str()
                    }
                };
                self.current_chunk().disassemble_chunk(name);
            }
        }

        self.compiler = unsafe { (*self.compiler).enclosing };
        function
    }

    fn emit_return(&mut self) {
        self.emit_opcode(OP_NIL);
        self.emit_opcode(OP_RETURN)
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        unsafe { &mut (*(*self.compiler).function).chunk }
    }

    fn expression(&mut self) {
        self.parse_precedence(PREC_ASSIGNMENT)
    }

    fn block(&mut self) {
        while !self.check(T!['}']) && !self.check(T![EOF]) {
            self.declaration()
        }

        self.consume(T!['}'], "Expect '}' after block.")
    }

    fn function(&mut self, kind: FunctionKind) {
        let mut compiler = Compiler::new();
        self.init_compiler(&mut compiler, kind);
        self.begin_scope();

        self.consume(T!['('], "Expect '(' after function name.");
        if !self.check(T![')']) {
            loop {
                unsafe {
                    if (*(*self.compiler).function).arity == 255 {
                        self.error_at_current("Can't have more than 255 parameters.");
                    }
                    (*(*self.compiler).function).arity += 1;
                    let constant = self.parse_variable("Expect parameter name.");
                    self.define_variable(constant);
                    if !self.matches(T![,]) {
                        break;
                    }
                }
            }
        }
        self.consume(T![')'], "Expect ')' after parameters.");
        self.consume(T!['{'], "Expect '{' before function body.");
        self.block();

        let function = self.end_compiler();
        let constant = self.make_constant(Value::Obj(ObjPtr::new(function.cast())));
        self.emit_bytes(OP_CLOSURE as u8, constant);

        unsafe {
            for upvalue in &compiler.upvalues[..(*function).upvalue_count as usize] {
                self.emit_byte(upvalue.is_local as u8);
                self.emit_byte(upvalue.index);
            }
        }
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect function name.");
        self.mark_initialized();
        self.function(FunctionKind::TYPE_FUNCTION);
        self.define_variable(global);
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.matches(T![=]) {
            self.expression();
        } else {
            self.emit_opcode(OP_NIL);
        }

        self.consume(T![;], "Expect ';' after variable declaration.");

        self.define_variable(global);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(T![;], "Expect ';' after expression.");
        self.emit_opcode(OP_POP);
    }

    fn for_statement(&mut self) {
        self.begin_scope();

        self.consume(T!['('], "Expect '(' after 'for'.");
        if self.matches(T![;]) {
            // No initializer.
        } else if self.matches(T![var]) {
            self.var_declaration();
        } else {
            self.expression_statement()
        }

        let mut loop_start = self.current_chunk().len();
        let mut exit_jump = usize::MAX;
        if !self.matches(T![;]) {
            self.expression();
            self.consume(T![;], "Expect ';' after loop condition.");

            // Jump out of the loop if the condition is false.
            exit_jump = self.emit_jump(OP_JUMP_IF_FALSE);
            self.emit_opcode(OP_POP); // Condition.
        }

        if !self.matches(T![')']) {
            let body_jump = self.emit_jump(OP_JUMP);
            let increment_start = self.current_chunk().len();
            self.expression();
            self.emit_opcode(OP_POP);
            self.consume(T![')'], "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop(loop_start);

        if exit_jump != usize::MAX {
            self.patch_jump(exit_jump);
            self.emit_opcode(OP_POP); // Condition.
        }

        self.end_scope()
    }

    fn if_statement(&mut self) {
        self.consume(T!['('], "Expect '(' after 'if'.");
        self.expression();
        self.consume(T![')'], "Expect ')' after condition.");

        let then_jump = self.emit_jump(OP_JUMP_IF_FALSE);
        self.emit_opcode(OP_POP);
        self.statement();
        let else_jump = self.emit_jump(OP_JUMP);

        self.patch_jump(then_jump);
        self.emit_opcode(OP_POP);

        if self.matches(T![else]) {
            self.statement()
        }
        self.patch_jump(else_jump);
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(T![;], "Expect ';' after value.");
        self.emit_opcode(OP_PRINT)
    }

    fn return_statement(&mut self) {
        unsafe {
            if (*self.compiler).kind == FunctionKind::TYPE_SCRIPT {
                self.error("Can't return from top-level code.")
            }
        }

        if self.matches(T![;]) {
            self.emit_return()
        } else {
            self.expression();
            self.consume(T![;], "Expect ';' after return value.");
            self.emit_opcode(OP_RETURN)
        }
    }

    fn while_statement(&mut self) {
        let loop_start = self.current_chunk().len();

        self.consume(T!['('], "Expect '(' after 'while'.");
        self.expression();
        self.consume(T![')'], "Expect ')' after condition.");

        let exit_jump = self.emit_jump(OP_JUMP_IF_FALSE);
        self.emit_opcode(OP_POP);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_opcode(OP_POP);
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
        if self.matches(T![fun]) {
            self.fun_declaration()
        } else if self.matches(T![var]) {
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
        } else if self.matches(T![for]) {
            self.for_statement();
        } else if self.matches(T![if]) {
            self.if_statement();
        } else if self.matches(T![return]) {
            self.return_statement()
        } else if self.matches(T![while]) {
            self.while_statement();
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

        let can_assign = precedence <= PREC_ASSIGNMENT;
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
        unsafe {
            (*self.compiler).scope_depth += 1;
        }
    }

    fn end_scope(&mut self) {
        unsafe {
            (*self.compiler).scope_depth -= 1;

            while (*self.compiler).local_count > 0
                && (*self.compiler).locals[(*self.compiler).local_count - 1].depth
                    > (*self.compiler).scope_depth
            {
                // FIXME: Add a special POP_POPN to pop multiple values from the stack at once
                if (*self.compiler).locals[(*self.compiler).local_count - 1].is_captured {
                    self.emit_opcode(OP_CLOSE_UPVALUE);
                } else {
                    self.emit_opcode(OP_POP);
                }
                (*self.compiler).local_count -= 1;
            }
        }
    }

    fn add_local(&mut self, name: Token<'a>) {
        unsafe {
            let Some(local) = (*self.compiler).locals.get_mut((*self.compiler).local_count) else {
                self.error("Too many local variables in function.");
                return
            };

            *local = Local {
                name,
                depth: -1,
                is_captured: false,
            };

            (*self.compiler).local_count += 1;
        }
    }

    fn declare_variable(&mut self) {
        unsafe {
            if (*self.compiler).scope_depth == 0 {
                return;
            }

            let name = self.previous;
            for local in (*self.compiler).locals[..(*self.compiler).local_count]
                .iter()
                .rev()
            {
                if local.depth != -1 && local.depth < (*self.compiler).scope_depth {
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
    }

    fn parse_variable(&mut self, msg: &str) -> u8 {
        self.consume(T![ident], msg);

        self.declare_variable();
        unsafe {
            if (*self.compiler).scope_depth > 0 {
                return 0;
            }
        }

        self.identifier_constant(self.previous)
    }

    fn mark_initialized(&mut self) {
        unsafe {
            if (*self.compiler).scope_depth == 0 {
                return;
            }
            (*self.compiler).locals[(*self.compiler).local_count - 1].depth =
                (*self.compiler).scope_depth;
        }
    }

    fn define_variable(&mut self, global: u8) {
        unsafe {
            if (*self.compiler).scope_depth > 0 {
                self.mark_initialized();
                return;
            }
        }

        self.emit_bytes(OP_DEFINE_GLOBAL as u8, global)
    }

    fn argument_list(&mut self) -> u8 {
        let mut arg_count = 0;
        if !self.check(T![')']) {
            loop {
                self.expression();
                if arg_count == 255 {
                    self.error("Can't have more than 255 arguments.")
                }
                arg_count += 1;
                if !self.matches(T![,]) {
                    break;
                }
            }
        }
        self.consume(T![')'], "Expect ')' after arguments.");
        arg_count
    }

    fn named_variable(&mut self, name: Token<'_>, can_assign: bool) {
        let mut arg = self.resolve_local(self.compiler, name);
        let (get_op, set_op) = if arg != -1 {
            (OP_GET_LOCAL, OP_SET_LOCAL)
        } else {
            let upvalue = self.resolve_upvalue(self.compiler, name);
            if upvalue != -1 {
                arg = upvalue;
                (OP_GET_UPVALUE, OP_SET_UPVALUE)
            } else {
                arg = self.identifier_constant(name) as i32;
                (OP_GET_GLOBAL, OP_SET_GLOBAL)
            }
        };

        if can_assign && self.matches(T![=]) {
            self.expression();
            self.emit_bytes(set_op as u8, arg as u8);
        } else {
            self.emit_bytes(get_op as u8, arg as u8);
        }
    }

    // FIXME: This should return Option<u8> ?
    fn resolve_local(&mut self, compiler: *mut Compiler<'_>, name: Token<'_>) -> i32 {
        unsafe {
            match (*compiler).locals[..(*compiler).local_count]
                .iter()
                .zip(0..(*compiler).local_count as i32)
                .rev()
                .find(|(local, _)| name.lexeme == local.name.lexeme)
            {
                Some((Local { depth: -1, .. }, i)) => {
                    self.error("Can't read local variable in its own initializer.");
                    i
                }
                Some((_, i)) => i,
                None => -1,
            }
        }
    }

    fn add_upvalue(&mut self, compiler: *mut Compiler<'_>, index: u8, is_local: bool) -> i32 {
        unsafe {
            let upvalue_count = (*(*compiler).function).upvalue_count as usize;

            if let Some((index, _)) = (*compiler).upvalues[..upvalue_count]
                .iter()
                .enumerate()
                .find(|(_, upvalue)| upvalue.index == index && upvalue.is_local == is_local)
            {
                return index as i32;
            }

            if upvalue_count == U8_COUNT {
                self.error("Too many closure variables in function.");
                return 0;
            }

            (*compiler).upvalues[upvalue_count].is_local = is_local;
            (*compiler).upvalues[upvalue_count].index = index;
            (*(*compiler).function).upvalue_count += 1;

            upvalue_count as i32
        }
    }

    // FIXME: This should return Option<u8> ?
    fn resolve_upvalue(&mut self, compiler: *mut Compiler<'_>, name: Token<'_>) -> i32 {
        unsafe {
            if (*compiler).enclosing.is_null() {
                return -1;
            }

            let local = self.resolve_local((*compiler).enclosing, name);
            if local != -1 {
                (*(*compiler).enclosing).locals[local as usize].is_captured = true;
                return self.add_upvalue(compiler, local as u8, true);
            }

            let upvalue = self.resolve_upvalue((*compiler).enclosing, name);
            if upvalue != -1 {
                return self.add_upvalue(compiler, upvalue as u8, false);
            }

            -1
        }
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

    use OpCode::*;
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
            $rules[$kind as usize] = ParseRule { prefix: Some($prefix), infix: Some($infix), precedence: $precedence };
        };
    }

    macro_rules! rules {
        ($([$kind:expr, $prefix:tt, $infix:tt, $precedence:tt])*) => {{
            let mut rules = [ParseRule {prefix: None, infix: None, precedence: PREC_NONE}; mem::variant_count::<TokenKind>()];
            $(
                rules_inner!(rules; $kind => [$prefix, $infix, $precedence]);
            )*

            rules
        }};
    }

    // FIXME: Replace this dumb thing with a simple match (check asm)
    pub static RULES: [ParseRule; mem::variant_count::<TokenKind>()] = rules! {
        [T![-],       unary,      binary,   PREC_TERM]
        [T!['('],     grouping,   call,     PREC_CALL]
        [T![number],  number,     None,     PREC_NONE]
        [T![false],   literal,    None,     PREC_NONE]
        [T![true],    literal,    None,     PREC_NONE]
        [T![nil],     literal,    None,     PREC_NONE]
        [T![str],     string,     None,     PREC_NONE]
        [T![!],       unary,      None,     PREC_NONE]
        [T![ident],   variable,   None,     PREC_NONE]
        [T![+],       None,       binary,   PREC_TERM]
        [T![/],       None,       binary,   PREC_FACTOR]
        [T![*],       None,       binary,   PREC_FACTOR]
        [T![!=],      None,       binary,   PREC_EQUALITY]
        [T![==],      None,       binary,   PREC_EQUALITY]
        [T![>],       None,       binary,   PREC_COMPARISON]
        [T![>=],      None,       binary,   PREC_COMPARISON]
        [T![<],       None,       binary,   PREC_COMPARISON]
        [T![<=],      None,       binary,   PREC_COMPARISON]
        [T![and],     None,       and,      PREC_AND]
        [T![or],      None,       or,       PREC_OR]
    };

    fn and(parser: &mut Parser<'_>, _: bool) {
        let end_jump = parser.emit_jump(OP_JUMP_IF_FALSE);

        parser.emit_opcode(OP_POP);
        parser.parse_precedence(PREC_AND);

        parser.patch_jump(end_jump);
    }

    fn or(parser: &mut Parser<'_>, _: bool) {
        let else_jump = parser.emit_jump(OP_JUMP_IF_FALSE);
        let end_jump = parser.emit_jump(OP_JUMP);

        parser.patch_jump(else_jump);
        parser.emit_opcode(OP_POP);

        parser.parse_precedence(PREC_OR);
        parser.patch_jump(end_jump);
    }

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
            T![false] => parser.emit_opcode(OP_FALSE),
            T![true] => parser.emit_opcode(OP_TRUE),
            T![nil] => parser.emit_opcode(OP_NIL),
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
        parser.parse_precedence(PREC_UNARY);

        // Emit the operator instruction.
        match operator {
            T![-] => parser.emit_opcode(OP_NEGATE),
            T![!] => parser.emit_opcode(OP_NOT),
            _ => {}
        }
    }

    fn binary(parser: &mut Parser<'_>, _: bool) {
        let operator = parser.previous.kind;
        let rule = parser.get_rule(operator);
        parser.parse_precedence(rule.precedence + 1);

        match operator {
            T![!=] => parser.emit_bytes(OP_EQUAL as u8, OP_NOT as u8),
            T![==] => parser.emit_opcode(OP_EQUAL),
            T![>] => parser.emit_opcode(OP_GREATER),
            T![>=] => parser.emit_bytes(OP_LESS as u8, OP_NOT as u8),
            T![<] => parser.emit_opcode(OP_LESS),
            T![<=] => parser.emit_bytes(OP_GREATER as u8, OP_NOT as u8),
            T![+] => parser.emit_opcode(OP_ADD),
            T![-] => parser.emit_opcode(OP_SUBSTRACT),
            T![*] => parser.emit_opcode(OP_MULTIPLY),
            T![/] => parser.emit_opcode(OP_DIVIDE),
            _ => {}
        }
    }

    fn call(parser: &mut Parser<'_>, _: bool) {
        let arg_count = parser.argument_list();
        parser.emit_bytes(OP_CALL as u8, arg_count);
    }
}
