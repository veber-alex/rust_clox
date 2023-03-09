use std::ptr;

use crate::{
    chunk::{Chunk, OpCode},
    compiler::compile,
    value::Value,
};

// FIXME: Replace with trait impls on Value
macro_rules! binary_op {
    ($vm:expr, $value_type:expr, $op:tt) => {{
        let (Value::Number(b), Value::Number(a)) = ($vm.pop(), $vm.pop()) else {
            $vm.runtime_error("Operands must be numbers.");
            return InterpretResult::RuntimeError
        };
        $vm.push($value_type(a $op b))
    }};
}

const STACK_MAX: usize = 256;

pub struct VM {
    chunk: Chunk,
    ip: *const u8,
    stack: Vec<Value>,
    stack_top: *mut Value,
}

impl VM {
    pub fn new() -> Self {
        let mut vm = Self {
            chunk: Chunk::new(),
            ip: ptr::null(),
            stack: Vec::with_capacity(STACK_MAX),
            stack_top: ptr::null_mut(),
        };
        vm.reset_stack();

        vm
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        // FIXME: can we use the chunk in self instead ?
        let mut chunk = Chunk::new();

        if !compile(source, &mut chunk) {
            return InterpretResult::CompileError;
        };

        self.set_chunk(chunk);

        // Safety: set_chunk() is called before run()
        unsafe { self.run() }
    }

    fn set_chunk(&mut self, chunk: Chunk) {
        self.ip = chunk.code.as_ptr();
        self.chunk = chunk;
    }

    fn reset_stack(&mut self) {
        self.stack_top = self.stack.as_mut_ptr();
    }

    // Safety: set_chunk() must be called before run()
    // FIXME: combine set_chunk() and run() ?
    // TODO: optimize the shit out of this loop + match
    unsafe fn run(&mut self) -> InterpretResult {
        use OpCode::*;

        loop {
            #[cfg(debug_assertions)]
            {
                print!("          ");
                let mut slot = self.stack.as_ptr();
                while slot < self.stack_top {
                    // Safety: slot is in valid range
                    unsafe { print!("[ {:?} ]", *slot) };
                    slot = slot.wrapping_add(1);
                }
                println!();
                self.chunk
                    .disassemble_instruction(self.ip as usize - self.chunk.code.as_ptr() as usize);
            }

            let byte = self.read_byte();

            // Safety: byte is a valid opcode by compiler bytecode construction
            let instruction = unsafe { OpCode::from_u8_unchecked(byte) };

            match instruction {
                OpConstant => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                OpReturn => {
                    println!("{:?}", self.pop());
                    return InterpretResult::Ok;
                }
                OpNegate => {
                    let Value::Number(number) = self.pop() else {
                        self.runtime_error("Operand must be a number.");
                        return InterpretResult::RuntimeError
                    };
                    self.push(Value::Number(-number));
                }
                OpAdd => binary_op!(self, Value::Number, +),
                OpSubtract => binary_op!(self, Value::Number, -),
                OpMultiply => binary_op!(self, Value::Number, *),
                OpDivide => binary_op!(self, Value::Number, /),
                OpNil => self.push(Value::Nil),
                OpTrue => self.push(Value::Boolean(true)),
                OpFalse => self.push(Value::Boolean(false)),
                OpNot => {
                    let b: bool = self.pop().into();
                    self.push(Value::Boolean(!b))
                }
            }
        }
    }

    // Safety: a valid Value must exist at that distance
    unsafe fn peek(&self, distance: isize) -> Value {
        // Safety: a valid Value must exist at that distance
        unsafe { *self.stack_top.offset(-1 - distance) }
    }

    fn read_byte(&mut self) -> u8 {
        // Safety: The instruction pointer is in bounds by compiler bytecode construction
        let byte = unsafe { *self.ip };

        // Safety: The new pointer is in bounds or 1 byte over
        self.ip = unsafe { self.ip.add(1) };

        byte
    }

    // FIXME: inline this method to the match loop for safety
    // calling it in any other place is unsafe because the next bytecode may not be a constant
    fn read_constant(&mut self) -> Value {
        let byte = self.read_byte();

        // Safety: constant exists at index by compiler bytecode construction
        unsafe { *self.chunk.constants.get_unchecked(byte as usize) }
    }

    fn push(&mut self, value: Value) {
        // FIXME: This is extremly unsafe
        unsafe { *self.stack_top = value };
        unsafe { self.stack_top = self.stack_top.add(1) };
    }

    fn pop(&mut self) -> Value {
        // FIXME: This is extremly unsafe
        // or is it? can we pop before push with valid bytecode ?
        unsafe { self.stack_top = self.stack_top.sub(1) };
        unsafe { *self.stack_top }
    }

    fn runtime_error(&mut self, msg: &str) {
        eprintln!("{msg}");

        let instruction = self.ip as usize - self.chunk.code.as_ptr() as usize - 1;
        let line = self.chunk.lines[instruction];
        eprintln!("[line {line}] in script");

        self.reset_stack();
    }
}

#[must_use]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}
