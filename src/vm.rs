use crate::{
    chunk::{Chunk, OpCode},
    value::Value,
};

// FIXME: Replace with trait impls on Value
macro_rules! binary_op {
    ($vm:expr, $op:tt) => {{
        let b = $vm.pop();
        let a = $vm.pop();
        $vm.push(a $op b)
    }};
}

const STACK_MAX: usize = 256;

pub struct VM<'a> {
    chunk: &'a Chunk,
    ip: *const u8,
    stack: Vec<Value>,
    stack_top: *mut Value,
}

impl<'a> VM<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        let mut stack = Vec::with_capacity(STACK_MAX);
        let stack_top = stack.as_mut_ptr();

        Self {
            chunk,
            ip: chunk.code.as_ptr(),
            stack,
            stack_top,
        }
    }

    pub fn interpret(source: &str) -> InterpretResult {
        todo!()
    }

    fn run(&mut self) -> InterpretResult {
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
                    println!("{}", self.pop());
                    return InterpretResult::Ok;
                }
                OpNegate => {
                    let val = self.pop();
                    self.push(-val);
                }
                OpAdd => binary_op!(self, +),
                OpSubtract => binary_op!(self, -),
                OpMultiply => binary_op!(self, *),
                OpDivide => binary_op!(self, /),
            }
        }
    }

    fn read_byte(&mut self) -> u8 {
        // Safety: The instruction pointer is in bounds by compiler bytecode construction
        let byte = unsafe { *self.ip };

        // Safety: The new pointer is in bounds or 1 byte over
        self.ip = unsafe { self.ip.add(1) };

        byte
    }

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
}

#[must_use]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}
