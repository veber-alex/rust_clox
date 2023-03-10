use std::ptr;

use crate::{
    chunk::{Chunk, OpCode},
    compiler::compile,
    object::{as_objstring_str, get_kind, take_string, ObjKind},
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
                OP_CONSTANT => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                OP_RETURN => {
                    println!("{:?}", self.pop());
                    return InterpretResult::Ok;
                }
                OP_NEGATE => {
                    let Value::Number(number) = self.pop() else {
                        self.runtime_error("Operand must be a number.");
                        return InterpretResult::RuntimeError
                    };
                    self.push(Value::Number(-number));
                }
                OP_ADD => match (self.pop(), self.pop()) {
                    (Value::Number(b), Value::Number(a)) => self.push(Value::Number(a + b)),
                    (Value::Obj(b), Value::Obj(a))
                        if get_kind(a) == ObjKind::OBJ_STRING
                            && get_kind(b) == ObjKind::OBJ_STRING =>
                    {
                        // Safety: obj is a valid ObjString due to kind check
                        let b = unsafe { as_objstring_str(b) }.as_bytes();
                        // Safety: obj is a valid ObjString due to kind check
                        let a = unsafe { as_objstring_str(a) }.as_bytes();
                        let len = a.len() + b.len();
                        let ptr: *mut u8 = Box::into_raw(Box::<[u8]>::new_uninit_slice(len)).cast();

                        // Safety: We have enough capacity due to allocation above
                        unsafe {
                            ptr::copy_nonoverlapping(a.as_ptr(), ptr, a.len());
                            ptr::copy_nonoverlapping(b.as_ptr(), ptr.add(a.len()), b.len())
                        }

                        self.push(Value::Obj(take_string(ptr, len).cast()))
                    }
                    _ => {
                        self.runtime_error("Operands must be two numbers or two strings.");
                        return InterpretResult::RuntimeError;
                    }
                },
                OP_SUBSTRACT => binary_op!(self, Value::Number, -),
                OP_MULTIPLY => binary_op!(self, Value::Number, *),
                OP_DIVIDE => binary_op!(self, Value::Number, /),
                OP_NIL => self.push(Value::Nil),
                OP_TRUE => self.push(Value::Boolean(true)),
                OP_FALSE => self.push(Value::Boolean(false)),
                OP_NOT => {
                    let b: bool = self.pop().into();
                    self.push(Value::Boolean(!b))
                }
                OP_EQUAL => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Boolean(a == b))
                }
                OP_GREATER => binary_op!(self, Value::Boolean, >),
                OP_LESS => binary_op!(self, Value::Boolean, <),
            }
        }
    }

    fn peek(&self, distance: isize) -> Value {
        // Safety: a valid Value exists at that distance by compiler bytecode construction
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
