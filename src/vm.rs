use std::ptr;

use crate::{
    chunk::{Chunk, OpCode},
    compiler::compile,
    memory::{allocate_memory, free_objects},
    object::{Obj, ObjKind, ObjPtr, ObjString},
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
    pub objects: ObjPtr,
}

impl VM {
    pub fn new() -> Self {
        let mut vm = Self {
            chunk: Chunk::new(),
            ip: ptr::null(),
            stack: Vec::with_capacity(STACK_MAX),
            stack_top: ptr::null_mut(),
            objects: ObjPtr::null(),
        };
        vm.reset_stack();

        vm
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        // FIXME: can we use the chunk in self instead ?
        let mut chunk = Chunk::new();

        if !compile(self, source, &mut chunk) {
            return InterpretResult::CompileError;
        };

        self.set_chunk(chunk);

        // Safety: set_chunk() is called before run()
        unsafe { self.run() }
    }

    pub fn free_vm(&mut self) {
        free_objects(self)
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
                OP_ADD => match (self.peek(0), self.peek(1)) {
                    (Value::Number(b), Value::Number(a)) => {
                        self.pop();
                        self.pop();
                        self.push(Value::Number(a + b))
                    }
                    (Value::Obj(b), Value::Obj(a)) if a.is_string() && b.is_string() => {
                        // Safety: a and b are ObjString due to kind check
                        unsafe { self.concatenate(a, b) };
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

    // Safety: a and b must be ObjString
    unsafe fn concatenate(&mut self, a: ObjPtr, b: ObjPtr) {
        self.pop();
        self.pop();
        // Safety: obj is a valid ObjString due to kind check
        let b = unsafe { b.as_string_str() };
        // Safety: obj is a valid ObjString due to kind check
        let a = unsafe { a.as_string_str() };
        let len = a.len() + b.len();
        let ptr: *mut u8 = allocate_memory(len);

        // Safety: We have enough capacity due to allocation above
        unsafe {
            ptr::copy_nonoverlapping(a.as_ptr(), ptr, a.len());
            ptr::copy_nonoverlapping(b.as_ptr(), ptr.add(a.len()), b.len())
        }
        let obj = self.take_string(ptr, len);
        self.push(Value::Obj(obj))
    }

    pub fn copy_string(&mut self, lexeme: &str) -> ObjPtr {
        let len = lexeme.len();
        let ptr = allocate_memory::<u8>(len);
        // Safety: dst is valid and non overlapping with src
        unsafe { ptr::copy_nonoverlapping(lexeme.as_ptr(), ptr, len) }

        self.take_string(ptr, lexeme.len())
    }

    fn take_string(&mut self, ptr: *mut u8, len: usize) -> ObjPtr {
        let ptr = ptr::slice_from_raw_parts_mut(ptr, len) as *mut str;
        ObjPtr::new(self.allocate_string(ptr).cast())
    }

    fn allocate_string(&mut self, ptr: *mut str) -> *mut ObjString {
        // Safety: OBJ_STRING allocates an ObjString
        let string = unsafe { self.allocate_object(ObjKind::OBJ_STRING).as_string() };

        // Safety: string is a valid ObjString
        unsafe { (*string).ptr = ptr };

        string
    }

    // TODO: Should this be generic?
    fn allocate_object(&mut self, kind: ObjKind) -> ObjPtr {
        let obj: *mut Obj = match kind {
            ObjKind::OBJ_STRING => allocate_memory::<ObjString>(1).cast(),
        };

        // Safety: types are compatible due to layout
        unsafe {
            obj.write(Obj {
                kind,
                next: self.objects,
            })
        };

        let obj = ObjPtr::new(obj);
        self.objects = obj;

        obj
    }
}

#[must_use]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}
