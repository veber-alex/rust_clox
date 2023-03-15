use std::{hint::unreachable_unchecked, ptr};

use crate::{
    chunk::{Chunk, OpCode},
    compiler::{compile, U8_COUNT},
    memory::{allocate_memory, free_array_memory, free_objects},
    object::{hash_string, Obj, ObjFunction, ObjKind, ObjPtr, ObjString},
    table::{free_table, table_delete, table_find_string, table_get, table_set, Table},
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

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * U8_COUNT;

#[derive(Clone, Copy)]
struct CallFrame {
    function: *mut ObjFunction,

    // Instruction pointer into the functions Chunk
    ip: *mut u8,

    // Pointer to the VM stack, where the function stack frame starts
    slots: *mut Value,
}

impl CallFrame {
    fn new(function: *mut ObjFunction, slots: *mut Value) -> Self {
        Self {
            function,
            ip: unsafe { (*function).chunk.as_code_ptr() },
            slots,
        }
    }
}

pub struct VM {
    frames: *mut CallFrame,
    frame_count: usize,
    stack: *mut Value,
    stack_top: *mut Value,
    strings: Table,
    globals: Table,
    pub objects: ObjPtr,
}

impl VM {
    pub fn new() -> Self {
        Self {
            frames: ptr::null_mut(),
            frame_count: 0,
            stack: ptr::null_mut(),
            stack_top: ptr::null_mut(),
            strings: Table::new(),
            globals: Table::new(),
            objects: ObjPtr::null(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let function = compile(self, source);

        if function.is_null() {
            return InterpretResult::CompileError;
        }

        self.push(Value::Obj(ObjPtr::new(function.cast())));
        unsafe {
            let frame = CallFrame::new(function, self.stack);
            *self.frames.add(self.frame_count) = frame;
            self.frame_count += 1;
        }
        self.run()
    }

    pub fn free_vm(&mut self) {
        free_objects(self);
        free_table(&mut self.strings);
        free_table(&mut self.globals);
        free_array_memory(self.stack, STACK_MAX);
        free_array_memory(self.frames, FRAMES_MAX);
    }

    pub fn reset_stack(&mut self) {
        free_array_memory(self.stack, STACK_MAX);
        self.stack = allocate_memory(STACK_MAX);
        self.stack_top = self.stack;

        free_array_memory(self.frames, FRAMES_MAX);
        self.frame_count = 0;
        self.frames = allocate_memory(FRAMES_MAX);
    }

    // TODO: optimize the shit out of this loop + match
    fn run(&mut self) -> InterpretResult {
        use OpCode::*;

        let frame = unsafe { self.frames.add(self.frame_count - 1) };

        fn read_byte(frame: *mut CallFrame) -> u8 {
            unsafe {
                let byte = *(*frame).ip;
                (*frame).ip = (*frame).ip.add(1);
                byte
            }
        }

        fn read_short(frame: *mut CallFrame) -> u16 {
            unsafe {
                let short = u16::from_le_bytes([*(*frame).ip.add(1), *(*frame).ip]);
                (*frame).ip = (*frame).ip.add(2);
                short
            }
        }

        fn read_constant(frame: *mut CallFrame) -> Value {
            unsafe {
                (*(*frame).function)
                    .chunk
                    .constants
                    .get(read_byte(frame) as usize)
            }
        }

        fn read_string(frame: *mut CallFrame) -> *mut ObjString {
            unsafe {
                let Value::Obj(obj) = read_constant(frame) else {
                unreachable_unchecked()
            };
                obj.as_string()
            }
        }

        loop {
            #[cfg(feature = "debug_prints")]
            {
                print!("          ");
                let mut slot = self.stack;
                while slot < self.stack_top {
                    // Safety: slot is in valid range
                    unsafe { print!("[ {:?} ]", *slot) };
                    slot = slot.wrapping_add(1);
                }
                println!();
                unsafe {
                    (*(*frame).function).chunk.disassemble_instruction(
                        (*frame).ip as usize - (*(*frame).function).chunk.as_code_ptr() as usize,
                    );
                }
            }

            let byte = read_byte(frame);

            // Safety: byte is a valid opcode by compiler bytecode construction
            let instruction = unsafe { OpCode::from_u8_unchecked(byte) };

            match instruction {
                OP_CONSTANT => {
                    let constant = read_constant(frame);
                    self.push(constant);
                }
                OP_RETURN => {
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
                OP_PRINT => println!("{:?}", self.pop()),
                OP_POP => {
                    self.pop();
                }
                OP_DEFINE_GLOBAL => {
                    let name = read_string(frame);
                    table_set(&mut self.globals, name, self.peek(0));
                    self.pop();
                }
                OP_GET_GLOBAL => {
                    let name = read_string(frame);
                    let mut value = Value::Nil;
                    if !table_get(&mut self.globals, name, &mut value) {
                        let string = ObjPtr::new(name.cast()).as_string_str();
                        self.runtime_error(&format!("Undefined variable '{string}'."));
                        return InterpretResult::RuntimeError;
                    }
                    self.push(value);
                }
                OP_SET_GLOBAL => {
                    let name = read_string(frame);
                    if table_set(&mut self.globals, name, self.peek(0)) {
                        table_delete(&mut self.globals, name);
                        let string = ObjPtr::new(name.cast()).as_string_str();
                        self.runtime_error(&format!("Undefined variable '{string}'."));
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_GET_LOCAL => {
                    let slot = read_byte(frame) as usize;
                    let local = unsafe { *(*frame).slots.add(slot) };
                    self.push(local);
                }
                OP_SET_LOCAL => {
                    let slot = read_byte(frame) as usize;
                    unsafe { *(*frame).slots.add(slot) = self.peek(0) };
                }
                OP_JUMP_IF_FALSE => {
                    let offset = read_short(frame) as usize;
                    // Check if doing this branchless will be faster
                    // (!bool::from(self.peek(0))) as usize * offset as usize
                    if !bool::from(self.peek(0)) {
                        unsafe { (*frame).ip = (*frame).ip.add(offset) };
                    }
                }
                OP_JUMP => {
                    let offset = read_short(frame) as usize;
                    unsafe { (*frame).ip = (*frame).ip.add(offset) };
                }
                OP_LOOP => {
                    let offset = read_short(frame) as usize;
                    unsafe { (*frame).ip = (*frame).ip.sub(offset) };
                }
            }
        }
    }

    fn peek(&self, distance: isize) -> Value {
        // Safety: a valid Value exists at that distance by compiler bytecode construction
        unsafe { *self.stack_top.offset(-1 - distance) }
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

    #[cold]
    fn runtime_error(&mut self, msg: &str) {
        eprintln!("{msg}");

        unsafe {
            let frame = self.frames.add(self.frame_count - 1);
            let instruction =
                (*frame).ip as usize - (*(*frame).function).chunk.as_code_ptr() as usize - 1;
            let line = (*(*frame).function).chunk.read_line(instruction);
            eprintln!("[line {line}] in script");
        }

        self.reset_stack();
    }

    // Safety: a and b must be ObjString
    unsafe fn concatenate(&mut self, a: ObjPtr, b: ObjPtr) {
        self.pop();
        self.pop();
        // Safety: obj is a valid ObjString due to kind check
        let b = b.as_string_str();
        // Safety: obj is a valid ObjString due to kind check
        let a = a.as_string_str();
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
        let ptr = lexeme.as_ptr();
        let hash = hash_string(ptr, len);

        // Try to find the string in the intern table
        let interned = table_find_string(&self.strings, ptr, len, hash);
        if !interned.is_null() {
            return ObjPtr::new(interned.cast());
        }

        let new_ptr = allocate_memory::<u8>(len);
        // Safety: dst is valid and non overlapping with src
        unsafe { ptr::copy_nonoverlapping(ptr, new_ptr, len) }

        ObjPtr::new(self.allocate_string(new_ptr, len, hash).cast())
    }

    fn take_string(&mut self, ptr: *mut u8, len: usize) -> ObjPtr {
        let hash = hash_string(ptr, len);
        let interned = table_find_string(&self.strings, ptr, len, hash);
        if !interned.is_null() {
            free_array_memory(ptr, len);
            return ObjPtr::new(interned.cast());
        }

        ObjPtr::new(self.allocate_string(ptr, len, hash).cast())
    }

    fn allocate_string(&mut self, ptr: *mut u8, len: usize, hash: u32) -> *mut ObjString {
        // Safety: OBJ_STRING allocates an ObjString
        let string = self.allocate_object::<ObjString>(ObjKind::OBJ_STRING);

        // Safety: string is a valid ObjString
        unsafe {
            (*string).ptr = ptr;
            (*string).len = len;
            (*string).hash = hash
        };

        table_set(&mut self.strings, string, Value::Nil);

        string
    }

    fn allocate_object<T>(&mut self, kind: ObjKind) -> *mut T {
        let object = allocate_memory::<T>(1);
        let obj: *mut Obj = object.cast();

        // Safety: types are compatible due to layout
        unsafe {
            obj.write(Obj {
                kind,
                next: self.objects,
            })
        };

        let obj = ObjPtr::new(obj);
        self.objects = obj;

        object
    }

    pub fn new_function(&mut self) -> *mut ObjFunction {
        let function = self.allocate_object::<ObjFunction>(ObjKind::OBJ_FUNCTION);

        unsafe {
            (*function).arity = 0;
            (*function).name = ptr::null_mut();
            (*function).chunk = Chunk::new();
        }

        function
    }
}

#[must_use]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}
