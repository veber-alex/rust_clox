use std::{hint::unreachable_unchecked, mem::MaybeUninit, ptr, slice, time::Instant};

use crate::{
    chunk::{Chunk, OpCode},
    compiler::{compile, U8_COUNT},
    memory::{allocate_memory, free_array_memory, free_objects},
    object::{
        hash_string, NativeFn, Obj, ObjClosure, ObjFunction, ObjKind, ObjNative, ObjPtr, ObjString,
        ObjUpvalue,
    },
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

static mut START_INSTANT: MaybeUninit<Instant> = MaybeUninit::uninit();

#[derive(Clone, Copy)]
struct CallFrame {
    closure: *mut ObjClosure,

    // Instruction pointer into the functions Chunk
    ip: *mut u8,

    // Pointer to the VM stack, where the function stack frame starts
    slots: *mut Value,
}

pub struct VM {
    frames: *mut CallFrame,
    frame_count: usize,
    stack: *mut Value,
    stack_top: *mut Value,
    strings: Table,
    globals: Table,
    pub objects: ObjPtr,
    open_upvalues: *mut ObjUpvalue,
}

impl VM {
    pub fn new() -> Self {
        // FIXME: This should not be here
        unsafe { START_INSTANT = MaybeUninit::new(Instant::now()) }

        Self {
            frames: ptr::null_mut(),
            frame_count: 0,
            stack: ptr::null_mut(),
            stack_top: ptr::null_mut(),
            strings: Table::new(),
            globals: Table::new(),
            objects: ObjPtr::null(),
            open_upvalues: ptr::null_mut(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let function = compile(self, source);

        if function.is_null() {
            return InterpretResult::CompileError;
        }

        self.push(Value::Obj(ObjPtr::new(function.cast())));
        let closure = self.new_closure(function);
        self.pop();
        self.push(Value::Obj(ObjPtr::new(closure.cast())));
        self.call(closure, 0);

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

        self.open_upvalues = ptr::null_mut();

        // FIXME: This should not be here
        self.define_native("clock", clock_native);
    }

    // TODO: optimize the shit out of this loop + match
    fn run(&mut self) -> InterpretResult {
        use OpCode::*;

        let mut frame = unsafe { self.frames.add(self.frame_count - 1) };

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
                (*(*(*frame).closure).function)
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

        fn read_function(frame: *mut CallFrame) -> *mut ObjFunction {
            unsafe {
                let Value::Obj(obj) = read_constant(frame) else {
                unreachable_unchecked()
            };
                obj.as_function()
            }
        }

        loop {
            #[cfg(feature = "debug_prints")]
            {
                print!("          ");
                let slice = unsafe { slice::from_ptr_range(self.stack..self.stack_top) };
                for slot in slice {
                    print!("[ {} ]", slot)
                }
                println!();
                unsafe {
                    (*(*(*frame).closure).function)
                        .chunk
                        .disassemble_instruction(
                            (*frame).ip as usize
                                - (*(*(*frame).closure).function).chunk.as_code_ptr() as usize,
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
                    let result = self.pop();
                    unsafe { self.close_upvalues((*frame).slots) };
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop();
                        return InterpretResult::Ok;
                    }

                    unsafe {
                        self.stack_top = (*frame).slots;
                        self.push(result);
                        frame = self.frames.add(self.frame_count - 1);
                    }
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
                OP_PRINT => println!("{}", self.pop()),
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
                OP_CALL => {
                    let arg_count = read_byte(frame);
                    if !self.call_value(self.peek(arg_count as isize), arg_count) {
                        return InterpretResult::RuntimeError;
                    }
                    frame = unsafe { self.frames.add(self.frame_count - 1) };
                }
                OP_CLOSURE => {
                    let function = read_function(frame);
                    let closure = self.new_closure(function);
                    self.push(Value::Obj(ObjPtr::new(closure.cast())));
                    unsafe {
                        for i in 0..(*closure).upvalue_count as usize {
                            let is_local = read_byte(frame) == 1;
                            let index = read_byte(frame) as usize;
                            *(*closure).upvalues.add(i) = if is_local {
                                self.capture_upvalue((*frame).slots.add(index))
                            } else {
                                *(*(*frame).closure).upvalues.add(index)
                            };
                        }
                    }
                }
                OP_GET_UPVALUE => unsafe {
                    let slot = read_byte(frame) as usize;
                    self.push(*(*(*(*(*frame).closure).upvalues.add(slot))).location)
                },
                OP_SET_UPVALUE => unsafe {
                    let slot = read_byte(frame) as usize;
                    *(*(*(*(*frame).closure).upvalues.add(slot))).location =
                        self.peek(slot as isize);
                },
                OP_CLOSE_UPVALUE => unsafe {
                    self.close_upvalues(self.stack_top.sub(1));
                    self.pop();
                },
            }
        }
    }

    fn peek(&self, distance: isize) -> Value {
        unsafe { *self.stack_top.offset(-1 - distance) }
    }

    fn call(&mut self, closure: *mut ObjClosure, arg_count: u8) -> bool {
        unsafe {
            let arity = (*(*closure).function).arity;
            if arg_count != arity {
                self.runtime_error(&format!(
                    "Expected {} arguments but got {}.",
                    arity, arg_count,
                ));
                return false;
            }

            if self.frame_count == FRAMES_MAX {
                self.runtime_error("Stack overflow.");
                return false;
            }

            let frame = self.frames.add(self.frame_count);
            self.frame_count += 1;
            (*frame).closure = closure;
            (*frame).ip = (*(*closure).function).chunk.as_code_ptr();
            (*frame).slots = self.stack_top.sub(arg_count as usize + 1);
        }
        true
    }

    fn call_value(&mut self, value: Value, arg_count: u8) -> bool {
        if let Value::Obj(obj) = value {
            match obj.kind() {
                ObjKind::OBJ_CLOSURE => return self.call(obj.as_closure(), arg_count),
                ObjKind::OBJ_NATIVE => unsafe {
                    let native = obj.as_native_fn();
                    let result = native(arg_count as i32, self.stack_top.sub(arg_count as usize));
                    self.stack_top = self.stack_top.sub(arg_count as usize + 1);
                    self.push(result);
                    return true;
                },
                _ => {}
            }
        }
        self.runtime_error("Can only call functions and classes.");
        false
    }

    fn capture_upvalue(&mut self, local: *mut Value) -> *mut ObjUpvalue {
        unsafe {
            let mut prev_upvalue = ptr::null_mut();
            let mut upvalue = self.open_upvalues;
            while !upvalue.is_null() && (*upvalue).location > local {
                prev_upvalue = upvalue;
                upvalue = (*upvalue).next;
            }

            if !upvalue.is_null() && (*upvalue).location == local {
                return upvalue;
            }

            let created_upvalue = self.new_upvalue(local);
            (*created_upvalue).next = upvalue;

            if prev_upvalue.is_null() {
                self.open_upvalues = created_upvalue;
            } else {
                (*prev_upvalue).next = created_upvalue;
            }

            created_upvalue
        }
    }

    fn close_upvalues(&mut self, last: *mut Value) {
        unsafe {
            while !self.open_upvalues.is_null() && (*self.open_upvalues).location >= last {
                let upvalue = self.open_upvalues;
                (*upvalue).closed = *(*upvalue).location;
                (*upvalue).location = ptr::addr_of_mut!((*upvalue).closed);
                self.open_upvalues = (*upvalue).next;
            }
        }
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
            for i in (0..self.frame_count).rev() {
                let frame = self.frames.add(i);
                let function = (*(*frame).closure).function;
                let instruction =
                    (*frame).ip as usize - (*function).chunk.as_code_ptr() as usize - 1;
                let line = (*function).chunk.read_line(instruction);
                eprint!("[line {line}] in ");
                if (*function).name.is_null() {
                    eprintln!("script")
                } else {
                    eprintln!("{}", ObjPtr::new((*function).name.cast()).as_string_str())
                }
            }
        }

        self.reset_stack();
    }

    fn define_native(&mut self, name: &str, function: NativeFn) {
        let string = self.copy_string(name);
        self.push(Value::Obj(string));

        let native = self.new_native(function).cast();
        let value = Value::Obj(ObjPtr::new(native));
        self.push(value);

        table_set(&mut self.globals, string.as_string(), value);

        self.pop();
        self.pop();
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
            (*function).upvalue_count = 0;
            (*function).name = ptr::null_mut();
            (*function).chunk = Chunk::new();
        }

        function
    }

    pub fn new_native(&mut self, function: NativeFn) -> *mut ObjNative {
        let native = self.allocate_object::<ObjNative>(ObjKind::OBJ_NATIVE);

        unsafe { (*native).function = function }

        native
    }

    pub fn new_closure(&mut self, function: *mut ObjFunction) -> *mut ObjClosure {
        unsafe {
            let upvalue_count = (*function).upvalue_count as usize;
            let upvalues = allocate_memory::<*mut ObjUpvalue>(upvalue_count);
            for i in 0..upvalue_count {
                *upvalues.add(i) = ptr::null_mut();
            }

            let closure = self.allocate_object::<ObjClosure>(ObjKind::OBJ_CLOSURE);
            (*closure).function = function;
            (*closure).upvalues = upvalues;
            (*closure).upvalue_count = upvalue_count as i32;

            closure
        }
    }

    pub fn new_upvalue(&mut self, slot: *mut Value) -> *mut ObjUpvalue {
        let upvalue = self.allocate_object::<ObjUpvalue>(ObjKind::OBJ_UPVALUE);

        unsafe {
            (*upvalue).location = slot;
            (*upvalue).closed = Value::Nil;
            (*upvalue).next = ptr::null_mut()
        };

        upvalue
    }
}

#[must_use]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

fn clock_native(_: i32, _: *mut Value) -> Value {
    unsafe { Value::Number(START_INSTANT.assume_init().elapsed().as_secs_f64()) }
}
