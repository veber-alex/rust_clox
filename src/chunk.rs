use std::ptr;

use crate::{
    memory::{free_array_memory, grow_capacity, reallocate_memory, Vector},
    value::Value,
};

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum OpCode {
    OP_CONSTANT,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_DEFINE_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_GLOBAL,
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBSTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_LOOP,
    OP_CALL,
    OP_CLOSURE,
    OP_CLOSE_UPVALUE,
    OP_RETURN,
}

impl OpCode {
    // Safety: byte must be a valid value from the OpCode enum
    pub unsafe fn from_u8_unchecked(byte: u8) -> Self {
        debug_assert!(byte as usize <= std::mem::variant_count::<OpCode>());
        // Safety: According to the contract of the function only valid OpCode bytes are allowed
        unsafe { std::mem::transmute(byte) }
    }
}

pub struct Chunk {
    code: *mut u8,
    lines: *mut u32,
    len: usize,
    capacity: usize,
    pub constants: Vector<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: ptr::null_mut(),
            lines: ptr::null_mut(),
            constants: Vector::new(),
            capacity: 0,
            len: 0,
        }
    }

    pub fn push(&mut self, byte: u8, line: u32) {
        if self.len == self.capacity {
            let old_capacity = self.capacity;
            self.capacity = grow_capacity(old_capacity);
            self.code = reallocate_memory(self.code, old_capacity, self.capacity);
            self.lines = reallocate_memory(self.lines, old_capacity, self.capacity);
        }

        unsafe {
            *self.code.add(self.len) = byte;
            *self.lines.add(self.len) = line;
        };
        self.len += 1;
    }

    pub fn write_byte(&self, offset: usize, byte: u8) {
        unsafe { *self.code.add(offset) = byte }
    }

    pub fn as_code_ptr(&self) -> *mut u8 {
        self.code
    }

    pub fn read_line(&self, offset: usize) -> u32 {
        unsafe { *self.lines.add(offset) }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn free_chunk(&mut self) {
        self.constants.free();
        free_array_memory(self.code, self.capacity);
        free_array_memory(self.lines, self.capacity);
        *self = Chunk::new();
    }
}

// for debugging
impl Chunk {
    pub fn disassemble_chunk(&self, name: &str) {
        println!("== {name} ==");

        let mut offset = 0;
        while offset < self.len {
            offset = self.disassemble_instruction(offset)
        }
    }

    pub fn disassemble_instruction(&self, mut offset: usize) -> usize {
        use OpCode::*;

        print!("{offset:04} ");
        if offset > 0 && unsafe { *self.lines.add(offset) == *self.lines.add(offset - 1) } {
            print!("   | ")
        } else {
            print!("{:4} ", unsafe { *self.lines.add(offset) })
        }

        let byte = unsafe { *self.code.add(offset) };
        // Safety: byte is a valid opcode by compiler construction
        let instruction = unsafe { OpCode::from_u8_unchecked(byte) };
        match instruction {
            OP_CONSTANT | OP_DEFINE_GLOBAL | OP_GET_GLOBAL | OP_SET_GLOBAL => {
                self.constant_instruction(instruction, offset)
            }
            OP_RETURN | OP_NEGATE | OP_ADD | OP_SUBSTRACT | OP_MULTIPLY | OP_DIVIDE | OP_NIL
            | OP_TRUE | OP_FALSE | OP_NOT | OP_EQUAL | OP_GREATER | OP_LESS | OP_PRINT | OP_POP
            | OP_CLOSE_UPVALUE => self.simple_instruction(instruction, offset),
            OP_GET_LOCAL | OP_SET_LOCAL | OP_CALL | OP_GET_UPVALUE | OP_SET_UPVALUE => {
                self.byte_instruction(instruction, offset)
            }
            OP_JUMP_IF_FALSE | OP_JUMP => self.jump_instruction(instruction, offset, 1),
            OP_LOOP => self.jump_instruction(instruction, offset, -1),
            OP_CLOSURE => {
                offset += 1;
                let constant_index = unsafe { *self.code.add(offset) };
                offset += 1;
                print!("{instruction:16?} {constant_index:4} ");
                println!("'{}'", self.constants.get(constant_index as usize));
                let Value::Obj(obj) = self.constants.get(constant_index as usize) else { unreachable!() };
                let function = obj.as_function();
                unsafe {
                    for _ in 0..(*function).upvalue_count {
                        let is_local = if *self.code.add(offset) == 1 {
                            "local"
                        } else {
                            "upvalue"
                        };
                        offset += 1;
                        let index = *self.code.add(offset);
                        offset += 1;
                        println!(
                            "{:04}      |                     {} {}",
                            offset - 2,
                            is_local,
                            index
                        )
                    }
                }
                offset
            }
        }
    }

    fn jump_instruction(&self, op_code: OpCode, offset: usize, sign: isize) -> usize {
        let jump = unsafe {
            u16::from_le_bytes([*self.code.add(offset + 2), *self.code.add(offset + 1)]) as isize
        };
        println!(
            "{op_code:16?} {offset:4} -> {}",
            offset as isize + 3 + sign * jump
        );
        offset + 3
    }

    fn simple_instruction(&self, op_code: OpCode, offset: usize) -> usize {
        println!("{op_code:?}");
        offset + 1
    }

    fn constant_instruction(&self, op_code: OpCode, offset: usize) -> usize {
        let constant_index = unsafe { *self.code.add(offset + 1) };
        print!("{op_code:16?} {constant_index:4} ");
        println!("'{}'", self.constants.get(constant_index as usize));
        offset + 2
    }

    fn byte_instruction(&self, op_code: OpCode, offset: usize) -> usize {
        let slot = unsafe { *self.code.add(offset + 1) };
        println!("{op_code:16?} {slot:4} ");
        offset + 2
    }
}
