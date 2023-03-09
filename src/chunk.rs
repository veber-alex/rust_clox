use crate::value::Value;

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum OpCode {
    OP_CONSTANT,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBSTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
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
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn write_chunk(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
}

// for debugging
impl Chunk {
    pub fn disassemble_chunk(&self, name: &str) {
        println!("== {name} ==");

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset)
        }
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        use OpCode::*;

        print!("{offset:04} ");
        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ")
        } else {
            print!("{:4} ", self.lines[offset])
        }

        let byte = self.code[offset];
        // Safety: byte is a valid opcode by compiler construction
        let instruction = unsafe { OpCode::from_u8_unchecked(byte) };
        match instruction {
            OP_CONSTANT => self.constant_instruction(instruction, offset),
            OP_RETURN | OP_NEGATE | OP_ADD | OP_SUBSTRACT | OP_MULTIPLY | OP_DIVIDE | OP_NIL
            | OP_TRUE | OP_FALSE | OP_NOT | OP_EQUAL | OP_GREATER | OP_LESS => {
                self.simple_instruction(instruction, offset)
            }
        }
    }

    fn simple_instruction(&self, op_code: OpCode, offset: usize) -> usize {
        println!("{op_code:?}");
        offset + 1
    }

    fn constant_instruction(&self, op_code: OpCode, offset: usize) -> usize {
        let constant_index = self.code[offset + 1];
        print!("{op_code:16?} {constant_index:4} ");
        println!("'{:?}'", self.constants[constant_index as usize]);
        offset + 2
    }
}
