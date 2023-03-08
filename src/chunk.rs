use crate::value::Value;

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    OpConstant,
    OpReturn,
}

impl OpCode {
    // Safety: byte must be a valid value from the OpCode enum
    unsafe fn from_u8_unchecked(byte: u8) -> Self {
        debug_assert!(byte as usize <= std::mem::variant_count::<OpCode>());
        // Safety: According to the contract of the function only valid OpCode bytes are allowed
        unsafe { std::mem::transmute(byte) }
    }
}

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn write_constant(&mut self, value: Value, line: usize) {
        let index = self.add_constant(value);
        self.write_chunk(OpCode::OpConstant as u8, line);
        self.write_chunk(index as u8, line)
    }

    pub fn write_code(&mut self, code: OpCode, line: usize) {
        self.write_chunk(code as u8, line)
    }

    fn write_chunk(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    fn free_chunk(&mut self) {
        *self = Chunk::new()
    }

    fn add_constant(&mut self, value: Value) -> usize {
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

    fn disassemble_instruction(&self, offset: usize) -> usize {
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
            OpReturn => self.simple_instruction("OP_RETURN", offset),
            OpConstant => self.constant_instruction("OP_CONSTANT", offset),
        }
    }

    fn simple_instruction(&self, name: &str, offset: usize) -> usize {
        println!("{name}");
        offset + 1
    }

    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let constant_index = self.code[offset + 1];
        print!("{name:16} {constant_index:4} ");
        println!("'{:?}'", self.constants[constant_index as usize]);
        offset + 2
    }
}
