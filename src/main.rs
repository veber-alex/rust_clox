#![feature(variant_count)]

use chunk::{Chunk, OpCode};

mod chunk;
mod value;

fn main() {
    let mut chunk = Chunk::new();

    chunk.write_constant(1.2, 123);
    chunk.write_code(OpCode::OpReturn, 123);

    chunk.disassemble_chunk("test chunk");
}
