#![feature(variant_count)]

use chunk::{Chunk, OpCode};
use vm::{free_vm, init_vm, VM};

mod chunk;
mod value;
mod vm;

fn main() {
    init_vm();

    let mut chunk = Chunk::new();

    chunk.write_constant(1.2, 123);
    chunk.write_constant(3.4, 123);
    chunk.write_code(OpCode::OpAdd, 123);
    chunk.write_constant(2.0, 123);
    chunk.write_code(OpCode::OpDivide, 123);
    chunk.write_code(OpCode::OpNegate, 123);
    chunk.write_code(OpCode::OpReturn, 123);

    chunk.disassemble_chunk("test chunk");

    let mut vm = VM::new(&chunk);
    vm.interpret();

    free_vm();
}
