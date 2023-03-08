#![feature(variant_count)]
#![feature(char_indices_offset)]

use std::{
    io::{stdin, stdout, BufRead, Write},
    process::exit,
};

use compiler::compile;
use vm::InterpretResult;

#[macro_use]
mod scanner;

mod chunk;
mod compiler;
mod value;
mod vm;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    match args.as_slice() {
        [_] => repl(),
        [_, path] => run_file(path),
        _ => {
            eprintln!("Usage: rust_clox [path]");
            exit(64)
        }
    }
}

fn run_file(path: &str) {
    let Ok(file) = std::fs::read_to_string(path) else {
        eprintln!("Could not open file {path}");
        exit(74)
    };

    match interpret(&file) {
        InterpretResult::CompileError => exit(65),
        InterpretResult::RuntimeError => exit(70),
        InterpretResult::Ok => {}
    }
}

fn repl() {
    let mut line = String::new();
    let mut stdin = stdin().lock();
    let mut stdout = stdout().lock();

    loop {
        print!("> ");
        let _ = stdout.flush();

        if let Ok(0) | Err(_) = stdin.read_line(&mut line) {
            break;
        }

        let _ = interpret(&line);
        line.clear();
    }
}

fn interpret(source: &str) -> InterpretResult {
    compile(source);
    InterpretResult::Ok
}
