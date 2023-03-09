#![feature(variant_count)]
#![feature(char_indices_offset)]
#![feature(const_mut_refs)]

use std::process::exit;

use vm::{InterpretResult, VM};

#[macro_use]
mod scanner;

mod chunk;
mod compiler;
mod value;
mod vm;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let vm = VM::new();

    match args.as_slice() {
        [_] => repl(vm),
        [_, path] => run_file(path, vm),
        _ => {
            eprintln!("Usage: rust_clox [path]");
            exit(64)
        }
    }
}

fn run_file(path: &str, mut vm: VM) {
    let Ok(file) = std::fs::read_to_string(path) else {
        eprintln!("Could not open file {path}");
        exit(74)
    };

    match vm.interpret(&file) {
        InterpretResult::CompileError => exit(65),
        InterpretResult::RuntimeError => exit(70),
        InterpretResult::Ok => {}
    }
}

fn repl(mut vm: VM) {
    use rustyline::{history::MemHistory, Config, Editor};

    let config = Config::builder().auto_add_history(true).build();
    let history = MemHistory::new();
    let mut rl: Editor<(), MemHistory> = Editor::with_history(config, history).unwrap();

    while let Ok(line) = rl.readline("> ") {
        let _ = vm.interpret(&line);
    }
}
