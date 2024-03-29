#![feature(variant_count)]
#![feature(char_indices_offset)]
#![feature(let_chains)]
#![feature(slice_from_ptr_range)]

use std::process::exit;

use vm::{InterpretResult, VM};

mod chunk;
mod compiler;
mod memory;
mod object;
mod scanner;
mod table;
mod value;
mod vm;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mut vm = VM::new();
    vm.init_vm();

    match args.as_slice() {
        [_] => repl(&mut vm),
        [_, path] => run_file(path, &mut vm),
        _ => {
            eprintln!("Usage: rust_clox [path]");
            exit(64)
        }
    }
}

fn run_file(path: &str, vm: &mut VM) {
    let Ok(file) = std::fs::read_to_string(path) else {
        eprintln!("Could not open file {path}");
        exit(74)
    };

    match vm.interpret(&file) {
        InterpretResult::CompileError => exit(65),
        InterpretResult::RuntimeError => exit(70),
        InterpretResult::Ok => {}
    }

    vm.free_vm();
}

#[cfg(not(miri))]
fn repl(vm: &mut VM) {
    use rustyline::{history::MemHistory, Config, Editor};

    let config = Config::builder().auto_add_history(true).build();
    let history = MemHistory::new();
    let mut rl: Editor<(), MemHistory> = Editor::with_history(config, history).unwrap();

    while let Ok(line) = rl.readline("> ") {
        if !line.is_empty() {
            let _ = vm.interpret(&line);
        }
    }

    vm.free_vm();
}

#[cfg(miri)]
fn repl(vm: &mut VM) {
    use std::io::{stdin, stdout, BufRead, Write};

    let mut line = String::new();
    let mut stdin = stdin().lock();
    let mut stdout = stdout().lock();

    loop {
        print!("> ");
        let _ = stdout.flush();

        if let Ok(0) | Err(_) = stdin.read_line(&mut line) {
            break;
        }

        let trim_line = line.trim();
        if !trim_line.is_empty() {
            let _ = vm.interpret(trim_line);
        }
        line.clear();
    }

    vm.free_vm();
}
