mod splitter;
mod context;
mod message;
mod executor;
mod lexer;
mod expr;
mod statement;
mod assembler;

use executor::Target;
use assembler::Assembler;
use context::ContextStr;

fn main() {
    let mut target = Target::new();
    let mut asm = Assembler::new();
    executor::exec_file("test.asm".into(), ContextStr::cli(), &mut target, &mut asm);
    for i in target.iter_messages() {
        println!("{}", i);
    }
    if target.has_error() {
        println!("Parsing failed");
        return;
    }
    target.clear_messages();
    let mut out = std::fs::File::create("out.bin").unwrap();
    asm.write_to_file(&mut target, &mut out);
    for i in target.iter_messages() {
        println!("{}", i);
    }
    if target.has_error() {
        println!("Assembly failed");
        return;
    }
}
