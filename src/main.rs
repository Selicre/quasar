mod splitter;
mod context;
mod message;
mod executor;
mod lexer;
mod expr;
mod statement;
mod assembler;
mod instruction;

use executor::Target;
use assembler::Assembler;
use context::ContextStr;

fn main() {
    let arg = std::env::args().nth(1).expect("args pls");
    //let mut compare = std::fs::read("compare.bin").unwrap();
    let mut target = Target::new();
    let mut asm = Assembler::new();
    //asm.set_compare(compare);
    executor::exec_file(arg.into(), ContextStr::cli(), &mut target, &mut asm);
    for i in target.iter_messages() {
        println!("{}", i);
    }
    if target.has_error() {
        println!("Parsing failed");
        return;
    }
    target.profiler("executed");
    target.clear_messages();
    let mut out = std::io::BufWriter::new(std::fs::File::create("out.bin").unwrap());
    asm.write_to_file(&mut target, &mut out);
    for i in target.iter_messages() {
        println!("{}", i);
    }
    if target.has_error() {
        println!("Assembly failed");
        return;
    }
    target.profiler("finished");
}
