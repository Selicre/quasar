mod splitter;
mod context;
mod message;
mod executor;
mod lexer;
mod expression;
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

    for (k,v) in target.labels().clone().iter().enumerate() {
        if let Some(expr) = asm.get_label_value(k).cloned() {
            let expr = expr.try_eval_float(&mut target, &mut asm).unwrap();
            println!("{:?} => {}", v, expr);
        }
    }

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
