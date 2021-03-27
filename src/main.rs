mod splitter;
mod context;
mod message;
mod executor;
mod lexer;
mod expr;

use executor::Target;
use context::ContextStr;

fn main() {
    let mut target = Target::new();
    executor::exec_file("test.asm".into(), ContextStr::cli(), &mut target);
    for i in target.iter_messages() {
        println!("{}", i);
    }
    if target.has_error() {
        println!("Assembly failed");
    }
}
