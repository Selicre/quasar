mod parser;
mod context;
mod message;
mod executor;

use executor::Target;
use context::LineInfo;

fn main() {
    let mut target = Target::new();
    executor::exec_file("test.asm".into(), LineInfo::cli(), &mut target);
    for i in target.iter_messages() {
        println!("{}", i);
    }
}
