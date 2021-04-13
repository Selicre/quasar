#![deny(unused_must_use)]

//mod splitter;
mod context;
mod message;
mod executor;
mod lexer;
mod expression;
mod assembler;
mod instruction;
mod rom;
mod freespace;

use executor::Target;
use assembler::Assembler;
use context::ContextStr;
use message::MsgQueue;
use rom::Rom;

fn main() {
    let asm_file = std::env::args().nth(1).expect("args pls");
    let rom_file = std::env::args().nth(2).expect("args pls");
    let mut rom_cache = std::fs::read(&rom_file).unwrap_or(vec![]);
    if rom_file.ends_with(".smc") && rom_cache.len() > 0x200 {
        rom_cache.drain(0..0x200);
    }
    let mut rom = Rom::new(rom_cache);


    //let mut compare = std::fs::read("compare.bin").unwrap();
    let mut target = Target::new(rom.clone());
    let mut asm = Assembler::new();
    //asm.set_compare(compare);

    executor::exec_file(&asm_file, ContextStr::cli(), &mut target, &mut asm);
    MsgQueue::drain(|i| {
        println!("{}", i);
    });
    if MsgQueue::has_error() {
        println!("Parsing failed");
        return;
    }
    target.profiler("executed");

    /*for (k,v) in target.labels().clone().iter().enumerate() {
        if let Some(expr) = asm.get_label_value(k).cloned() {
            let expr = expr.try_eval_float(&mut target, &mut asm).unwrap();
            println!("{:?} => {}", v, expr);
        }
    }*/

    freespace::resolve_freespace(&mut rom, &mut asm);
    asm.resolve_labels();
    asm.write_to_rom(&mut target, &mut rom);
    MsgQueue::drain(|i| {
        println!("{}", i);
    });
    if MsgQueue::has_error() {
        println!("Assembly failed");
        return;
    }
    std::fs::write("out.sfc", rom.as_slice()).unwrap();
    target.profiler("finished");
    // this actually takes a surprisingly long time
    std::mem::forget(asm);
}
