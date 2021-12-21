
use quasar::{
    executor::{self, Target},
    assembler::Assembler,
    context::ContextStr,
    message::MsgQueue,
    rom::Rom,
    optimizer,
    freespace
};

use std::io::Write;

fn main() {
    let asm_file = std::env::args().nth(1).expect("args pls");
    let rom_file = std::env::args().nth(2).expect("args pls");
    let out_file = std::env::args().nth(3).unwrap_or("out.sfc".into());
    println!("{}", out_file);
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


    freespace::resolve_freespace(&mut rom, &mut asm);
    asm.resolve_labels();
    //optimizer::label::same_bank(&mut target, &mut asm);

    for (k,v) in target.labels().clone().iter().enumerate() {
        if let Some(expr) = asm.get_label_value(k).cloned() {
            use std::fmt::Write;
            if let Some(expr) = expr.try_eval_int(&mut target, &mut asm) {
                println!("%{} = ${:06X} ; {:?}", k, expr, v);
            } else {
                println!("%{} = error ; {:?}", k, v);
            }
        }
    }

    asm.write_to_rom(&mut target, &mut rom);
    MsgQueue::drain(|i| {
        println!("{}", i);
    });
    if MsgQueue::has_error() {
        println!("Assembly failed");
        return;
    }
    rom.fix_checksum();
    let mut output = std::fs::File::create(&out_file).unwrap();
    if out_file.ends_with(".smc") {
        output.write_all(&[0; 0x200]).unwrap();
    }
    output.write_all(rom.as_slice()).unwrap();
    target.profiler("finished");

    let mut labels = String::new();
    for (k,v) in target.labels().clone().iter().enumerate() {
        if let quasar::expression::Label::Named { stack, invoke } = v {
            if stack.len() == 1 && invoke.is_none() {
                if let Some(expr) = asm.get_label_value(k).cloned() {
                    use std::fmt::Write;
                    let expr = expr.try_eval_int(&mut target, &mut asm).unwrap();
                    writeln!(labels, "{} = ${:X}", &stack[0], expr);
                }
            }
        }
    }
    std::fs::write("labels.asm", labels).unwrap();

    // this actually takes a surprisingly long time, and we can just let the OS do the cleanup on
    // its own
    std::mem::forget(target);
    std::mem::forget(asm);
}
