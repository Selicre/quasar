use crate::assembler::{Assembler, BankOpt, StartKind, StatementKind};
use crate::executor::Target;
use crate::instruction;
use crate::message::Message;

pub fn same_bank(target: &mut Target, asm: &mut Assembler) {
    let mut new_segments = asm.segments().to_vec();
    for seg in new_segments.iter_mut() {
        let addr = match &seg.start {
            StartKind::Expression(e) => e.try_eval_int(target, asm).expect("label??") as usize,
            _ => panic!("internal error: freespace pointer not resolved yet")
        };
        let mut shift = 0;
        for i in seg.statements_mut() {
            i.offset -= shift;
            match &mut i.kind {
                StatementKind::Instruction { ref mut opcode, expr, opt_bank } => {
                    if let Some(op) = instruction::lower_to_16(*opcode) {
                        let pc = i.offset + addr;
                        let opt_for = match opt_bank {
                            BankOpt::None => continue,
                            BankOpt::Pc => (pc >> 16) as u32,
                            BankOpt::Value(c) => *c as u32
                        };
                        if let Some(e) = expr.try_eval_int(target, asm) {
                            if e >> 16 == opt_for {
                                println!("opt for {:06X} at {:06X}: {:02X} -> {:02X}", e, pc, *opcode, op);
                                *opcode = op;
                                i.size -= 1;
                                shift += 1;
                            }
                        }
                    }
                },
                _ => {}
            }
        }
    }
    *asm.segments_vec() = new_segments;
}
