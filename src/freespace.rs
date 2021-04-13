use crate::rom::Rom;
use crate::assembler::{StartKind, Assembler};
use crate::context::ContextStr;

use bytes::Buf;

pub struct RomSpace {
    free: Vec<[usize;2]>,
    rats: Vec<[usize;2]>,
}

pub fn parse_freespace(rom: &mut Rom) -> RomSpace {
    let mut bank = rom.freespace_area();
    let start = bank;
    let mut free = vec![];
    let mut rats = vec![];
    let mut current_area = None;

    let get_file_ptr = |bank: &[u8]| {
        bank.as_ptr() as usize - start.as_ptr() as usize + 0x80000
    };
    let get_addr = |ptr: usize| {
        let in_bank = ptr & 0x7FFF;
        let bank_id = ptr >> 15;
        in_bank + (bank_id << 16) + 0x8000
    };
    let mut push_fsp = |cur: &mut Option<usize>, bank: &[u8]| {
        if let Some(c) = cur.take() {
            let r = get_file_ptr(bank);
            if r-c >= 16 {
                //println!("free space at ${:06X}, len {:04X}", get_addr(c), r-c);
                free.push([c,r]);
            }
        }
    };
    while bank.len() > 8 {
        if &bank[..4] == b"STAR" {
            bank.advance(4);
            let len = bank.get_u16_le();
            let unlen = bank.get_u16_le();
            if len == !unlen {
                push_fsp(&mut current_area, bank);
                //println!("rat at ${:06X}, len {:04X}", get_addr(get_file_ptr(bank)), len);
                let ptr = get_file_ptr(bank);
                rats.push([ptr, ptr+len as usize]);
                bank.advance(len as usize);
            }
        } else {
            if bank[0] != 0 {
                //println!("Unprotected byte {:02X} at ${:06X}", bank[0], get_addr(get_file_ptr(bank)));
                push_fsp(&mut current_area, bank);
            } else {
                if current_area.is_none() {
                    current_area = Some(get_file_ptr(bank));
                }
            }
            bank.advance(1);
        }
    }
    push_fsp(&mut current_area, bank);
    RomSpace { free, rats }
}

pub fn write_rat(rom: &mut Rom, offset: usize, len: usize, span: &ContextStr) {
    use bytes::BufMut;
    let mut data = vec![];
    data.put(&b"STAR"[..]);
    data.put_u16_le(len as u16);
    data.put_u16_le(!(len as u16));
    rom.write_at_raw(offset as _, &data);
}

pub fn resolve_freespace(rom: &mut Rom, asm: &mut Assembler) {
    let mut fsp = parse_freespace(rom);
    'outer: for i in asm.segments_mut() {
        match i.start {
            StartKind::Freecode | StartKind::Freedata => {
                for (idx, c) in fsp.free.iter().enumerate() {
                    let [mut start, end] = c;
                    if end - start <= i.offset + 8 { continue; }
                    // does it fit without bankcrossing?
                    let bank_border = (start & !0x7FFF) + 0x8000;
                    println!("{:06X} {:06X} {:06X} {:06X}", start, end, start + i.offset + 8, bank_border);
                    if start + i.offset + 8 >= bank_border {
                        if *end < bank_border { continue; }
                        start = bank_border;
                        if end - start <= i.offset + 8 { continue; }
                        println!("corr: {:06X} {:06X} {:06X} {:06X}", start, end, start + i.offset + 8, bank_border);
                    }
                    let offset = start;
                    fsp.free[idx][0] = start + i.offset + 8;
                    write_rat(rom, offset, i.offset, &i.span);
                    fsp.rats.push([offset, offset+i.offset+8]);
                    let addr = rom.mapper().map_to_addr(offset+8) | 0x800000;
                    i.start = StartKind::Expression(crate::expression::Expression::value(i.span.clone(), addr as f64));
                    continue 'outer;
                }/*
                if let Some(fsp_id) = fsp.free.iter().position(|c| c[1] - c[0] > i.offset + 8) {
                    println!("using freespace {:X?} len {:X}", fsp.free[fsp_id], i.offset);
                    // TODO: figure out bankcross
                    let offset = fsp.free[fsp_id][0];
                    fsp.free[fsp_id][0] += i.offset + 8;
                    write_rat(rom, offset, i.offset, &i.span);
                    fsp.rats.push([offset, offset+i.offset+8]);
                    let addr = rom.mapper().map_to_addr(offset+8);
                    i.start = StartKind::Expression(crate::expression::Expression::value(i.span.clone(), addr as f64));
                } else {
                    panic!("no freespace in rom");
                }*/
            },
            _ => {}
        }
    }
}
