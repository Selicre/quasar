use crate::rom::Rom;
use crate::assembler::{StartKind, Segment, StatementKind, Assembler};
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
            println!("free space at ${:06X}, len {:04X}", get_addr(c), r-c);
            if r-c >= 16 {
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
                println!("rat at ${:06X}, len {:04X}", get_addr(get_file_ptr(bank)), len);
                let ptr = get_file_ptr(bank);
                let len = len as usize + 1;
                rats.push([ptr, ptr+len]);
                check_nested_rats(&bank[..len]);
                bank.advance(len);
            }
        } else {
            if bank[0] != 0 {
                println!("Unprotected byte {:02X} at ${:06X}", bank[0], get_addr(get_file_ptr(bank)));
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

pub fn check_nested_rats(mut data: &[u8]) {
    if let Some(n) = data.windows(4).position(|c| c == b"STAR") {
        data.advance(n);
        let len = data.get_u16_le();
        let unlen = data.get_u16_le();
        if len == !unlen {
            println!("uh oh nested rat!! at offset ${:X}, len {:X}", n, len);
        }
    }
}

pub fn write_rat(rom: &mut Rom, offset: usize, len: usize, span: &ContextStr) {
    use bytes::BufMut;
    let mut data = vec![];
    data.put(&b"STAR"[..]);
    let len = (len-1) as u16;
    data.put_u16_le(len);
    data.put_u16_le(!len);
    rom.write_at_raw(offset as _, &data);
}

pub fn get_segment_len(i: &Segment, asm: &Assembler) -> usize {
    // TODO: this is a band-aid. This needs to be a lot more robust. Perhaps assign each segment a
    // freespace ID, such that it's easy to figure out how they're connected?
    let seg_len = i.offset;
    if let Some(l) = i.stmts.last() {
        match l.kind {
            StatementKind::Label(c) => {
                if let Some(seg) = asm.segments().iter().find(|s| matches!(&s.start, StartKind::Expression(s) if s.is_label(c))) {
                    return seg_len + get_segment_len(seg, asm);
                }
            },
            _ => {}
        }
    }
    seg_len
}

//pub fn run_autoclean(rom: &mut Rom, addrs: Vec<) {
pub fn resolve_freespace(rom: &mut Rom, asm: &mut Assembler) {
    let mut fsp = parse_freespace(rom);
    'outer: for s_idx in 0..asm.segments().len() {
        let i = &asm.segments()[s_idx];
        match i.start {
            StartKind::Freespace { align } => {
                let seg_len = get_segment_len(i, asm);
                for (idx, c) in fsp.free.iter().enumerate() {
                    let [mut start, end] = c;
                    if end - start <= seg_len + 8 { continue; }
                    // does it fit without bankcrossing?
                    let bank_border = (start & !0x7FFF) + 0x8000;
                    println!("{:06X} {:06X} {:06X} {:06X}", start, end, start + seg_len + 8, bank_border);
                    if start + i.offset + 8 >= bank_border || align {
                        if *end < bank_border { continue; }
                        start = bank_border - 8;
                        if end - start <= seg_len + 8 { continue; }
                        println!("corr: {:06X} {:06X} {:06X} {:06X}", start, end, start + seg_len + 8, bank_border);
                    }
                    let offset = start;
                    fsp.free[idx][0] = start + seg_len + 8;
                    write_rat(rom, offset, seg_len, &i.span);
                    fsp.rats.push([offset, offset+seg_len+8]);
                    let addr = rom.mapper().map_to_addr(offset+8) | 0x800000;
                    asm.segments_mut()[s_idx].start = StartKind::Expression(crate::expression::Expression::value(i.span.clone(), addr as f64));
                    continue 'outer;
                }
            },
            _ => {}
        }
    }
}
