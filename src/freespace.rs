use crate::rom::Rom;
use crate::assembler::{StartKind, Assembler};
use crate::context::ContextStr;

use bytes::Buf;

pub fn parse_freespace(rom: &mut Rom) -> Vec<[usize;2]> {
    let mut bank = rom.freespace_area();
    let start = bank;
    let mut areas = vec![];
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
                areas.push([c,r]);
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
    areas
}

pub fn write_rat(rom: &mut Rom, offset: usize, len: usize, span: &ContextStr) {
    use bytes::BufMut;
    let mut data = vec![];
    data.put(&b"STAR"[..]);
    data.put_u16(len as u16);
    data.put_u16(!(len as u16));
    rom.write_at_raw(offset as _, &data);
}

pub fn resolve_freespace(rom: &mut Rom, asm: &mut Assembler) {
    let mut fsp = parse_freespace(rom);
    for i in asm.segments_mut() {
        match i.start {
            StartKind::Freecode | StartKind::Freedata => {
                if let Some(fsp_id) = fsp.iter().position(|c| c[1] - c[0] > i.offset + 8) {
                    // TODO: figure out bankcross
                    let offset = fsp[fsp_id][0];
                    fsp[fsp_id][0] += i.offset + 8;
                    write_rat(rom, offset, i.offset, &i.span);
                    let addr = rom.mapper().map_to_addr(offset+8);
                    i.start = StartKind::Expression(crate::expression::Expression::value(i.span.clone(), addr as f64));
                } else {
                    panic!("no freespace in rom");
                }
            },
            _ => {}
        }
    }
}
