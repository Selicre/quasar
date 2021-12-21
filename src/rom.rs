// Storage for ROM files, mapper support, etc.
//
use crate::context::ContextStr;
use crate::message::{Message, errors};

#[derive(Copy, Clone)]
pub enum Mapper {
    NoRom,
    LoRom,
    HiRom
}

impl Mapper {
    pub fn map_to_file(&self, addr: usize) -> Option<usize> {
        match self {
            Mapper::NoRom => Some(addr),
            Mapper::LoRom => {
                if (addr&0xFE0000)==0x7E0000        //wram
                || (addr&0x408000)==0x000000        //hardware regs, ram mirrors, other strange junk
                || (addr&0x708000)==0x700000 {      //sram (low parts of banks 70-7D)
                    None
                } else {
                    Some((addr&0x7F0000)>>1|(addr&0x7FFF))
                }
            }
            Mapper::HiRom => {
                if (addr&0xFE0000)==0x7E0000       //wram
                || (addr&0x408000)==0x000000 {     //hardware regs, ram mirrors, other strange junk
                    None
                } else {
                    Some(addr&0x3FFFFF)
                }
            }
        }
    }
    pub fn map_to_addr(&self, offset: usize) -> usize {
        match self {
            Mapper::NoRom => offset,
            Mapper::LoRom => {
                let in_bank = offset & 0x7FFF;
                let bank = offset >> 15;
                (bank << 16) + in_bank + 0x8000
            }
            Mapper::HiRom => {
                offset | 0xC00000
            }
        }
    }
}

#[derive(Clone)]
pub struct Rom {
    buf: Vec<u8>,
    mapper: Mapper,
    bankcross: bool,
}

impl Rom {
    pub fn new(buf: Vec<u8>) -> Self {
        Self {
            buf, mapper: Mapper::LoRom, bankcross: false,
        }
    }
    pub fn mapper(&self) -> Mapper {
        self.mapper
    }
    pub fn set_bankcross(&mut self, bankcross: bool) {
        self.bankcross = bankcross;
    }
    pub fn check_bankcross(&mut self, old: u32, new: u32) -> bool {
        if !self.bankcross {
            let old = self.mapper.map_to_file(old as _);
            let new = self.mapper.map_to_file(new as _);
            if let Some((old,new)) = old.zip(new) {
                old >> 15 != new >> 15
            } else {
                true
            }
        } else {
            false
        }
    }
    pub fn read_at(&mut self, addr: u32, len: usize, span: &ContextStr) -> Result<f64, Message> {
        let offset = self.mapper.map_to_file(addr as _)
            .ok_or(errors::rom_unmapped(span.clone()))?;
        let end = offset+len;
        if !self.bankcross && offset>>15 != end>>15 {
            return Err(errors::rom_bank_crossed(span.clone()));
        }
        if self.buf.len() < end {
            return Err(errors::expr_read_file_oob(span.clone()));
        }
        let mut v = [0; 4];
        v[..len].copy_from_slice(&self.buf[offset..end]);
        Ok(u32::from_le_bytes(v) as f64)
    }
    pub fn write_at_raw(&mut self, offset: usize, data: &[u8]) {
        let end = offset+data.len();
        if self.buf.len() < end {
            self.buf.resize(end, 0);
        }
        self.buf[offset..end].copy_from_slice(data);
    }
    pub fn write_at(&mut self, addr: u32, data: &[u8], span: &ContextStr) -> Option<()> {
        let offset = if let Some(c) = self.mapper.map_to_file(addr as _) { c } else {
            errors::rom_unmapped(span.clone()).push();
            return None;
        };
        let end = offset+data.len();
        if !self.bankcross && offset>>15 != (end-1)>>15 {
            errors::rom_bank_crossed(span.clone()).push();
            return None;
        }
        if self.buf.len() < end {
            self.buf.resize(end, 0);
        }
        self.buf[offset..end].copy_from_slice(data);
        Some(())
    }
    pub fn as_slice(&self) -> &[u8] {
        &self.buf
    }
    pub fn freespace_area(&self) -> &[u8] {
        if self.buf.len() > 0x80000 {
            &self.buf[0x80000..]
        } else {
            &[]
        }
    }
    pub fn fix_checksum(&mut self) {
        let src = ContextStr::empty();
        self.write_at(0xFFDC, b"\xFF\xFF\0\0", &src);
        let cksum = self.checksum();
        self.write_at(0xFFDE, &cksum.to_le_bytes(), &src);
        self.write_at(0xFFDC, &(!cksum).to_le_bytes(), &src);
    }
    pub fn checksum(&self) -> u16 {
        // TODO: npo2 roms
        self.buf.iter().map(|c| *c as u16).sum()
    }
}
