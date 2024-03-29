use crate::executor::Target;
use crate::expression::Expression;
use crate::message::Message;
use crate::context::ContextStr;
use crate::rom::Rom;

use std::collections::HashMap;
use std::io::Write;
use std::cell::Cell;

#[derive(Clone)]
pub enum StartKind {
    Expression(Expression),
    Freespace { align: bool },
}

#[derive(Clone)]
pub struct Segment {
    pub start: StartKind,
    pub label_id: usize,
    pub offset: usize,
    pub pad_byte: Option<u8>,
    pub base_offset: Option<usize>,
    pub span: ContextStr,
    pub stmts: Vec<Statement>
}

impl Segment {
    pub fn new(span: ContextStr, label_id: usize, start: StartKind) -> Self {
        Segment {
            start, label_id, offset: 0, pad_byte: None, base_offset: None, span, stmts: vec![],
        }
    }
    pub fn push(&mut self, mut stmt: Statement) -> &Statement {
        stmt.offset = self.offset;
        stmt.base = self.base_offset;
        self.offset += stmt.size;
        self.base_offset.as_mut().map(|c| {
            *c += stmt.size;
        });
        self.stmts.push(stmt);
        self.stmts.last().unwrap()
    }
    pub fn statements_mut(&mut self) -> &mut [Statement] {
        &mut self.stmts
    }
    pub fn statements(&self) -> &[Statement] {
        &self.stmts
    }
}


pub struct Assembler {
    segments: Vec<Segment>,
    labels: HashMap<usize, Expression>,
    compare: Vec<u8>,
    current_pc: Cell<u32>,
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            segments: vec![],
            labels: HashMap::new(),
            compare: vec![],
            current_pc: Cell::new(u32::MAX)
        }
    }
    pub fn get_datasize(&self, label: usize, span: ContextStr, target: &mut Target) -> Option<f64> {
        // todo: maybe backrefs to find labels?
        for seg in self.segments.iter() {
            if let Some(s) = seg.stmts.iter().position(|c| matches!(c.kind, StatementKind::Label(d) if d == label)) {
                let start = seg.stmts[s].offset;
                let other = seg.stmts[s+1..].iter().find(|c| matches!(c.kind, StatementKind::Label(_)));
                let end = other.unwrap_or_else(|| {
                    Message::warning(span, "`datasize` used on the last label of a segment".into()).push();
                    seg.stmts.last().unwrap()
                }).offset;
                return Some((end - start) as _)
            }
        }
        return None;
    }
    pub fn current_base(&mut self) -> Option<usize> {
        self.segments.last().and_then(|c| c.base_offset)
    }
    pub fn segments_vec(&mut self) -> &mut Vec<Segment> {
        &mut self.segments
    }
    pub fn segments_mut(&mut self) -> &mut [Segment] {
        &mut self.segments
    }
    pub fn segments(&self) -> &[Segment] {
        &self.segments
    }
    pub fn set_compare(&mut self, v: Vec<u8>) {
        self.compare = v;
    }
    pub fn new_segment(&mut self, span: ContextStr, s: StartKind, target: &mut Target) {
        let label_id = target.segment_label(self.segments.len());
        //println!("new segment: {}", label_id);
        self.segments.push(Segment::new(span, label_id, s));
    }
    pub fn new_segment_padded(&mut self, span: ContextStr, s: StartKind, pad: u8, target: &mut Target) {
        let label_id = target.segment_label(self.segments.len());
        //println!("new segment: {}", label_id);
        let mut seg = Segment::new(span, label_id, s);
        seg.pad_byte = Some(pad);
        self.segments.push(seg);
    }
    pub fn resolve_labels(&mut self) {
        for s in self.segments.iter() {
            match s.start {
                StartKind::Expression(ref c) => { self.labels.insert(s.label_id, c.clone()); },
                _ => panic!("unresolved freespace"),
            }
        }
    }
    pub fn append(&mut self, stmt: Statement, target: &mut Target) {
        if self.segments.len() == 0 {
            Message::warning(stmt.span.clone(), "Missing `org` or `freespace` command".into()).push();
            self.new_segment(
                stmt.span.clone(),
                StartKind::Expression(Expression::value(stmt.span.clone(), 0x8000 as _)),
                target
            );
        }
        let seg = self.segments.last_mut().unwrap();
        let label_id = seg.label_id;
        let stmt = seg.push(stmt);
        match &stmt.kind {
            StatementKind::Label(id) => {
                if let Some(base) = stmt.base {
                    let expr = Expression::value(stmt.span.clone(), base as _);
                    let span = stmt.span.clone();
                    let id = *id;
                    self.set_label(id, expr, span, target);
                } else {
                    let expr = Expression::label_offset(stmt.span.clone(), label_id, stmt.offset as _);
                    let span = stmt.span.clone();
                    let id = *id;
                    self.set_label(id, expr, span, target);
                }
            }
            StatementKind::Base { expr } => {
                seg.base_offset = *expr;
            }
            _ => {},
        }
    }
    pub fn set_label(&mut self, id: usize, expr: Expression, span: ContextStr, target: &mut Target) {
        if !self.labels.contains_key(&id) {
            self.labels.insert(id, expr);
        } else {
            target.push_error(span, 0, "Label redefinition".into());
        }
    }
    pub fn write_to_rom(&self, target: &mut Target, rom: &mut Rom) -> Option<()> {
        let debug = false;
        let mut last_addr = 0x8000;
        for i in self.segments.iter() {
            let addr = match &i.start {
                StartKind::Expression(e) => e.try_eval_int(target, self)? as u32,
                _ => panic!("internal error: freespace pointer not resolved yet")
            };
            if debug { println!(" -- segment {:06X}", addr); }
            if let Some(c) = i.pad_byte {
                // TODO: handle this properly
                let d1 = if addr & 0xFFFF == 0x8000 {
                    rom.mapper().map_to_file(addr as usize).unwrap()
                } else {
                    rom.mapper().map_to_file(addr as usize - 1).unwrap() + 1
                };
                let d2 = rom.mapper().map_to_file(last_addr as _).unwrap();
                let dist = d1 - d2;
                if (d1-1)>>15 == d2>>15 {
                    let mut padding = vec![c; dist as usize];
                    rom.write_at(last_addr, &padding[..], &i.span);
                } else {
                    println!("uh oh {:06X} {:06X} {:X} {:X}", d1, d2, (d1-1)>>15, d2>>15);
                    crate::message::errors::rom_bank_crossed(i.span.clone()).push();
                }
            }
            let mut last_addr_bc = addr;
            let addr_start = addr;
            let mut ch = 0;
            for s in i.stmts.iter() {
                if ch != s.offset { println!("uh oh wrong offset: {} != {}", ch, s.offset); }
                ch += s.size;

                last_addr = label_offset(addr_start, s.offset as u32 + s.size as u32);
                let addr = label_offset(addr_start, s.offset as u32);

                if rom.check_bankcross(last_addr_bc, addr) {
                    crate::message::errors::rom_bank_crossed(s.span.clone()).with_help(format!("{:06X} -> {:06X}", last_addr_bc, addr)).push();
                    break;
                }
                if s.size > 0 {
                    last_addr_bc = label_offset(addr_start, s.offset as u32 + s.size as u32 - 1);
                }
                self.current_pc.set(addr);
                match &s.kind {
                    StatementKind::Print { expr } => {
                        let mut out = String::new();
                        for i in expr {
                            if let Some(val) = i.try_eval(false, target, self) {
                                let s = val.1.to_string();
                                out.push_str(&s);
                            }
                        }
                        println!("{}", out);
                        target.push_print(out);
                    },
                    StatementKind::Data { expr } => {
                        if let Some(val) = expr.try_eval_int(target, self) {
                            let val = val.to_le_bytes();
                            rom.write_at(addr, &val[..s.size], &s.span)?;
                        }
                    },
                    StatementKind::InstructionRel { opcode, expr } => {
                        rom.write_at(addr, &[*opcode], &s.span)?;
                        if let Some(val) = expr.try_eval_int(target, self) {
                            let offset = if expr.contains_label() {
                                // actually make relative
                                if let Some(base) = s.base {
                                    base as i32 + s.size as i32
                                } else {
                                    addr as i32 + s.size as i32
                                }
                            } else {
                                0
                            };
                            //println!("rel val: {:06X} - {:06X}", val, offset);
                            let rel = (val as i32).wrapping_sub(offset);
                            if s.size == 2 {
                                if rel < -128 || rel > 127 {
                                    crate::message::errors::instr_rel_oob(s.span.clone(), rel, offset, val as i32).push();
                                }
                            }
                            let rel = rel.to_le_bytes();
                            rom.write_at(addr+1, &rel[..s.size-1], &s.span)?;
                        }
                    },
                    StatementKind::Instruction { opcode, expr, .. } => {
                        rom.write_at(addr, &[*opcode], &s.span)?;
                        if s.size > 1 {
                            if let Some(val) = expr.try_eval_int(target, self) {
                                let val = val.to_le_bytes();
                                rom.write_at(addr+1, &val[..s.size-1], &s.span)?;
                            }
                        }
                    },
                    StatementKind::InstructionRep { opcode } => {
                        for i in 0..s.size as u32 {
                            rom.write_at(addr+i, &[*opcode], &s.span)?;
                        }
                    },
                    StatementKind::Skip => {}
                    StatementKind::Binary { data } => {
                        rom.write_at(addr, &data, &s.span)?;
                    }
                    StatementKind::WarnPc { expr } => {
                        let val = if let Some(c) = expr.try_eval_int(target, self) { c } else { continue; };
                        if addr > val {
                            crate::message::errors::rom_warnpc(s.span.clone(), addr, val).push();
                        }
                    }
                    StatementKind::Assert { expr, msg } => {
                        let val = if let Some(c) = expr.try_eval_float(target, self) { c } else { continue; };
                        if val == 0.0 {
                            let msg = msg.try_eval(false, target, self);
                            crate::message::errors::rom_assert(s.span.clone(), &format!("{:?}", msg)).push();
                        }
                    }
                    StatementKind::Bankcross(n) => {
                        rom.set_bankcross(*n);
                    }
                    _ => {}
                }
                if debug {
                    if let Some(off) = rom.mapper().map_to_file(addr as _) {
                        print!("{:06X} {}", addr, s);
                        if s.size == 0 || off >= rom.as_slice().len() { println!(); continue; }
                        let written = &rom.as_slice()[off..][..s.size.min(16)];
                        println!(" -> {:02X?} [{}]", written, (&*s.span.full_line()).trim());
                    }
                }
                /*if self.compare.len() > 0 {
                    let lhs = &written[..];
                    let rhs = &self.compare[s.offset..s.offset+s.size];
                    if lhs != rhs {
                        //target.push_error(s.span.clone(), 0, format!("Test failed: {:02X?} != {:02X?}", lhs, rhs));
                        println!("test failed: {:02X?} != {:02X?}", lhs, rhs);
                    } else {
                        println!("test passed: {:02X?} == {:02X?}", lhs, rhs);
                    }
                }*/
            }
        }
        Some(())
    }
    pub fn current_pc(&self) -> u32 {
        self.current_pc.get()
    }
    pub fn get_label_value(&self, label: usize) -> Option<&Expression> {
        self.labels.get(&label)
    }
}

pub fn label_offset(start: u32, offset: u32) -> u32 {
    let bank_start = start & 0xFF8000;
    let offset = offset + (start - bank_start);
    bank_start + (offset&0x7FFF) + ((offset&0xFF8000) << 1)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn label_offset_works() {
        assert_eq!(label_offset(0x8000, 0x8000), 0x18000);
        assert_eq!(label_offset(0x8000, 0x10000), 0x28000);
        assert_eq!(label_offset(0xFFFF, 1), 0x18000);
    }
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub offset: usize,          // from the start of the segment
    pub base: Option<usize>,    // if some, then this is used as the absolute value
    pub kind: StatementKind,
    pub size: usize,
    pub span: ContextStr
}

#[derive(Debug, Clone)]
pub enum BankOpt {
    None,
    Value(u8),
    Pc,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Data {
        expr: Expression
    },
    Instruction {
        opcode: u8,
        expr: Expression,
        opt_bank: BankOpt
    },
    InstructionRel {
        opcode: u8,
        expr: Expression
    },
    InstructionRep {
        opcode: u8
    },
    Label(usize),
    Print {
        expr: Vec<Expression>
    },
    Binary {
        data: Vec<u8>
    },
    Skip,
    Base {
        expr: Option<usize>
    },
    WarnPc {
        expr: Expression
    },
    Assert {
        expr: Expression,
        msg: Expression,
    },
    Bankcross(bool),
}

impl Statement {
    pub fn new(kind: StatementKind, size: usize, span: ContextStr) -> Self {
        Self {
            offset: 0, base: None, size, kind, span
        }
    }
    pub fn print(expr: Vec<Expression>, span: ContextStr) -> Self {
        Statement::new(StatementKind::Print { expr }, 0, span)
    }
    pub fn data(expr: Expression, size: usize, span: ContextStr) -> Self {
        Statement::new(StatementKind::Data { expr }, size, span)
    }
    pub fn label(data: usize, span: ContextStr) -> Self {
        Statement::new(StatementKind::Label(data), 0, span)
    }
    pub fn instruction(opcode: u8, expr: Expression, size: usize, span: ContextStr) -> Self {
        Statement::new(StatementKind::Instruction { opcode, expr, opt_bank: BankOpt::Pc }, size, span)
    }
    pub fn instruction_rel(opcode: u8, expr: Expression, size: usize, span: ContextStr) -> Self {
        Statement::new(StatementKind::InstructionRel { opcode, expr }, size, span)
    }
    pub fn instruction_rep(opcode: u8, size: usize, span: ContextStr) -> Self {
        Statement::new(StatementKind::InstructionRep { opcode }, size, span)
    }
    pub fn skip(amt: usize, span: ContextStr) -> Self {
        Statement::new(StatementKind::Skip, amt, span)
    }
    pub fn base(expr: Option<usize>, span: ContextStr) -> Self {
        Statement::new(StatementKind::Base { expr }, 0, span)
    }
    pub fn binary(data: Vec<u8>, span: ContextStr) -> Self {
        let len = data.len();
        Statement::new(StatementKind::Binary { data }, len, span)
    }
    pub fn warnpc(expr: Expression, span: ContextStr) -> Self {
        Statement::new(StatementKind::WarnPc { expr }, 0, span)
    }
    pub fn assert(expr: Expression, msg: Expression, span: ContextStr) -> Self {
        Statement::new(StatementKind::Assert { expr, msg }, 0, span)
    }
    pub fn bankcross(value: bool, span: ContextStr) -> Self {
        Statement::new(StatementKind::Bankcross(value), 0, span)
    }
}
impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use std::fmt::Write;
        write!(f, "{:04X} ", self.offset)?;
        if let Some(b) = self.base {
            write!(f, "[{:06X}] ", b)?;
        }
        write!(f, "len {:X} ", self.size)?;
        match &self.kind {
            StatementKind::Data { expr } => write!(f, "data  {}", expr),
            StatementKind::Print { expr } => {
                write!(f, "print ")?;
                let mut first = true;
                for i in expr.iter() {
                    if !std::mem::take(&mut first) { write!(f, "; ")?; }
                    write!(f, "{}", i)?;
                }
                Ok(())
            },
            StatementKind::Instruction { opcode, expr, .. } => {
                write!(f, "instr ${:02X}", opcode)?;
                if !expr.is_empty() {
                    write!(f, ": {}", expr)?;
                }
                Ok(())
            },
            StatementKind::InstructionRel { opcode, expr } => {
                write!(f, "insrl ${:02X}", opcode)?;
                if !expr.is_empty() {
                    write!(f, ": {}", expr)?;
                }
                Ok(())
            },
            StatementKind::Binary { data } => write!(f, "bin  "),
            StatementKind::Label(id) => write!(f, "label %{}", id),
            c => write!(f, "{:?}", c)
        }?;
        Ok(())
    }
}
