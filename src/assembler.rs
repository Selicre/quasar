use crate::statement::{Statement, StatementKind};
use crate::executor::Target;
use crate::expr::Expression;
use crate::message::Message;
use crate::context::ContextStr;

use std::collections::HashMap;
use std::io::{SeekFrom, Seek, Write};

pub enum StartKind {
    Expression(Expression),
    Freecode,
    Freedata
}

pub struct Segment {
    start: StartKind,
    label_id: usize,
    offset: usize,
    span: ContextStr,
    stmts: Vec<Statement>
}

impl Segment {
    pub fn new(span: ContextStr, label_id: usize, start: StartKind) -> Self {
        Segment {
            start, label_id, offset: 0, span, stmts: vec![],
        }
    }
    pub fn push(&mut self, mut stmt: Statement) -> &Statement {
        stmt.offset = self.offset;
        self.offset += stmt.size;
        self.stmts.push(stmt);
        self.stmts.last().unwrap()
    }
}


pub struct Assembler {
    segments: Vec<Segment>,
    labels: HashMap<usize, Expression>,
    compare: Vec<u8>,
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            segments: vec![],
            labels: HashMap::new(),
            compare: vec![],
        }
    }
    pub fn get_datasize(&self, label: usize, span: ContextStr, target: &mut Target) -> Option<f64> {
        // todo: maybe backrefs to find labels?
        for seg in self.segments.iter() {
            if let Some(s) = seg.stmts.iter().position(|c| matches!(c.kind, StatementKind::Label(d) if d == label)) {
                let start = seg.stmts[s].offset;
                let other = seg.stmts[s+1..].iter().find(|c| matches!(c.kind, StatementKind::Label(_)));
                let end = other.unwrap_or_else(|| {
                    target.push_warning(span, 0, "datasize function used on the last label of a segment".into());
                    seg.stmts.last().unwrap()
                }).offset;
                return Some((end - start) as _)
            }
        }
        return None;
    }
    pub fn set_compare(&mut self, v: Vec<u8>) {
        self.compare = v;
    }
    pub fn new_segment(&mut self, span: ContextStr, s: StartKind, target: &mut Target) {
        let label_id = target.segment_label(self.segments.len());
        //println!("new segment: {}", label_id);
        match s {
            StartKind::Expression(ref c) => { self.labels.insert(label_id, c.clone()); },
            _ => panic!("no freespace yet"),
        }
        self.segments.push(Segment::new(span, label_id, s));
    }
    pub fn append(&mut self, stmt: Statement, target: &mut Target) {
        if self.segments.len() == 0 {
            target.push_msg(Message::warning(stmt.span.clone(), 0, "Missing `org` or `freespace` command".into()));
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
                let expr = Expression::label_offset(stmt.span.clone(), label_id, stmt.offset as _);
                let span = stmt.span.clone();
                let id = *id;
                self.set_label(id, expr, span, target);
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
    pub fn write_to_file<W:Write+Seek>(&self, target: &mut Target, mut w: W) -> Option<()> {
        for i in self.segments.iter() {
            let addr = match &i.start {
                StartKind::Expression(e) => e.try_eval_int(target, self)? as u64,
                _ => panic!("internal error: freespace pointer not resolved yet")
            };
            // TODO: this needs to be abstracted into a mapper
            if  (addr&0xFE0000)==0x7E0000        //wram
            ||  (addr&0x408000)==0x000000        //hardware regs, ram mirrors, other strange junk
            ||  (addr&0x708000)==0x700000 {      //sram (low parts of banks 70-7D)
                target.push_error(i.span.clone(), 0, "Attempt to seek to unmapped area".into());
                return None;
            }
            let offset = ((addr&0x7F0000)>>1|(addr&0x7FFF));

            w.seek(SeekFrom::Start(offset));
            let mut offset = 0;
            for s in i.stmts.iter() {
                if offset != s.offset { panic!("uh oh wrong offset: {} != {}", offset, s.offset); }
                let mut written = vec![];
                match &s.kind {
                    StatementKind::Print { expr } => {
                        println!("trying: {:?}", expr);
                        println!("{:?}", expr.try_eval(false, target, self));
                    },
                    StatementKind::DataStr { data, size } => {
                        if *size == 1 {
                            w.write_all(data.as_bytes()).unwrap();
                            written.write_all(data.as_bytes()).unwrap();
                        }
                    },
                    StatementKind::Data { expr } => {
                        let val = expr.try_eval_int(target, self)?;
                        let val = val.to_le_bytes();
                        w.write_all(&val[..s.size]).unwrap();
                        written.write_all(&val[..s.size]).unwrap();
                    },
                    StatementKind::InstructionRel { opcode, expr } => {
                        w.write_all(&[*opcode]).unwrap();
                        written.write_all(&[*opcode]).unwrap();
                        let mut val = expr.try_eval_int(target, self)?;
                        //println!("rel val: {}", val);
                        if expr.contains_label() {
                            // actually make relative
                            val = val.wrapping_sub(addr as u32 + s.offset as u32 + s.size as u32);
                        }
                        let val = val.to_le_bytes();
                        w.write_all(&val[..s.size-1]).unwrap();
                        written.write_all(&val[..s.size-1]).unwrap();
                    },
                    StatementKind::Instruction { opcode, expr } => {
                        w.write_all(&[*opcode]).unwrap();
                        written.write_all(&[*opcode]).unwrap();
                        if s.size > 1 {
                            let val = expr.try_eval_int(target, self)?;
                            let val = val.to_le_bytes();
                            w.write_all(&val[..s.size-1]).unwrap();
                            written.write_all(&val[..s.size-1]).unwrap();
                        }
                    },
                    StatementKind::InstructionRep { opcode } => {
                        for _ in 0..s.size {
                            w.write_all(&[*opcode]).unwrap();
                            written.write_all(&[*opcode]).unwrap();
                        }
                    },
                    _ => {}
                }
                if self.compare.len() > 0 {
                    let lhs = &written[..];
                    let rhs = &self.compare[s.offset..s.offset+s.size];
                    if lhs != rhs {
                        //target.push_error(s.span.clone(), 0, format!("Test failed: {:02X?} != {:02X?}", lhs, rhs));
                        println!("test failed: {:02X?} != {:02X?}", lhs, rhs);
                    } else {
                        println!("test passed: {:02X?} == {:02X?}", lhs, rhs);
                    }
                }
                offset += s.size;
            }
        }
        Some(())
    }
    pub fn get_label_value(&self, label: usize) -> Option<&Expression> {
        self.labels.get(&label)
    }
}
