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
    labels: HashMap<usize, Expression>
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            segments: vec![],
            labels: HashMap::new()
        }
    }
    pub fn new_segment(&mut self, span: ContextStr, s: StartKind, target: &mut Target) {
        let label_id = target.segment_label(self.segments.len());
        println!("new segment: {}", label_id);
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
                self.labels.insert(*id, Expression::label_offset(stmt.span.clone(), label_id, stmt.offset as _));
            }
            _ => {},
        }
    }
    pub fn write_to_file<W:Write+Seek>(&self, target: &mut Target, mut w: W) {
        for i in self.segments.iter() {
            let addr = match &i.start {
                StartKind::Expression(e) => e.try_eval(target, self),
                _ => panic!("internal error: freespace pointer not resolved yet")
            };
            let addr = if let Some(addr) = addr { addr as u64 } else { return; };
            // TODO: this needs to be abstracted into a mapper
            if  (addr&0xFE0000)==0x7E0000        //wram
            ||  (addr&0x408000)==0x000000        //hardware regs, ram mirrors, other strange junk
            ||  (addr&0x708000)==0x700000 {      //sram (low parts of banks 70-7D)
                target.push_error(i.span.clone(), 0, "Attempt to seek to unmapped area".into());
                return;
            }
            let offset = ((addr&0x7F0000)>>1|(addr&0x7FFF));

            w.seek(SeekFrom::Start(offset));
            for s in i.stmts.iter() {
                match &s.kind {
                    StatementKind::Print { expr } => {
                        println!("trying: {:?}", expr);
                        println!("{:?}", expr.try_eval(target, self));
                    },
                    StatementKind::Data { expr } => {
                        let val = expr.try_eval(target, self).unwrap() as u32;
                        let val = val.to_le_bytes();
                        w.write_all(&val[..s.size]).unwrap();
                    },
                    _ => {}
                }
            }
        }
    }
    pub fn resolve_exprs(&mut self) {

    }
    pub fn get_label_value(&self, label: usize) -> Option<&Expression> {
        self.labels.get(&label)
    }
}
