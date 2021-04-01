use crate::expr::Expression;
use crate::context::ContextStr;

#[derive(Debug)]
pub struct Statement {
    pub offset: usize,  // from the start of the segment
    pub kind: StatementKind,
    pub size: usize,
    pub span: ContextStr
}

#[derive(Debug)]
pub enum StatementKind {
    Data {
        expr: Expression
    },
    DataStr {
        data: String,
        size: usize,
    },
    Instruction {
        opcode: u8,
        expr: Expression
    },
    InstructionRel {
        opcode: u8,
        expr: Expression
    },
    InstructionRep {
        opcode: u8
    },
    Label(usize),
    LabelExpr {
        id: usize,
        expr: Expression
    },
    Print {
        expr: Expression
    }
}

impl Statement {
    pub fn new(kind: StatementKind, size: usize, span: ContextStr) -> Self {
        Self {
            offset: 0, size, kind, span
        }
    }
    pub fn print(expr: Expression, span: ContextStr) -> Self {
        Statement::new(StatementKind::Print { expr }, 0, span)
    }
    pub fn data(expr: Expression, size: usize, span: ContextStr) -> Self {
        Statement::new(StatementKind::Data { expr }, size, span)
    }
    pub fn data_str(data: String, size: usize, span: ContextStr) -> Self {
        let l = size * data.len();
        Statement::new(StatementKind::DataStr { data, size }, l, span)
    }
    pub fn label(data: usize, span: ContextStr) -> Self {
        Statement::new(StatementKind::Label(data), 0, span)
    }
    pub fn instruction(opcode: u8, expr: Expression, size: usize, span: ContextStr) -> Self {
        Statement::new(StatementKind::Instruction { opcode, expr }, size, span)
    }
    pub fn instruction_rel(opcode: u8, expr: Expression, size: usize, span: ContextStr) -> Self {
        Statement::new(StatementKind::InstructionRel { opcode, expr }, size, span)
    }
    pub fn instruction_rep(opcode: u8, size: usize, span: ContextStr) -> Self {
        Statement::new(StatementKind::InstructionRep { opcode }, size, span)
    }
}
