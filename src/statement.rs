use crate::expr::Expression;
use crate::context::ContextStr;

pub struct Statement {
    pub offset: usize,  // from the start of the segment
    pub kind: StatementKind,
    pub size: usize,
    pub span: ContextStr
}

pub enum StatementKind {
    Data {
        expr: Expression
    },
    Instruction {
        opcode: u8,
        expr: Expression
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
    pub fn label(data: usize, span: ContextStr) -> Self {
        Statement::new(StatementKind::Label(data), 0, span)
    }
}
