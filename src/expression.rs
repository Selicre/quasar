// Expression parser
use crate::context::ContextStr;
use crate::executor::Target;
use crate::lexer::{TokenList, TokenKind};
use crate::assembler::Assembler;
use crate::message::errors;

mod parser;
mod evaluator;

pub use parser::parse_label;

type Node = (ContextStr, ExprNode);

#[derive(Debug, Clone)]
pub struct Expression {
    nodes: Vec<Node>
}

#[derive(Debug, Clone)]
pub enum ExprNode {
    Binop(Binop),
    Unop(Unop),
    Call(usize, ContextStr),
    Value(Value),
    FuncArg(usize),
    Empty
}


impl Expression {
    pub fn empty() -> Self {
        Self { nodes: vec![] }
    }
    pub fn pc(span: ContextStr) -> Self {
        Self { nodes: vec![(span.clone(), ExprNode::Call(0, span))] }
    }
    pub fn value(span: ContextStr, value: f64) -> Self {
        Self { nodes: vec![(span, ExprNode::Value(Value::Literal { value, size_hint: 0 }))] }
    }
    pub fn label(span: ContextStr, value: usize) -> Self {
        Self { nodes: vec![(span, ExprNode::Value(Value::Label(value)))] }
    }
    pub fn is_label(&self, value: usize) -> bool {
        self.nodes.len() == 1 && matches!(self.nodes[0].1, ExprNode::Value(Value::Label(val)) if val == value)
    }
    pub fn string(span: ContextStr, value: impl Into<String>) -> Self {
        Self { nodes: vec![(span, ExprNode::Value(Value::String(value.into())))] }
    }
    pub fn label_offset(span: ContextStr, label: usize, value: f64) -> Self {
        Self { nodes: vec![
            (span.clone(), ExprNode::Value(Value::Literal { value, size_hint: 0 })),
            (span.clone(), ExprNode::Value(Value::Label(label))),
            (span, ExprNode::Binop(Binop::LabelOffset)),
        ] }
    }
    pub fn block_move(span: ContextStr, arg1: Expression, mut arg2: Expression) -> Self {
        let mut nodes = arg1.nodes;
        nodes.append(&mut arg2.nodes);
        nodes.push((span, ExprNode::Binop(Binop::BlockMove)));
        Self { nodes }
    }
    pub fn contains_label(&self) -> bool {
        self.nodes.iter().any(|c| matches!(
            c.1,
            ExprNode::Value(Value::Label(_))
        ))
    }
    pub fn size_hint(&self) -> Option<usize> {
        self.nodes.iter().filter_map(|c| match c.1 {
            ExprNode::Value(Value::Literal { size_hint, .. }) if size_hint > 0 => Some(size_hint),
            _ => None
        }).max()
    }
    pub fn is_empty(&self) -> bool {
        self.nodes.len() == 0
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.nodes.len() == 0 {
            write!(f, "[empty]")?;
        }
        let mut first = true;
        for (span, expr) in self.nodes.iter() {
            if !std::mem::take(&mut first) { write!(f, " ")?; }
            write!(f, "{}", expr)?;
        }
        Ok(())
    }
}
impl std::fmt::Display for ExprNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ExprNode::Value(v) => match v {
                Value::Literal { value, size_hint } => if value.fract() < 1e-6 {
                    write!(f, "${:X}", *value as i64)
                } else {
                    write!(f, "{}", value)
                },
                Value::Label(i) => write!(f, "%{}", i),
                Value::String(i) => write!(f, "{:?}", i),
            },
            ExprNode::Binop(c) => write!(f, "{:?}", c),
            ExprNode::Unop(c) => write!(f, "{:?}", c),
            ExprNode::FuncArg(c) => write!(f, "funcarg {}", c),
            ExprNode::Call(c,s) => write!(f, "{}({})", s, c),
            ExprNode::Empty => write!(f, "empty")
        }?;
        Ok(())
    }
}

pub fn int_cast(val: f64) -> u32 {
    if val >= 0.0 {
        if val > u32::MAX as f64 {
            u32::MAX
        } else {
            val as u32
        }
    } else {
        if -val > u32::MAX as f64 {
            i32::MAX as u32 + 1
        } else {
            (!((-val) as u32)) + 1
        }
    }
}

#[derive(Debug,Clone)]
pub enum StackValue {
    Number {
        value: f64,
        origin: Option<usize>,  // label that birthed it
    },
    String(String),
}

impl StackValue {
    fn set_origin(&mut self, id: usize) {
        match self {
            StackValue::Number { origin, .. } => *origin = Some(id),
            _ => {}
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            Self::Number { value, .. } => format!("{}", value),
            Self::String(s) => s.clone()
        }
    }
}

#[derive(Debug,Clone)]
pub enum Value {
    Literal { value: f64, size_hint: usize },
    Label(usize),
    String(String),
}
impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Literal { value, .. } => *value != 0.0,
            _ => true
        }
    }
}

#[derive(Clone,Debug,PartialEq,Eq,Hash)]
pub enum Label {
    Named { stack: Vec<String>, invoke: Option<usize> },
    AnonPos { depth: usize, pos: usize, invoke: Option<usize> },
    AnonNeg { depth: usize, pos: usize, invoke: Option<usize> },
    Segment(usize),
    Phantom(usize),
}
impl Label {
    pub fn glue_sub(&mut self) {
        if let Label::Named { ref mut stack, .. } = self {
            *stack = vec![stack.join("_")];
        }
    }
    pub fn glue_namespace(&mut self, namespace: &[String]) {
        if let Label::Named { ref mut stack, .. } = self {
            let mut n = namespace.to_vec();
            n.push(stack.pop().unwrap());
            *stack = vec![n.join("_")];
        }
    }
    pub fn no_colon(&self) -> bool {
        if let Label::Named { ref stack, ref invoke } = self {
            stack.len() > 1 || invoke.is_some()
        } else {
            true
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    BitAnd,
    BitOr,
    BitXor,
    Pow,
    // quasar-only
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
    And,
    Or,
    // internal
    BlockMove,
    LabelOffset,
}

#[derive(Debug, Copy, Clone)]
pub enum Unop {
    Unp,
    Unm,
    BitNot,
    Not,
    Bank,
}

impl Binop {
    fn precedence(&self) -> (u8, u8) {
        use Binop::*;
        match self {
            Pow => (13,12),
            Mul | Div | Mod => (10,11),
            Add | Sub => (8,9),
            Shl | Shr => (6,7),
            BitAnd | BitOr | BitXor => (4,5),
            Eq | Ne | Gt | Ge | Lt | Le => (2,3),
            And | Or => (0,1),
            _ => unreachable!(),
        }
    }
    fn exec(&self, span: &ContextStr, l: f64, r: f64, target: &mut Target) -> f64 {
        use Binop::*;
        let li = l as i64;
        let ri = r as i64;

        let from_bool = |res| if res { 1.0 } else { 0.0 };
        match self {
            Pow => l.powf(r),
            Mul => l * r,
            Div | Mod if r == 0.0 => {
                target.push_error(span.clone(), 30, format!("Division by zero"));
                0.0
            },
            Div => l / r,
            Mod => l % r,
            Add => l + r,
            Sub => l - r,
            Shl | Shr if r < 0.0 => {
                target.push_error(span.clone(), 31, format!("Shift by negative number"));
                0.0
            },
            Shl => (li << ri) as f64,
            Shr => (li >> ri) as f64,
            BitAnd => (li & ri) as f64,
            BitOr  => (li | ri) as f64,
            BitXor => (li ^ ri) as f64,
            Eq => from_bool(l == r),
            Ne => from_bool(l != r),
            Gt => from_bool(l >  r),
            Ge => from_bool(l >= r),
            Lt => from_bool(l <  r),
            Le => from_bool(l <= r),
            And =>from_bool((l != 0.0) && (r != 0.0)),
            Or => from_bool((l != 0.0) || (r != 0.0)),
            BlockMove => (((ri & 0xFF) << 8) | (li & 0xFF)) as f64,
            LabelOffset => {
                let start = ri;
                let offset = li;
                let bank_start = start & 0xFF8000;
                let offset = offset + (start - bank_start);
                (bank_start + (offset&0x7FFF) + ((offset&0xFF8000) << 1)) as f64
            }
        }
    }
}


impl Unop {
    fn precedence(&self) -> u8 {
        20
    }
    fn exec(&self, span: &ContextStr, arg: f64, target: &mut Target) -> f64 {
        use Unop::*;
        let iarg = arg as i32;
        match self {
            Unp => arg,
            Unm => -arg,
            BitNot => (!iarg) as f64,
            Not => if arg != 0.0 { 0.0 } else { 1.0 },
            Bank => (iarg >> 16) as f64,
        }
    }
}

