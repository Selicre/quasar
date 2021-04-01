// Expression parser
use crate::context::ContextStr;
use crate::executor::{TokenList,Target};
use crate::lexer::TokenKind;
use crate::assembler::Assembler;

type Node = (ContextStr, ExprNode);

#[derive(Debug, Clone)]
pub struct Expression {
    nodes: Vec<Node>
}

#[derive(Debug, Clone)]
pub enum ExprNode {
    Binop(Binop),
    Unop(Unop),
    Call(usize),
    Value(Value),
    Empty
}

fn parse_literal(tokens: &mut TokenList<'_>) -> Option<Node> {
    let mut peek = tokens.clone();
    let t = peek.next_non_wsp()?;
    let t = match t.kind {
        TokenKind::Number { value, radix, length } => {
            let val = ExprNode::Value(Value::Literal {
                value: value as f64,
                size_hint: match radix {
                    16 => length / 2,
                    2 => length / 8,
                    _ => 0,
                }
            });
            (t.span.clone(), val)
        }
        _ => return None
    };
    *tokens = peek;
    Some(t)
}

pub fn parse_ident(tokens: &mut TokenList<'_>, has_macro: bool, target: &mut Target) -> Option<Node> {
    parse_label(tokens, has_macro, target).map(|(s,t)| {
        (s,ExprNode::Value(Value::Label(target.label_id(t))))
    })
}
pub fn parse_label(tokens: &mut TokenList<'_>, has_macro: bool, target: &mut Target) -> Option<(ContextStr, Label)> {
    let mut peek = tokens.clone();
    let t = peek.next_non_wsp()?;
    let t = match t.kind {
        TokenKind::Ident => {
            let id = Label::Named { stack: vec![t.span.to_string()] };
            (t.span.clone(), id)
        }
        TokenKind::Symbol => {
            match t.span.chars().next().expect("Symbol with no chars?") {
                '?' if !has_macro => {
                    parse_label(tokens, true, target)?
                },
                '.' => {
                    let mut depth = t.span.len();
                    let c = peek.next();
                    if let Some(c) = c {
                        if c.is_ident() {
                            (c.span.clone(), Label::Named { stack: target.resolve_sub(depth, c.span.clone()) })
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                },
                '+' => {
                    let mut depth = t.span.len();
                    (t.span.clone(), Label::AnonPos { depth, pos: target.resolve_anon(depth, true, t.span.clone()) })
                },
                '-' => {
                    let mut depth = t.span.len();
                    (t.span.clone(), Label::AnonNeg { depth, pos: target.resolve_anon(depth, false, t.span.clone()) })
                },
                _ => return None
            }
        }
        _ => return None
    };
    *tokens = peek;
    Some(t)
}
fn parse_unop(tokens: &mut TokenList<'_>) -> Option<Node> {
    let mut peek = tokens.clone();
    let t = peek.next_non_wsp()?;
    let mut t = match t.kind {
        TokenKind::Symbol => {
            // unop or sublabel
            let op = match &*t.span {
                "+" => Unop::Unp,
                "-" => Unop::Unm,
                "~" => Unop::BitNot,
                "!" => Unop::Not,
                "<:" => Unop::Bank,
                c if c.starts_with("+") => Unop::Unp,
                c if c.starts_with("-") => if c.len() % 2 == 0 { Unop::Unp } else { Unop::Unm },
                _ => return None
            };
            (t.span.clone(), ExprNode::Unop(op))
        }
        _ => return None
    };
    *tokens = peek;
    Some(t)
}
fn parse_binop(tokens: &mut TokenList<'_>) -> Option<Node> {
    use Binop::*;
    let mut peek = tokens.clone();
    let t = peek.next_non_wsp()?;
    let mut t = match t.kind {
        TokenKind::Symbol => {
            // unop or sublabel
            let op = match &*t.span {
                "+"  => Add,
                "-"  => Sub,
                "*"  => Mul,
                "/"  => Div,
                "%"  => Mod,
                ">>" => Shl,
                "<<" => Shr,
                "&"  => BitAnd,
                "|"  => BitOr,
                "^"  => BitXor,
                "**" => Pow,
                "==" => Eq,
                "!=" => Ne,
                ">"  => Gt,
                ">=" => Ge,
                "<"  => Lt,
                "<=" => Le,
                "&&" => And,
                "||" => Or,
                _ => return None
            };
            (t.span.clone(), ExprNode::Binop(op))
        }
        _ => return None
    };
    *tokens = peek;
    Some(t)
}

#[derive(Debug)]
struct S {
    node: Node,
    children: Vec<Self>
}

impl S {
    fn atom(node: Node) -> Self { Self { node, children: vec![] } }
    fn cons(node: Node, children: Vec<Self>) -> Self { Self { node, children } }
    fn empty(ctx: ContextStr) -> Self { Self::atom((ctx, ExprNode::Empty)) }
    fn is_empty(&self) -> bool { matches!(self.node.1, ExprNode::Empty) }
    fn to_vec(self, v: &mut Vec<Node>) {
        for i in self.children {
            i.to_vec(v);
        }
        v.push(self.node)
    }
    fn fmt_inner(&self, f: &mut std::fmt::Formatter, level: usize) -> std::fmt::Result {
        use std::fmt::Write;
        if self.children.len() > 0 { f.pad("(")?; }
        write!(f, "{:?} \"{}\"", self.node.1, self.node.0)?;
        for i in self.children.iter() {
            f.pad("\n")?;
            f.pad(&"  ".repeat(level+1));
            i.fmt_inner(f, level+1)?;
        }
        if self.children.len() > 0 { f.pad(")")?; }
        Ok(())
    }
}
impl std::fmt::Display for S {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.fmt_inner(f, 0);
        Ok(())
    }
}

fn parse_expr(tokens: &mut TokenList<'_>, min_bp: u8, target: &mut Target) -> Option<S> {
    let next = if let Some(c) = tokens.peek_non_wsp() {
        c
    } else {
        return Some(S::empty(tokens.last().map(|c| c.span.clone()).unwrap_or(ContextStr::empty())))
    };
    let mut peek = tokens.clone();

    let mut lhs = if &*next.span == "(" {
        tokens.next_non_wsp();
        let lhs = parse_expr(tokens, 0, target)?;
        let paren = tokens.next_non_wsp();
        if paren.map(|c| &*c.span != ")").unwrap_or(true) {
            target.push_error(next.span.clone(), 0, "Unclosed parenthesis".into());
            return None;
        }
        lhs
    } else if let Some(op) = parse_unop(&mut peek) {
        let rhs = parse_expr(&mut peek, 20, target)?;
        if rhs.is_empty() {
            if let Some(lhs) = parse_ident(tokens, false, target) {
                S::atom(lhs)
            } else {
                target.push_error(next.span.clone(), 0, "Unexpected end of expression".into());
                return None;
            }
        } else {
            *tokens = peek;
            S::cons(op, vec![rhs])
        }
    } else if let Some(lhs) = parse_ident(tokens, false, target) {
        S::atom(lhs)
    } else if let Some(lhs) = parse_literal(tokens) {
        S::atom(lhs)
    } else if &*next.span == ")" || &*next.span == "," || &*next.span == "]" {
        return Some(S::empty(next.span.clone()))
    } else {
        target.push_error(next.span.clone(), 0, "Unknown unary operator".into());
        return None;
    };
    loop {
        let next = if let Some(c) = tokens.peek_non_wsp() {
            c
        } else {
            break;
        };
        let mut peek = tokens.clone();
        let op = if &*next.span == ")" || &*next.span == "," || &*next.span == "]" {
            break;
        } else if let Some(c) = parse_binop(&mut peek) {
            c
        } else {
            target.push_error(next.span.clone(), 0, "Unknown binary operator".into());
            return None;
        };
        if let ExprNode::Binop(ref c) = op.1 {
            let (l_bp, r_bp) = c.precedence();
            if l_bp < min_bp { break; }
            *tokens = peek;
            let rhs = parse_expr(tokens, r_bp, target)?;
            if rhs.is_empty() {
                target.push_error(op.0.clone(), 0, "Empty right-hand side expression".into());
                return None;
            }
            // TODO: reduce in-place if possible
            lhs = S::cons(op, vec![lhs, rhs]);
            continue;
        }
        break;
    }
    Some(lhs)
}

impl Expression {
    pub fn empty() -> Self {
        Self { nodes: vec![] }
    }
    pub fn value(span: ContextStr, value: f64) -> Self {
        Self { nodes: vec![(span, ExprNode::Value(Value::Literal { value, size_hint: 0 }))] }
    }
    pub fn label_offset(span: ContextStr, label: usize, value: f64) -> Self {
        Self { nodes: vec![
            (span.clone(), ExprNode::Value(Value::Label(label))),
            (span.clone(), ExprNode::Value(Value::Literal { value, size_hint: 0 })),
            (span, ExprNode::Binop(Binop::Add)),
        ] }
    }
    pub fn block_move(span: ContextStr, arg1: Expression, mut arg2: Expression) -> Self {
        let mut nodes = arg1.nodes;
        nodes.append(&mut arg2.nodes);
        nodes.push((span, ExprNode::Binop(Binop::BlockMove)));
        Self { nodes }
    }
    pub fn parse(tokens: &mut TokenList<'_>, target: &mut Target) -> Self {
        let s = parse_expr(tokens, 0, target);
        let mut nodes = vec![];
        s.map(|s| {
            println!("{}", s);
            s.to_vec(&mut nodes);
        });
        for i in nodes.iter() {
            if matches!(i.1, ExprNode::Empty) {
                target.push_error(i.0.clone(), 0, "Empty expression".into());
                nodes.clear();
                break;
            }
        }
        Self { nodes }
    }
    pub fn size_hint(&self) -> Option<usize> {
        self.nodes.iter().filter_map(|c| match c.1 {
            ExprNode::Value(Value::Literal { size_hint, .. }) if size_hint > 0 => Some(size_hint),
            _ => None
        }).max()
    }
    pub fn try_eval_stack(&self, target: &mut Target, asm: &Assembler, stack: &mut Vec<(ContextStr, Value)>, label_depth: &mut Vec<usize>) -> Option<()> {
        //println!("evaluating: {:?}", self);
        let get_val = |arg: Value, span: &ContextStr, target: &mut Target| match arg {
            Value::Literal { value, .. } => value,
            Value::Label(_) => {
                panic!("label encountered where it really should not be");
            },
            Value::String => {
                target.push_error(span.clone(), 35, "Strings are not allowed in math operations".into());
                0.0
            },
        };
        for (span, i) in self.nodes.iter() {
            match i {
                ExprNode::Value(Value::Label(c)) if label_depth.contains(&c) => {
                    target.push_error(span.clone(), 0, "Self-referential label used".into());
                    return None;
                }
                ExprNode::Value(Value::Label(c)) => {
                    label_depth.push(*c);
                    //println!("getting label value {}", c);
                    let expr = asm.get_label_value(*c)?;
                    //println!("got: {:?}", expr);
                    expr.try_eval_stack(target, asm, stack, label_depth)?;
                }
                ExprNode::Value(v) => {
                    stack.push((span.clone(), v.clone()));
                }
                ExprNode::Unop(op) => {
                    let (span1, arg) = stack.pop().expect("unbalanced expr");
                    let arg = get_val(arg, &span1, target);
                    let value = op.exec(&span, arg, target);
                    stack.push((span.clone(), Value::Literal { value, size_hint: 0 }));
                }
                ExprNode::Binop(op) => {
                    let (span1, arg1) = stack.pop().expect("unbalanced expr");
                    let arg1 = get_val(arg1, &span1, target);
                    let (span2, arg2) = stack.pop().expect("unbalanced expr");
                    let arg2 = get_val(arg2, &span2, target);
                    let value = op.exec(&span, arg1, arg2, target);
                    stack.push((span.clone(), Value::Literal { value, size_hint: 0 }));
                }
                ExprNode::Call(len) => {
                    panic!()
                }
                _ => panic!()
            }
        }
        Some(())
    }
    pub fn try_eval(&self, target: &mut Target, asm: &Assembler) -> Option<f64> {
        let mut stack = vec![];
        self.try_eval_stack(target, asm, &mut stack, &mut vec![])?;
        let val = stack.pop().expect("unbalanced expr");
        match val.1 {
            Value::Literal { value, .. } => Some(value),
            _ => panic!("uh oh")
        }
    }
    pub fn eval_const(&self, target: &mut Target) -> f64 {
        let mut stack = vec![];

        let get_val = |arg: Value, span: &ContextStr, target: &mut Target| match arg {
            Value::Literal { value, .. } => value,
            Value::Label(_) => {
                target.push_error(span.clone(), 34, "Labels are not allowed in const contexts".into());
                0.0
            },
            Value::String => {
                target.push_error(span.clone(), 35, "Strings are not allowed in math operations".into());
                0.0
            },
        };

        for (span, i) in self.nodes.iter() {
            match i {
                ExprNode::Value(v) => {
                    stack.push((span, v.clone()));
                }
                ExprNode::Unop(op) => {
                    let (span1, arg) = stack.pop().expect("unbalanced expr");
                    let arg = get_val(arg, &span1, target);
                    let value = op.exec(&span, arg, target);
                    stack.push((span, Value::Literal { value, size_hint: 0 }));
                }
                ExprNode::Binop(op) => {
                    let (span1, arg1) = stack.pop().expect("unbalanced expr");
                    let arg1 = get_val(arg1, &span1, target);
                    let (span2, arg2) = stack.pop().expect("unbalanced expr");
                    let arg2 = get_val(arg2, &span2, target);
                    let value = op.exec(&span, arg1, arg2, target);
                    stack.push((span, Value::Literal { value, size_hint: 0 }));
                }
                ExprNode::Call(len) => {
                    panic!()
                }
                _ => panic!()
            }
        }
        let val = stack.pop().expect("unbalanced expr");
        get_val(val.1, val.0, target)
    }
    pub fn is_empty(&self) -> bool {
        self.nodes.len() == 0
    }
}

#[derive(Debug,Clone)]
pub enum Value {
    Literal { value: f64, size_hint: usize },
    Label(usize),
    String,
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
    Named { stack: Vec<String> },
    AnonPos { depth: usize, pos: usize },
    AnonNeg { depth: usize, pos: usize },
    Segment(usize)
}
impl Label {
    pub fn no_colon(&self) -> bool {
        if let Label::Named { ref stack } = self {
            stack.len() > 1
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
    BlockMove
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
            BlockMove => unreachable!(),
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
            BlockMove => (((li & 0xFF) << 8) | (ri & 0xFF)) as f64,
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

