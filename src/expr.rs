// Expression parser
use crate::context::ContextStr;
use crate::executor::{TokenList,Target};
use crate::lexer::TokenKind;

type Node = (ContextStr, ExprNode);

#[derive(Debug)]
pub struct Expression {
    nodes: Vec<Node>
}

#[derive(Debug)]
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
                ">=" => Le,
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
    fn empty() -> Self { Self::atom((ContextStr::empty(), ExprNode::Empty)) }
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
        return Some(S::empty())
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
    } else if &*next.span == ")" {
        return Some(S::empty())
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
        let op = if &*next.span == ")" {
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
    pub fn parse(tokens: &mut TokenList<'_>, target: &mut Target) -> Self {
        let s = parse_expr(tokens, 0, target);
        let mut nodes = vec![];
        s.map(|s| {
            println!("{}", s);
            s.to_vec(&mut nodes);
        });
        Self { nodes }
    }
}

#[derive(Debug)]
pub enum Value {
    Literal { value: f64, size_hint: usize },
    Label(usize),
    String,
    Error
}

#[derive(Clone,Debug,PartialEq,Eq,Hash)]
pub enum Label {
    Named { stack: Vec<String> },
    AnonPos { depth: usize, pos: usize },
    AnonNeg { depth: usize, pos: usize },
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

#[derive(Debug)]
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
}

#[derive(Debug)]
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
            And | Or => (0,1)
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

