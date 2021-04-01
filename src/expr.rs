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
    Call(usize, ContextStr),
    Value(Value),
    FuncArg(usize),
    Empty
}

fn parse_literal(tokens: &mut TokenList<'_>, target: &mut Target) -> Option<Node> {
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
        TokenKind::String => {
            let exp = crate::lexer::expand_str(t.span.clone(), target)?;
            (t.span.clone(), ExprNode::Value(Value::String(exp)))
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
                "<<" => Shl,
                ">>" => Shr,
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
    fn is_ident(&self) -> Option<ContextStr> {
        if self.children.len() == 0 {
            Some(self.node.0.clone())
        } else {
            None
        }
    }
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
    } else if let Some(lhs) = parse_literal(tokens, target) {
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
        } else if &*next.span == "(" {
            // fn call, process arguments
            let _ = tokens.next_non_wsp();
            let mut children = vec![];
            let name = if let Some(c) = lhs.is_ident() {
                c
            } else {
                target.push_error(next.span.clone(), 0, "Only idents can be called as a function".into());
                return None;
            };
            loop {
                match tokens.peek_non_wsp() {
                    Some(c) if &*c.span == ")" => { tokens.next_non_wsp(); break },
                    None => {
                        target.push_error(next.span.clone(), 0, "Unclosed parenthesis".into());
                        return None;
                    },
                    _ => {}
                }
                //println!("tokens: {:?}", tokens.rest());
                let arg = parse_expr(tokens, 0, target)?;
                let comma = tokens.peek_non_wsp();
                match comma {
                    // TODO: unfuck this
                    Some(c) if &*c.span == "," => { tokens.next_non_wsp(); },
                    Some(c) if &*c.span == ")" => {},
                    Some(c) => {
                        target.push_error(c.span.clone(), 0, "Unexpected token".into());
                        return None;
                    },
                    None => {
                        target.push_error(next.span.clone(), 0, "Unclosed parenthesis".into());
                        return None;
                    },
                }
                //println!("tokens: {:?}", tokens.rest());
                children.push(arg)
            }
            lhs = S::cons((name.clone(), ExprNode::Call(children.len(), name)), children);
            continue;
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
            //println!("{}", s);
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
    pub fn parse_fn_body(arguments: &[String], tokens: &mut TokenList<'_>, target: &mut Target) -> Self {
        let s = parse_expr(tokens, 0, target);
        let mut nodes = vec![];
        s.map(|s| {
            //println!("{}", s);
            s.to_vec(&mut nodes);
        });
        for i in nodes.iter_mut() {
            match i.1 {
                ExprNode::Empty => {
                    target.push_error(i.0.clone(), 0, "Empty expression".into());
                    nodes.clear();
                    break;
                },
                ExprNode::Value(Value::Label(c)) => {
                    // replace arguments
                    // this isn't as clean as I'd hope it would be but it is not invasive
                    if let Some(Label::Named { stack }) = target.label_name(c) {
                        if stack.len() == 1 {
                            if let Some(c) = arguments.iter().position(|c| &stack[0] == c) {
                                i.1 = ExprNode::FuncArg(c);
                            }
                        }
                    }
                },
                _ => {}
            }
        }
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
    pub fn try_eval_stack(
        &self,
        constexpr: bool,
        target: &mut Target,
        asm: &Assembler,
        stack: &mut Vec<(ContextStr, StackValue)>,
        args: &[(ContextStr, StackValue)],
        label_depth: &mut Vec<usize>
    ) -> Option<()> {
        //println!("evaluating: {:?}", self);
        let get_val = |stack: &mut Vec<(ContextStr, StackValue)>, target: &mut Target| {
            let (span, arg) = stack.pop().expect("unbalanced expr");
            match arg {
                StackValue::Number { value, .. } => value,
                StackValue::String(_) => {
                    target.push_error(span.clone(), 35, "Strings are not allowed in math operations".into());
                    0.0
                },
            }
        };
        for (span, i) in self.nodes.iter() {
            match i {
                ExprNode::Value(v) if constexpr => {
                    let v = match v {
                        Value::Literal { value, .. } => StackValue::Number { value: *value, origin: None },
                        Value::String(s) => StackValue::String(s.clone()),
                        Value::Label(_) => {
                            // Do not attempt to resolve labels
                            target.push_error(span.clone(), 34, "Labels are not allowed in const contexts".into());
                            return None;
                        }
                    };
                    stack.push((span.clone(), v));
                }
                ExprNode::Value(Value::Label(c)) if label_depth.contains(&c) => {
                    target.push_error(span.clone(), 0, "Self-referential label used".into());
                    return None;
                }
                ExprNode::Value(Value::Label(c)) => {
                    label_depth.push(*c);
                    //println!("getting label value {}", c);
                    let expr = asm.get_label_value(*c);
                    if expr.is_none() {
                        target.push_error(span.clone(), 0, "Can't find label".into());
                    }
                    expr?.try_eval_stack(constexpr, target, asm, stack, &[], label_depth)?;
                    label_depth.pop();
                    stack.last_mut().unwrap().1.set_origin(*c);
                }
                ExprNode::Value(v) => {
                    let v = match v {
                        Value::Literal { value, .. } => StackValue::Number { value: *value, origin: None },
                        Value::String(s) => StackValue::String(s.clone()),
                        Value::Label(_) => unreachable!()
                    };
                    stack.push((span.clone(), v));
                }
                ExprNode::Unop(op) => {
                    let arg = get_val(stack, target);
                    let value = op.exec(&span, arg, target);
                    stack.push((span.clone(), StackValue::Number { value, origin: None }));
                }
                ExprNode::Binop(op) => {
                    let arg2 = get_val(stack, target);
                    let arg1 = get_val(stack, target);
                    let value = op.exec(&span, arg1, arg2, target);
                    stack.push((span.clone(), StackValue::Number { value, origin: None }));
                }
                ExprNode::FuncArg(f) => {
                    stack.push((span.clone(), args[*f].1.clone()));
                }
                // I really do not want to increase the indent on the builtins lol
                ExprNode::Call(len, name) if target.function(&name).is_some() => {
                    let (arity, expr) = target.function(&name).unwrap();
                    let arity = *arity;
                    if *len != arity {
                        target.push_error(span.clone(), 0, format!("Wrong number of function arguments (expected {}, found {})", arity, len));
                        return None;
                    }
                    let mut args = vec![];
                    for i in 0..*len { args.insert(0, stack.pop().unwrap()) }
                    let mut expr = expr.clone();
                    // adjust span info (otherwise errors go to the function def, which is not what
                    // you want)
                    for i in expr.nodes.iter_mut() {
                        println!("{} -> {} [{:?}]", i.0, span, span.parent());
                        i.0.to_child_of(span.clone());
                    }
                    println!("---");
                    expr.try_eval_stack(constexpr, target, asm, stack, &args[..], label_depth)?;
                }
                ExprNode::Call(len, name) => {
                    // do builtins
                    macro_rules! arity {
                        (@l $l:expr; $($dummy:expr),*) => {
                            if $l != *len {
                                target.push_error(span.clone(), 0, format!("Wrong number of function arguments (expected {}, found {})", $l, len));
                                return None;
                            } else {
                                [$({ $dummy; get_val(stack, target) }),*]
                            }
                        };
                        (1) => { arity!(@l 1; 0) };
                        (2) => { arity!(@l 2; 0, 1) };
                        (3) => { arity!(@l 3; 0, 1, 2) };
                    }
                    let number = |value| StackValue::Number { value, origin: None };
                    macro_rules! func {
                        (|$a0:ident| $b:expr) => {{ let [$a0] = arity!(1); number($b) }};
                        (|$a0:ident, $a1:ident| $b:expr) => {{ let [$a1,$a0] = arity!(2); number($b) }};
                        (|$a0:ident, $a1:ident, $a2:ident| $b:expr) => {{ let [$a2,$a1,$a0] = arity!(3); number($b) }};
                    }
                    let value = match &**name {
                        "not" => func! { |a| Unop::Not.exec(&span, a, target) },
                        "min" => func! { |a,b| a.min(b) },
                        "max" => func! { |a,b| a.max(b) },
                        "select" => func! { |cond,a,b| if cond != 0.0 { a } else { b } },
                        "datasize" => {
                            let label_name = stack.pop().expect("unbalanced expr");
                            if let StackValue::Number { origin: Some(c), .. } = label_name.1 {
                                number(asm.get_datasize(c, span.clone(), target).expect("no label for datasize?"))
                            } else {
                                target.push_error(span.clone(), 0, "This function expects a symbol, not an expression".into());
                                return None;
                            }
                        },
                        "hex" => {
                            let [arg1] = arity!(1);
                            StackValue::String(format!("{:X}", arg1 as i64))
                        },
                        "dec" => {
                            let [arg1] = arity!(1);
                            StackValue::String(format!("{}", arg1 as i64))
                        },
                        "bin" => {
                            let [arg1] = arity!(1);
                            StackValue::String(format!("{:b}", arg1 as i64))
                        },
                        "double" => {
                            let [arg1] = arity!(1);
                            StackValue::String(format!("{}", arg1))
                        },
                        _ => {
                            target.push_error(span.clone(), 0, format!("Unknown function"));
                            return None;
                        }
                    };
                    stack.push((span.clone(), value));
                }
                _ => panic!()
            }
        }
        Some(())
    }
    pub fn try_eval(&self, constexpr: bool, target: &mut Target, asm: &Assembler) -> Option<(ContextStr, StackValue)> {
        let mut stack = vec![];
        self.try_eval_stack(constexpr, target, asm, &mut stack, &[], &mut vec![])?;
        let val = stack.pop().expect("unbalanced expr");
        //println!("value: {:?}", val);
        Some(val)
    }
    pub fn try_eval_float(&self, target: &mut Target, asm: &Assembler) -> Option<f64> {
        let val = self.try_eval(false, target,asm)?;
        match val.1 {
            StackValue::Number { value, .. } => Some(value),
            _ => {
                target.push_error(val.0, 0, "Expected an expression that returns a number".into());
                None
            }
        }
    }
    pub fn try_eval_int(&self, target: &mut Target, asm: &Assembler) -> Option<u32> {
        // see asar's getnum
        let val = self.try_eval_float(target, asm)?;
        Some(int_cast(val))
    }
    pub fn eval_const(&self, target: &mut Target) -> f64 {
        let val = self.try_eval(true, target, &Assembler::new());
        match val {
            Some((_, StackValue::Number { value, .. })) => value,
            Some((c,_)) => {
                target.push_error(c, 0, "Expected an expression that returns a number".into());
                0.0
            },
            None => 0.0
        }
    }
    pub fn is_empty(&self) -> bool {
        self.nodes.len() == 0
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

