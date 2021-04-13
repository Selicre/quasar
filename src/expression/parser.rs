use super::*;

pub fn parse_literal(tokens: &mut TokenList<'_>, target: &mut Target) -> Option<Node> {
    let mut peek = tokens.clone();
    let t = peek.next_non_wsp()?;
    let t = match t.kind {
        TokenKind::Number { value, radix, length } => {
            let val = ExprNode::Value(Value::Literal {
                value: value as f64,
                size_hint: match radix {
                    16 => (length+1) / 2,
                    2 => (length+7) / 8,
                    _ => if value >= 65536 { 3 } else if value >= 256 { 2 } else { 1 },
                }
            });
            (t.span.clone(), val)
        }
        TokenKind::String => {
            let exp = crate::lexer::expand_str(t.span.clone(), target)?;
            let exp = crate::lexer::display_str(&exp);
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
    let invoke = if has_macro { Some(target.macro_invoke().expect("macro label outside of macro")) } else { None };
    let t = match t.kind {
        TokenKind::Ident => {
            let id = Label::Named { stack: vec![t.span.to_string()], invoke };
            (t.span.clone(), id)
        }
        TokenKind::Symbol => {
            match t.span.chars().next().expect("Symbol with no chars?") {
                '?' if !has_macro => {
                    *tokens = peek;
                    return parse_label(tokens, true, target);
                },
                '.' => {
                    let mut depth = t.span.len();
                    let c = peek.next();
                    if let Some(c) = c {
                        if c.is_ident() || c.is_decimal() { // grumble grumble
                            let mut label = Label::Named { stack: target.resolve_sub(depth, c.span.clone()), invoke };
                            (c.span.clone(), label)
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                },
                '+' => {
                    let mut depth = t.span.len();
                    (t.span.clone(), Label::AnonPos { depth, pos: target.resolve_anon(depth, true, t.span.clone()), invoke })
                },
                '-' => {
                    let mut depth = t.span.len();
                    (t.span.clone(), Label::AnonNeg { depth, pos: target.resolve_anon(depth, false, t.span.clone()), invoke })
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
                "#" => Unop::Unp,
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
                "=" => Eq,
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
            f.pad(&"  ".repeat(level+1))?;
            i.fmt_inner(f, level+1)?;
        }
        if self.children.len() > 0 { f.pad(")")?; }
        Ok(())
    }
}
impl std::fmt::Display for S {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.fmt_inner(f, 0)?;
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
    pub fn parse(tokens: &mut TokenList<'_>, target: &mut Target) -> Self {
        let s = parse_expr(tokens, 0, target);
        let mut nodes = Vec::with_capacity(4);
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
                    if let Some(Label::Named { stack, .. }) = target.label_name(c) {
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
}
