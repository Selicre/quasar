use crate::statement::Statement;
use crate::executor::{TokenList, Target};
use crate::expr::Expression;
use crate::lexer::Token;
use crate::message::Message;

include!(concat!(env!("OUT_DIR"), "/codegen.rs"));

pub enum AddressingMode {
    Implied,
    Immediate,
    DirectPage,
    DpX,
    DpY,
    DpInd,
    DpIndX,
    DpIndY,
    DpIndLong,
    DpIndLongY,
    Stack,
    StackY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    AbsInd,
    AbsIndX,
    AbsIndLong,
    AbsLong,
    AbsLongX,
    Relative,
    RelativeWord,
    BlockMove
}

#[derive(Debug)]
pub enum ArgumentKind {
    Immediate,
    Direct,
    IndexedX,
    IndexedY,
    Indirect,
    IndX,
    IndY,
    IndLong,
    IndLongY,
    Stack,
    StackY
}
impl ArgumentKind {
    fn to_addr_mode(&self, size: usize) -> Option<AddressingMode> {
        use AddressingMode::*;
        Some(match (self, size) {
            (Self::Immediate, 1) => Immediate,
            (Self::Immediate, 2) => Immediate,
            (Self::Direct, 1)    => DirectPage,
            (Self::IndexedX, 1)  => DpX,
            (Self::IndexedY, 1)  => DpY,
            (Self::Indirect, 1)  => DpInd,
            (Self::IndX, 1)      => DpIndX,
            (Self::IndY, 1)      => DpIndY,
            (Self::IndLong, 1)   => DpIndLong,
            (Self::IndLongY, 1)  => DpIndLongY,
            (Self::Stack, 1)     => Stack,
            (Self::StackY, 1)    => StackY,
            (Self::Direct, 2)    => Absolute,
            (Self::IndexedX, 2)  => AbsoluteX,
            (Self::IndexedY, 2)  => AbsoluteY,
            (Self::Indirect, 2)  => AbsInd,
            (Self::IndX, 2)      => AbsIndX,
            (Self::IndLong, 2)   => AbsIndLong,
            (Self::Direct, 3)    => AbsLong,
            (Self::IndexedX, 3)  => AbsLongX,
            _ => return None
        })
    }
}

pub fn parse(instr: &Token, tokens: &mut TokenList<'_>, target: &mut Target) -> Option<Statement> {
    let mut size = None;

    match tokens.peek_non_wsp() {
        None => {
            if instr.span.eq_ignore_ascii_case("brk")
            || instr.span.eq_ignore_ascii_case("cop")
            || instr.span.eq_ignore_ascii_case("wdm") {
                return try_opcode(instr.span.as_bytes(), AddressingMode::Immediate).map(|opcode| {
                    Statement::instruction(opcode, Expression::value(instr.span.clone(), 0.0), 2, instr.span.clone())
                });
            }
            return try_opcode(instr.span.as_bytes(), AddressingMode::Implied).map(|opcode| {
                Statement::instruction(opcode, Expression::empty(), 1, instr.span.clone())
            });
        },
        // support for `inc a` and the like
        Some(c) if &*c.span == "a" || &*c.span == "A" => {
            if let Some(opcode) = try_opcode(instr.span.as_bytes(), AddressingMode::Implied) {
                return Some(Statement::instruction(opcode, Expression::empty(), 1, instr.span.clone()))
            }
        },
        _ => {}
    }

    let mut size_set = false;
    // parse sizing
    if tokens.peek_non_wsp().map(|c| &*c.span) == Some(".") {
        let dot = tokens.next_non_wsp().unwrap();
        let s = tokens.next_non_wsp();
        size_set = true;
        size = Some(match s.map(|c| &*c.span) {
            Some("b") => 1,
            Some("w") => 2,
            Some("l") => 3,
            _ => {
                target.push_error(s.unwrap_or(dot).span.clone(), 0, "Unknown size hint".into());
                return None;
            }
        });
    }

    if instr.span.eq_ignore_ascii_case("mvn")
    || instr.span.eq_ignore_ascii_case("mvp") {
        if size.is_some() { panic!("TODO: proper message"); }
        let expr1 = Expression::parse(tokens, target);
        let comma = match tokens.next_non_wsp() {
            Some(c) if &*c.span == "," => c,
            Some(c) => {
                target.push_error(c.span.clone(), 0, "Unexpected token".into());
                return None;
            }
            None => {
                return try_opcode(instr.span.as_bytes(), AddressingMode::BlockMove).map(|opcode| {
                    Statement::instruction(opcode, expr1, 3, instr.span.clone())
                });
            }
        };
        let expr2 = Expression::parse(tokens, target);
        let expr = Expression::block_move(comma.span.clone(), expr1, expr2);
        return try_opcode(instr.span.as_bytes(), AddressingMode::BlockMove).map(|opcode| {
            Statement::instruction(opcode, expr, 3, instr.span.clone())
        });
    }

    let tokens_backup = tokens.clone();

    // parse addr mode
    let mut bracket = None;
    let b = tokens.peek_non_wsp().map(|c| &*c.span);
    if b == Some("(") || b == Some("[") || b == Some("#") {
        bracket = tokens.next_non_wsp();
    }

    let mut expr = Expression::parse(tokens, target);
    if expr.is_empty() { return None; }

    macro_rules! next_or_err {
        ($($tt:tt)*) => {{
            let cur = tokens.next_non_wsp();
            if cur.is_none() {
                target.push_error($($tt)*);
            }
            cur?
        }}
    }

    let mut arg = match bracket {
        Some(b) if &*b.span == "(" => {
            let cur = next_or_err!(b.span.clone(), 0, "Unclosed addressing mode parenthesis".into());
            match &*cur.span {
                "," => {
                    let cur = next_or_err!(b.span.clone(), 0, "Unclosed addressing mode parenthesis".into());
                    match &*cur.span {
                        "x"|"X" => {
                            let cur = next_or_err!(b.span.clone(), 0, "Unclosed addressing mode parenthesis".into());
                            if &*cur.span != ")" {
                                target.push_error(cur.span.clone(), 0, "Unexpected token".into());
                                return None;
                            }
                            ArgumentKind::IndX
                        },
                        "s"|"S" => {
                            let cur = next_or_err!(b.span.clone(), 0, "Unclosed addressing mode parenthesis".into());
                            if &*cur.span != ")" {
                                target.push_error(cur.span.clone(), 0, "Unexpected token".into());
                                return None;
                            }
                            let cur = next_or_err!(cur.span.clone(), 0, "Unexpected end of addressing mode".into());
                            if &*cur.span != "," {
                                target.push_error(cur.span.clone(), 0, "Unexpected token".into());
                                return None;
                            }
                            let cur = next_or_err!(cur.span.clone(), 0, "Unexpected end of addressing mode".into());
                            if &*cur.span != "y" && &*cur.span != "Y" {
                                target.push_msg(Message::error(cur.span.clone(), 0, "Unsupported offset register".into())
                                    .with_help(format!("Only `y` is valid in this position, like `eor ($03,s),y`")));
                                return None;
                            }
                            ArgumentKind::StackY
                        }
                        _ => {
                            target.push_msg(Message::error(cur.span.clone(), 0, "Unsupported offset register".into())
                                .with_help(format!("Only `x` is valid in this position, like `lda ($12,x)`")));
                            return None;
                        }
                    }
                },
                ")" => {
                    if let Some(cur) = tokens.peek_non_wsp() {
                        if &*cur.span != "," {
                            // for expr backtracking
                            ArgumentKind::Indirect
                        } else {
                            tokens.next_non_wsp();
                            let cur = next_or_err!(cur.span.clone(), 0, "Unexpected end of expression".into());
                            match &*cur.span {
                                "y"|"Y" => {
                                    ArgumentKind::IndY
                                },
                                _ => {
                                    target.push_msg(Message::error(cur.span.clone(), 0, "Unsupported offset register".into())
                                        .with_help(format!("Only `y` is valid in this position, like `lda ($12),y`")));
                                    return None;
                                }
                            }
                        }
                    } else {
                        ArgumentKind::Indirect
                    }
                }
                _ => {
                    target.push_error(cur.span.clone(), 0, "Unexpected token".into());
                    return None;
                },
            }
        }
        Some(b) if &*b.span == "[" => {
            let cur = next_or_err!(b.span.clone(), 0, "Unclosed addressing mode bracket".into());
            match &*cur.span {
                "," => {
                    target.push_msg(Message::error(cur.span.clone(), 0, "Unsupported offset register".into())
                        .with_help(format!("An offset register is not valid in this position; use `lda [$12]` or `lda [$12],y`")));
                    return None;
                },
                "]" => {
                    if let Some(cur) = tokens.next_non_wsp() {
                        if &*cur.span != "," {
                            target.push_error(cur.span.clone(), 0, "Unexpected token".into());
                            return None;
                        }
                        let cur = next_or_err!(cur.span.clone(), 0, "Unexpected end of expression".into());
                        match &*cur.span {
                            "y"|"Y" => {
                                ArgumentKind::IndLongY
                            },
                            _ => {
                                target.push_msg(Message::error(cur.span.clone(), 0, "Unsupported offset register".into())
                                    .with_help(format!("Only `y` is valid in this position, like `lda [$12],y`")));
                                return None;
                            }
                        }
                    } else {
                        ArgumentKind::IndLong
                    }
                }
                _ => {
                    target.push_error(cur.span.clone(), 0, "Unexpected token".into());
                    return None;
                },
            }
        }
        Some(b) if &*b.span == "#" => {
            ArgumentKind::Immediate
        }
        _ => {
            if let Some(cur) = tokens.next_non_wsp() {
                if &*cur.span != "," {
                    target.push_error(cur.span.clone(), 0, "Unexpected token".into());
                    return None;
                }
                let cur = next_or_err!(cur.span.clone(), 0, "Unexpected end of expression".into());
                match &*cur.span {
                    "x"|"X" => {
                        ArgumentKind::IndexedX
                    },
                    "y"|"Y" => {
                        ArgumentKind::IndexedY
                    },
                    "s"|"S" => {
                        ArgumentKind::Stack
                    },
                    _ => {
                        target.push_msg(Message::error(cur.span.clone(), 0, "Unsupported offset register".into())
                            .with_help(format!("Only `x`, `y` and `s` are valid in this position, like `lda $12,x`")));
                        return None;
                    }
                }
            } else {
                ArgumentKind::Direct
            }
        },
    };
    if let Some(c) = tokens.next_non_wsp() {
        // We have some garbage at the end. Try to backtrack to see if it's actually a part of an expression
        if !matches!(bracket, Some(c) if &*c.span == "(") {
            target.push_error(c.span.clone(), 0, "Unexpected token".into());
            return None;
        }
        target.push_msg(Message::warning(bracket.unwrap().span.clone(), 0, "Ambiguous addressing mode; using direct addressing".into())
            .with_help("Use the unary plus before the expression to disambiguate: `lda +($00)`".into()));
        *tokens = tokens_backup;
        expr = Expression::parse(tokens, target);
        if expr.is_empty() { return None; }
        arg = if let Some(cur) = tokens.next_non_wsp() {
            if &*cur.span != "," {
                target.push_error(cur.span.clone(), 0, "Unexpected token".into());
                return None;
            }
            let cur = next_or_err!(cur.span.clone(), 0, "Unexpected end of expression".into());
            // TODO: deduplicate this
            match &*cur.span {
                "x"|"X" => {
                    ArgumentKind::IndexedX
                },
                "y"|"Y" => {
                    ArgumentKind::IndexedY
                },
                "s"|"S" => {
                    ArgumentKind::Stack
                },
                _ => {
                    target.push_msg(Message::error(cur.span.clone(), 0, "Unsupported offset register".into())
                        .with_help(format!("Only `x`, `y` and `s` are valid in this position, like `lda $12,x`")));
                    return None;
                }
            }
        } else {
            ArgumentKind::Direct
        }
    }

    if size.is_none() {
        size = expr.size_hint();
    }

    if let Some(opcode) = try_opcode(instr.span.as_bytes(), AddressingMode::Relative) {
        //println!("{}: {}", opcode, instr.span);
        let stmt = Statement::instruction_rel(opcode, expr, 2, instr.span.clone());
        return Some(stmt)
    }
    if let Some(opcode) = try_opcode(instr.span.as_bytes(), AddressingMode::RelativeWord) {
        let stmt = Statement::instruction_rel(opcode, expr, 3, instr.span.clone());
        return Some(stmt)
    }
    if let Some(size) = size {
        let mode = arg.to_addr_mode(size);
        if mode.is_none() {
            target.push_error(instr.span.clone(), 0, "Unsupported addressing mode".into());
        }
        let opcode = try_opcode(instr.span.as_bytes(), mode?);
        if opcode.is_none() {
            if !size_set && matches!(arg, ArgumentKind::Immediate) {
                // try repeat opcode
                if let Some(opcode) = try_opcode(instr.span.as_bytes(), AddressingMode::Implied) {
                    let size = expr.eval_const(target) as usize;
                    return Some(Statement::instruction_rep(opcode, size, instr.span.clone()));
                }
            }
            target.push_error(instr.span.clone(), 0, "Unsupported addressing mode for instruction".into());
        }
        Some(Statement::instruction(opcode?, expr, size+1, instr.span.clone()))
    } else {
        for i in (1..=3).rev() {
            let mode = arg.to_addr_mode(i);
            let mode = if let Some(m) = mode { m } else { continue; };
            let opcode = try_opcode(instr.span.as_bytes(), mode);
            let opcode = if let Some(m) = opcode { m } else { continue; };
            return Some(Statement::instruction(opcode, expr, i+1, instr.span.clone()));
        }
        if matches!(arg, ArgumentKind::Immediate) {
            // try repeat opcode
            if let Some(opcode) = try_opcode(instr.span.as_bytes(), AddressingMode::Implied) {
                let size = expr.eval_const(target) as usize;
                return Some(Statement::instruction_rep(opcode, size, instr.span.clone()));
            }
        }
        target.push_error(instr.span.clone(), 0, "Unsupported instruction".into());
        None
    }
}

fn try_opcode(instr: &[u8], mode: AddressingMode) -> Option<u8> {
    let mut buf = [0; 4];
    buf[0..3].copy_from_slice(instr);
    buf.make_ascii_lowercase();
    buf[3] = mode as u8;
    OPCODES.get(&buf).cloned()
}
