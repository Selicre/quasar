use super::*;

pub(super) fn exec_cmd(mut tokens: &[Token], newline: bool, target: &mut Target, ctx: &mut ExecCtx, asm: &mut Assembler) {
    /*
    print!("{} ", if ctx.enabled() { "+" } else { " " });
    for i in tokens.iter() {
        print!("{}", i.span);
    }
    println!();
    */

    let mut tokens = TokenList::new(tokens);

    loop {
        let mut peek = tokens.clone();
        // Parse labels
        if let Some(c) = expression::parse_label(&mut peek, false, target) {
            let ate_colon = peek.peek().map(|n| &*n.span == ":").unwrap_or(false);
            if ate_colon { peek.next(); }
            if c.1.no_colon() || ate_colon {
                tokens = peek;
                let id = target.set_label(c.1, c.0.clone());
                asm.append(Statement::label(id, c.0), target);
                continue;
            }
        }
        break;
    }

    let if_inline = ctx.if_inline;
    if ctx.if_inline && newline {
        ctx.if_inline = false;
        ctx.run_endif();
    }
    let cmd = if let Some(c) = tokens.next_non_wsp() { c } else { return; };

    if matches!(tokens.peek_non_wsp(), Some(c) if &*c.span == "=") {
        tokens.next_non_wsp();
        let id = target.label_id(Label::Named { stack: vec![cmd.span.to_string()] });
        let expr = Expression::parse(&mut tokens, target);
        asm.set_label(id, expr, cmd.span.clone(), target);
    }

    if cmd.is_ident() {
        match &*cmd.span {
            "if" => {
                if let Some(_) = tokens.peek_non_wsp() {
                    if ctx.enabled() {
                        let expr = Expression::parse(&mut tokens, target);
                        let val = expr.eval_const(target);
                        if val == 0.0 {
                            ctx.if_depth = Some(ctx.if_stack.len());
                        }
                    }
                    ctx.if_inline = !newline;
                    ctx.if_stack.push(None);
                } else {
                    target.push_msg(Message::error(cmd.span.clone(), 13, format!("`if` statement with no condition"))
                        .with_help(format!("add a condition, like `if 2+2 == 4`")));
                    return;
                }
            }
            "while" => {
                if let Some(c) = tokens.peek_non_wsp() {
                    if ctx.enabled() {
                        let expr = Expression::parse(&mut tokens, target);
                        let val = expr.eval_const(target);
                        println!("while cond: {}", val);
                        if val == 0.0 {
                            ctx.if_depth = Some(ctx.if_stack.len());
                        }
                    }
                    ctx.if_inline = !newline;
                    ctx.if_stack.push(Some(ctx.exec_ptr));
                } else {
                    target.push_msg(Message::error(cmd.span.clone(), 16, format!("`while` statement with no condition"))
                        .with_help(format!("add a condition, like `while 2+2 == 4`")));
                    return;
                }
            }
            "elseif" => {
                if if_inline {
                    target.push_msg(Message::error(cmd.span.clone(), 19, format!("`elseif` within inline `if`"))
                        .with_help(format!("inline `if` statements can't use `elseif`"))
                        .with_help(format!("remove the `elseif`")));
                    return;
                }
                if ctx.if_stack.len() == 0 {
                    target.push_msg(Message::error(cmd.span.clone(), 20, format!("Stray `elseif`"))
                        .with_help(format!("change `elseif` to `if`")));
                    return;
                }
                // Are we in a while loop?
                if ctx.if_stack.last().unwrap().is_some() {
                    target.push_msg(Message::error(cmd.span.clone(), 21, format!("`elseif` after a `while` loop"))
                        .with_help(format!("remove `elseif`")));
                    return;
                }
                if let Some(_) = tokens.peek_non_wsp() {
                    if ctx.enabled() {
                        ctx.if_branch_done = true;
                    }
                    if ctx.if_branch_done {
                        ctx.if_depth = Some(ctx.if_stack.len());
                    } else {
                        let expr = Expression::parse(&mut tokens, target);
                        let val = expr.eval_const(target);
                        if val == 0.0 {
                            ctx.if_depth = Some(ctx.if_stack.len());
                        } else {
                            ctx.if_depth = None;
                        }
                    }
                } else {
                    target.push_msg(Message::error(cmd.span.clone(), 13, format!("`elseif` statement with no condition"))
                        .with_help(format!("add a condition, like `if 2+2 == 4`")));
                    return;
                }
            }
            "else" => {
                if if_inline {
                    target.push_msg(Message::error(cmd.span.clone(), 22, format!("`else` within inline `if`"))
                        .with_help(format!("inline `if` statements can't use `else`"))
                        .with_help(format!("remove the `else`")));
                    return;
                }
                if ctx.if_stack.len() == 0 {
                    target.push_msg(Message::error(cmd.span.clone(), 23, format!("Stray `else`"))
                        .with_help(format!("remove `else`")));
                    return;
                }
                // Are we in a while loop?
                if ctx.if_stack.last().unwrap().is_some() {
                    target.push_msg(Message::error(cmd.span.clone(), 24, format!("`else` after a `while` loop"))
                        .with_help(format!("remove `else`")));
                    return;
                }
                if ctx.enabled() {
                    ctx.if_branch_done = true;
                }
                if ctx.if_branch_done {
                    ctx.if_depth = Some(ctx.if_stack.len());
                } else {
                    ctx.if_depth = None;
                }
            }
            "endif" => {
                if ctx.enabled() {
                    // reset the if branch state
                    ctx.if_branch_done = false;
                }
                if if_inline {
                    target.push_msg(Message::error(cmd.span.clone(), 14, format!("`endif` within inline `if`"))
                        .with_help(format!("inline `if` statements have an implicit `endif` at the end"))
                        .with_help(format!("remove the `endif`")));
                    return;
                }
                if ctx.if_stack.len() == 0 {
                    target.push_msg(Message::error(cmd.span.clone(), 15, format!("Stray `endif`"))
                        .with_help(format!("remove the `endif` or put an appropriate `if` statement above it")));
                    return;
                }
                ctx.run_endif();
            }
            "function" if ctx.enabled() => {
                let name = if let Some(n) = tokens.next_non_wsp() { n } else {
                    target.push_msg(Message::error(cmd.span.clone(), 0, format!("`function` statement with no function name"))
                        .with_help(format!("add a function name and body, like `function test(a) = a+2`")));
                    return;
                };
                let paren = tokens.next_non_wsp().unwrap(); // TODO: handle later
                assert_eq!(&*paren.span, "(");
                let mut args = vec![];
                if &*tokens.peek_non_wsp().unwrap().span != ")" { loop {
                    let arg = tokens.next_non_wsp().unwrap();
                    if !arg.is_ident() { panic!(); }
                    args.push(arg.span.to_string());
                    match tokens.next_non_wsp() {
                        Some(c) if &*c.span == "," => continue,
                        Some(c) if &*c.span == ")" => break,
                        None => panic!(),
                        Some(c) => {
                            target.push_msg(Message::error(c.span.clone(), 0, format!("Unknown separator")));
                            return;
                        }
                    }
                } } else { tokens.next_non_wsp(); }
                let eq = tokens.next_non_wsp().unwrap();
                assert_eq!(&*eq.span, "=");
                let expr = Expression::parse_fn_body(&args, &mut tokens, target);
                target.add_function(name.span.to_string(), args.len(), expr);
            }
            "rep" if ctx.enabled() => {
                if tokens.peek_non_wsp().is_none() {
                    target.push_msg(Message::error(cmd.span.clone(), 17, format!("`repeat` statement with no loop count"))
                        .with_help(format!("add a loop count, like `repeat 5`")));
                    return;
                }
                let expr = Expression::parse(&mut tokens, target);
                let mut count = expr.eval_const(target);
                if count < 0.0 { count = 0.0; }
                ctx.rep_count = Some(count as usize);
            }
            "incsrc" if ctx.enabled() => {
                let temp;
                let c = if let Some(c) = tokens.peek_non_wsp() {
                    if c.is_string() {
                        temp = if let Some(c) = lexer::expand_str(c.span.clone(), target) { c } else { return; };
                        &temp[1..temp.len()-1]
                    } else {
                        temp = tokens.rest().iter().map(|c| &*c.span).collect::<Vec<_>>().concat();
                        temp.trim()
                    }
                } else {
                    ""
                };
                exec_file(c.into(), cmd.span.clone(), target, asm);
            }
            "db" | "dw" | "dl" | "dd" if ctx.enabled() => {
                let size = match &*cmd.span { "db" => 1, "dw" => 2, "dl" => 3, "dd" => 4, _ => 0 };
                loop {
                    if tokens.peek_non_wsp().map(|c| c.is_string()).unwrap_or(false) {
                        let s = tokens.next_non_wsp().unwrap();
                        let c = lexer::expand_str(s.span.clone(), target).unwrap_or("".into());
                        let c = lexer::display_str(&c);
                        let stmt = Statement::data_str(c, size, cmd.span.clone());
                        asm.append(stmt, target);
                    } else {
                        let c = Expression::parse(&mut tokens, target);
                        if c.is_empty() { break; }
                        let stmt = Statement::data(c, size, cmd.span.clone());
                        asm.append(stmt, target);
                    }
                    match tokens.next_non_wsp() {
                        Some(c) if &*c.span == "," => continue,
                        None => break,
                        Some(c) => {
                            target.push_msg(Message::error(c.span.clone(), 0, format!("Unknown separator"))); break; 
                        }
                    }
                }
            }
            "org" if ctx.enabled() => {
                let c = Expression::parse(&mut tokens, target);
                asm.new_segment(cmd.span.clone(), crate::assembler::StartKind::Expression(c), target);
            }
            "print" if ctx.enabled() => {
                //let c = tokens.next_non_wsp().map(|c| c.span.to_string()).unwrap_or("".into());
                //target.push_msg(Message::info(cmd.span.clone(), 0, c))
                let c = Expression::parse(&mut tokens, target);
                let stmt = Statement::print(c, cmd.span.clone());
                asm.append(stmt, target);
            }
            "expr" if ctx.enabled() => {
                let c = Expression::parse(&mut tokens, target);
                target.push_msg(Message::info(cmd.span.clone(), 0, format!("{:?}", c)))
            }
            instr if instr.len() == 3 && ctx.enabled() => {
                if let Some(stmt) = crate::instruction::parse(cmd, &mut tokens, target) {
                    asm.append(stmt, target);
                }
            }
            _ => {}
        }
    }
}
