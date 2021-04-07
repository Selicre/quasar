use super::*;
pub(super) fn exec_stmt(
    i: ContextStr,
    newline: bool,
    target: &mut Target,
    ctx: &mut ExecCtx,
    macro_args: &HashMap<String, Vec<Token>>,
    asm: &mut Assembler
) {
    //println!("executing: {} at {}", i, i.source().short());
    let mut tokens = lexer::tokenize_stmt(i.clone(), target, target.cur_macro.is_some() || macro_args.len() > 0);

    if tokens.len() == 0 { return; }

    // macro support is really stupid!
    if &*tokens[0].span == "macro" && ctx.enabled() {
        if target.cur_macro.is_some() {
            target.push_error(tokens[0].span.clone(), 0, format!("Nested macro definition"));
            return;
        }
        let mut tokens = TokenList::new(&tokens);
        let _ = tokens.next_non_wsp();
        let name = tokens.next_non_wsp().unwrap();
        if !name.is_ident() { panic!(); }
        let args = parse_func_args(&mut tokens, target);
        target.start_macro(name.span.to_string(), args);
        return;
    }
    if let Some((_, c)) = &mut target.cur_macro {
        if &*tokens[0].span == "endmacro" {
            target.finish_macro();
            return;
        }
        c.blocks.push((i, newline));
        return;
    }

    // Parse setting defines as special syntax
    // TODO: make this check not horrible
    if matches!(tokens.as_slice(),
            [def,ws1,op,ws2,value,..]
            if def.is_define()
            && ws1.is_whitespace()
            && op.is_symbol()
            && ws2.is_whitespace()) {
        // mostly for endifs to work properly
        exec_cmd(TokenList::new(&[]), newline, target, ctx, asm);
        if !ctx.enabled() { return; }
        let def = &tokens[0];
        let op = &tokens[2];

        let mut exp_defines = false;
        let mut check_existing = false;
        let mut append = false;
        let mut do_math = false;
        match &*op.span {
            "=" => {},
            ":=" => exp_defines = true,
            "?=" => check_existing = true,
            "+=" => append = true,
            "#=" => { do_math = true; exp_defines = true; },
            _ => {
                target.push_error(op.span.clone(), 9, format!("Unknown define operator"));
                return;
            }
        }
        let mut v;
        if let Some(value) = tokens[4].as_string() {
            let ctx = ContextStr::new(value[1..value.len()-1].to_string(), LineInfo::custom(format!("<define at {}>", tokens[4].span.source().short())));
            if tokens.len() > 5 {
                let mut value = i.clone();
                value.advance(tokens[5].span.start() - i.start());
                target.push_error(value, 7, format!("Trailing characters at the end of define"));
                return;
            }
            v = lexer::tokenize_stmt(ctx, target, false);
        } else {
            v = tokens[4..].to_vec();
        }

        let name = def.as_define().unwrap();
        if macro_args.len() != 0 {
            expand_macro_args(&mut v, macro_args, &i, target);
        }
        if exp_defines {
            expand_defines(&mut v, &i, target);
        }
        if do_math {
            let mut t = TokenList::new(&v);
            let expr = Expression::parse(&mut t, target);
            let value = expr.eval_const(target);
            target.defines.insert(name.to_string(), vec![Token::from_number(def.span.clone(), value as _)]);
            return;
        }

        let name = if def.is_define_escaped().unwrap() {
            if let Some(c) = lexer::expand_str(name, target) { c } else { return; }
        } else {
            name.to_string()
        };
        if append {
            let entry = target.defines.entry(name).or_insert(vec![]);
            entry.append(&mut v);
        } else if check_existing {
            if !target.defines.contains_key(&name) {
                target.defines.insert(name, v);
            }
        } else {
            target.defines.insert(name, v);
        }

        return;
    }
    let mut expanded = false;
    if macro_args.len() != 0 {
        expanded |= expand_macro_args(&mut tokens, macro_args, &i, target);
        if expanded {
            glue_tokens(&mut tokens);
        }
    }
    expanded |= expand_defines(&mut tokens, &i, target);
    if expanded {
        glue_tokens(&mut tokens);

        let mut tokens = TokenList::new(&tokens);
        // Split statements and process them separately
        while let Some(part) = tokens.split_off() {
            exec_cmd(part, false, target, ctx, asm);
        }
        exec_cmd(tokens, newline, target, ctx, asm);
    } else {
        let tokens = TokenList::new(&tokens);
        exec_cmd(tokens, newline, target, ctx, asm);
    }
}

fn expand_macro_args(
    mut tokens: &mut Vec<Token>,
    macro_args: &HashMap<String, Vec<Token>>,
    line: &ContextStr, target: &mut Target
) -> bool {
    let mut expanded = false;
    while let Some(c) = tokens.iter().position(|c| c.is_macro_arg()) {
        let def = tokens[c].as_macro_arg().unwrap();
        if let Some(value) = macro_args.get(&*def) {
            expanded = true;
            let mut value = value.clone();
            value.iter_mut().for_each(|c| c.span.set_parent(def.clone()));
            tokens.splice(c..c+1, value);
        }
    }
    expanded
}

fn glue_tokens(tokens: &mut Vec<Token>) {
    // Glue tokens
    // TODO: this is kinda really hacky, but if you do this you do not deserve good diagnostics
    let mut cur = 0;
    while cur < tokens.len() {
        if !(tokens[cur].is_ident() || (tokens[cur].is_define() && !tokens[cur].is_define_escaped().unwrap())) {
            cur += 1;
            continue;
        }

        let mut first = true;
        let t = tokens[cur..].iter().take_while(|c| {
            std::mem::take(&mut first) || c.is_ident() || c.is_decimal()
        }).cloned().collect::<Vec<_>>();
        if t.len() > 1 {
            let mut new_token = String::new();
            let mut locations = String::new();
            for i in t.iter() {
                new_token.push_str(&i.span);
                locations.push_str(&format!("{}, ", i.span.source().short()));
            }
            locations.pop();
            locations.pop();
            let mut span = ContextStr::new(new_token, LineInfo::custom(format!("<glued from {}>", locations)));
            span.set_parent(t.first().unwrap().span.clone());
            let new_token = Token {
                span,
                kind: tokens[cur].kind.clone()
            };
            tokens.splice(cur..cur+t.len(), std::iter::once(new_token));
        }
        cur += 1;
    }
}


pub(super) fn exec_cmd(mut tokens: TokenList, newline: bool, target: &mut Target, ctx: &mut ExecCtx, asm: &mut Assembler) {
    if true {
        print!("{} ", if ctx.enabled() { "+" } else { " " });
        for i in tokens.rest().iter() {
            print!("{}", i.span);
        }
        println!(" {:?}", ctx.if_stack);
    }

    loop {
        let mut peek = tokens.clone();
        // Parse labels
        if let Some(c) = expression::parse_label(&mut peek, false, target) {
            let ate_colon = peek.peek().map(|n| n.span.eq(":")).unwrap_or(false);
            if ate_colon { peek.next(); }
            if c.1.no_colon() || ate_colon {
                tokens = peek;
                if ctx.enabled() {
                    let id = target.set_label(c.1, c.0.clone());
                    asm.append(Statement::label(id, c.0), target);
                }
                continue;
            }
        }
        break;
    }

    let rep = ctx.rep_count.take().unwrap_or(1);
    for c in 0..rep {
        if ctx.enabled() {
            exec_enabled(tokens.clone(), newline, target, ctx, asm);
        } else {
            exec_disabled(tokens.clone(), newline, target, ctx, asm);
        }
    }
    if ctx.if_inline && newline {
        ctx.if_inline = false;
        ctx.run_endif();
    }
}
fn exec_disabled(mut tokens: TokenList, newline: bool, target: &mut Target, ctx: &mut ExecCtx, asm: &mut Assembler) {
    // todo: this sucks balls
    let cmd = if let Some(c) = tokens.next_non_wsp() { c } else { return; };
    match &*cmd.span.to_lowercase() {
        "if" => ctx.enter_skip(),
        "while" => ctx.enter_skip(),
        "elseif" if !ctx.skipping() => {
            if ctx.if_inline {
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
            if ctx.if_stack.last().unwrap().is_while() {
                target.push_msg(Message::error(cmd.span.clone(), 21, format!("`elseif` after a `while` loop"))
                    .with_help(format!("remove `elseif`")));
                return;
            }
            if let Some(_) = tokens.peek_non_wsp() {
                let expr = Expression::parse(&mut tokens, target);
                let val = expr.eval_const(target);
                ctx.enter_elseif(val != 0.0);
            } else {
                target.push_msg(Message::error(cmd.span.clone(), 13, format!("`elseif` statement with no condition"))
                    .with_help(format!("add a condition, like `if 2+2 == 4`")));
                return;
            }
        }
        "else" if !ctx.skipping() => {
            if ctx.if_inline {
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
            if ctx.if_stack.last().unwrap().is_while() {
                target.push_msg(Message::error(cmd.span.clone(), 24, format!("`else` after a `while` loop"))
                    .with_help(format!("remove `else`")));
                return;
            }
            ctx.enter_elseif(true);
        }
        "endif" => {
            if ctx.if_inline {
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
        _ => {}
    }
}

fn exec_enabled(mut tokens: TokenList, newline: bool, target: &mut Target, ctx: &mut ExecCtx, asm: &mut Assembler) {
    let cmd = if let Some(c) = tokens.next_non_wsp() { c } else { return; };

    if matches!(tokens.peek_non_wsp(), Some(c) if &*c.span == "=") {
        tokens.next_non_wsp();
        let id = target.label_id(Label::Named { stack: vec![cmd.span.to_string()], invoke: None });
        let expr = Expression::parse(&mut tokens, target);
        asm.set_label(id, expr, cmd.span.clone(), target);
        return;
    }
    match &*cmd.span.to_lowercase() {
        "if" => {
            if let Some(_) = tokens.peek_non_wsp() {
                let expr = Expression::parse(&mut tokens, target);
                let val = expr.eval_const(target);
                ctx.enter_if(val != 0.0);
                ctx.if_inline = !newline;
            } else {
                target.push_msg(Message::error(cmd.span.clone(), 13, format!("`if` statement with no condition"))
                    .with_help(format!("add a condition, like `if 2+2 == 4`")));
                return;
            }
        }
        "while" => {
            if let Some(c) = tokens.peek_non_wsp() {
                let expr = Expression::parse(&mut tokens, target);
                let val = expr.eval_const(target);
                ctx.enter_while(val != 0.0);
                ctx.if_inline = !newline;
            } else {
                target.push_msg(Message::error(cmd.span.clone(), 16, format!("`while` statement with no condition"))
                    .with_help(format!("add a condition, like `while 2+2 == 4`")));
                return;
            }
        }
        "elseif" => {
            if ctx.if_inline {
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
            if ctx.if_stack.last().unwrap().is_while() {
                target.push_msg(Message::error(cmd.span.clone(), 21, format!("`elseif` after a `while` loop"))
                    .with_help(format!("remove `elseif`")));
                return;
            }
            if let Some(_) = tokens.peek_non_wsp() {
                let expr = Expression::parse(&mut tokens, target);
                let val = expr.eval_const(target);
                ctx.enter_elseif(val != 0.0);
            } else {
                target.push_msg(Message::error(cmd.span.clone(), 13, format!("`elseif` statement with no condition"))
                    .with_help(format!("add a condition, like `if 2+2 == 4`")));
                return;
            }
        }
        "else" => {
            if ctx.if_inline {
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
            if ctx.if_stack.last().unwrap().is_while() {
                target.push_msg(Message::error(cmd.span.clone(), 24, format!("`else` after a `while` loop"))
                    .with_help(format!("remove `else`")));
                return;
            }
            ctx.enter_elseif(true);
        }
        "endif" => {
            if ctx.if_inline {
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
        "%" => {
            let name = if let Some(n) = tokens.next_non_wsp() { n } else {
                target.push_msg(Message::error(cmd.span.clone(), 0, format!("macro call with no name"))
                    .with_help(format!("add a macro name, like `%test()`")));
                return;
            };
            if !tokens.next_non_wsp().unwrap().span.eq("(") { panic!(); }
            let mut macro_args = vec![vec![]];
            loop {
                let t = tokens.next_non_wsp().unwrap();
                if t.is_string() {
                    if macro_args.last_mut().unwrap().len() == 0 {
                        // TODO: expand macro args too
                        let l = if let Some(c) = lexer::expand_str(t.span.clone(), target) { c } else { return; };
                        let span = ContextStr::new(lexer::display_str(&l), LineInfo::custom(format!("<string at {}>", t.span.source().short())));
                        let mut out = lexer::tokenize_stmt(span, target, false);
                        macro_args.last_mut().unwrap().append(&mut out);
                    }
                } else if t.span.eq(",") {
                    macro_args.push(vec![]);
                } else if t.span.eq(")") {
                    break;
                } else {
                    macro_args.last_mut().unwrap().push(t.clone());
                }
            }
            exec_macro(&name.span, macro_args, cmd.span.clone(), target, asm);
        }
        "function" => {
            let name = if let Some(n) = tokens.next_non_wsp() { n } else {
                target.push_msg(Message::error(cmd.span.clone(), 0, format!("`function` statement with no function name"))
                    .with_help(format!("add a function name and body, like `function test(a) = a+2`")));
                return;
            };
            let args = parse_func_args(&mut tokens, target);
            let eq = tokens.next_non_wsp().unwrap();
            assert_eq!(&*eq.span, "=");
            let expr = Expression::parse_fn_body(&args, &mut tokens, target);
            target.add_function(name.span.to_string(), args.len(), expr);
        }
        "rep" if !tokens.peek_non_wsp().map(|c| c.span.eq("#")).unwrap_or(true) => {
            if tokens.peek_non_wsp().is_none() {
                target.push_msg(Message::error(cmd.span.clone(), 17, format!("`repeat` statement with no loop count"))
                    .with_help(format!("add a loop count, like `repeat 5`")));
                return;
            }
            let expr = Expression::parse(&mut tokens, target);
            let mut count = expr.eval_const(target);
            println!("rep expr: {:?}", expr);
            if count < 0.0 { count = 0.0; }
            ctx.rep_count = Some(count as usize);
        }
        "skip" => {
            if tokens.peek_non_wsp().is_none() {
                target.push_msg(Message::error(cmd.span.clone(), 17, format!("`skip` statement with no skip count"))
                    .with_help(format!("add a skip count, like `skip 5`")));
                return;
            }
            let expr = Expression::parse(&mut tokens, target);
            let mut count = expr.eval_const(target);
            if count != 0.0 {
                let stmt = Statement::skip(count as usize, cmd.span.clone());
                asm.append(stmt, target);
            }
        }
        "incsrc" => {
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
        "db" | "dw" | "dl" | "dd" => {
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
        "org" => {
            let c = Expression::parse(&mut tokens, target);
            asm.new_segment(cmd.span.clone(), crate::assembler::StartKind::Expression(c), target);
        }
        "incbin" => {
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
            let stmt = Statement::binary(std::fs::read(c).unwrap(), cmd.span.clone());
            asm.append(stmt, target);
        }
        "pad" => {
            let c = Expression::parse(&mut tokens, target);
            asm.new_segment(cmd.span.clone(), crate::assembler::StartKind::Expression(c), target);
        }
        "base" => {
            let val = if tokens.peek_non_wsp().unwrap().span.eq("off") {
                None
            } else {
                let expr = Expression::parse(&mut tokens, target);
                let mut base = expr.eval_const(target) as usize;
                Some(base)
            };
            let stmt = Statement::base(val, cmd.span.clone());
            asm.append(stmt, target);
        }
        "print" => {
            //let c = tokens.next_non_wsp().map(|c| c.span.to_string()).unwrap_or("".into());
            //target.push_msg(Message::info(cmd.span.clone(), 0, c))
            let c = Expression::parse(&mut tokens, target);
            let stmt = Statement::print(c, cmd.span.clone());
            asm.append(stmt, target);
        }
        "expr" => {
            let c = Expression::parse(&mut tokens, target);
            target.push_msg(Message::info(cmd.span.clone(), 0, format!("{:?}", c)))
        }
        instr if instr.len() == 3 => {
            if let Some(stmt) = crate::instruction::parse(cmd, &mut tokens, target) {
                asm.append(stmt, target);
            }
        }
        c => println!("unknown command {}", c)
    }
}
fn parse_func_args(tokens: &mut TokenList, target: &mut Target) -> Vec<String> {
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
                return vec![];
            }
        }
    } } else { tokens.next_non_wsp(); }
    args
}
