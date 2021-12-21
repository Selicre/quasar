use super::*;


pub(super) fn exec_stmt(
    tokens: &[Token],
    newline: bool,
    target: &mut Target,
    ctx: &mut ExecCtx,
    asm: &mut Assembler
) {
    //println!("executing: {} at {}", i, i.source().short());
    //let mut tokens = lexer::tokenize_stmt(i.clone(), target, target.cur_macro.is_some() || macro_args.len() > 0);
    if tokens.len() == 0 { return; }

    let mut tokens = Cow::from(tokens);

    if &*tokens[0].span == "macro" && ctx.enabled() {
        if target.cur_macro.is_some() {
            errors::macro_nested_def(tokens[0].span.clone()).push();
            return;
        }
        let mut tokens = TokenList::new(&tokens);
        let macro_token = tokens.next_non_wsp().unwrap();
        let name = if let Some(n) = tokens.next_non_wsp() { n } else {
            errors::macro_def_no_name(macro_token.span.clone()).push();
            return;
        };
        if !name.is_ident() {
            errors::macro_def_no_name(name.span.clone()).push();
            return;
        }
        let args = if let Some(c) = parse_func_args(&mut tokens, target) { c } else { return; };
        target.start_macro(name.span.to_string(), args, ctx.path.clone());
        return;
    }
    if let Some((_, c)) = &mut target.cur_macro {
        if &*tokens[0].span == "endmacro" {
            target.finish_macro();
            return;
        }
        c.blocks.push((tokens.to_vec(), newline));
        return;
    }
    let mut expanded = false;
    expanded |= expand_macro_args(tokens.to_mut(), target);
    if expanded {
        glue_tokens(tokens.to_mut());
    }

    // Parse setting defines as special syntax
    // TODO: make this check not horrible
    if matches!(&tokens[..],
            [def,ws1,op,ws2,value,..]
            if def.is_define()
            && ws1.is_whitespace()
            && op.is_symbol()
            && {
                let x = &op.span;
                x.eq("=") || x.eq(":=") || x.eq("?=") || x.eq("+=") || x.eq("#=")
            }
            && ws2.is_whitespace()) {
        // mostly for endifs to work properly
        let enabled = ctx.enabled();
        exec_cmd(TokenList::new(&[]), newline, target, ctx, asm);
        if !enabled { return; }
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
                return;
            }
        }
        let mut v;
        if let Some(value) = tokens[4].as_string() {
            let ctx = ContextStr::new(value[1..value.len()-1].to_string(), LineInfo::custom(format!("<define at {}>", tokens[4].span.source().short())));
            if tokens.len() > 5 {
                //let mut value = i.clone();
                //value.advance(tokens[5].span.start() - i.start());
                errors::define_trailing_chars(tokens[5].span.clone()).push();
                return;
            }
            v = Cow::from(lexer::tokenize_stmt(ctx, target, false));
        } else {
            v = Cow::from(&tokens[4..]);
        }

        let name = def.as_define().unwrap();
        expand_macro_args(v.to_mut(), target);
        if exp_defines {
            let res = expand_defines(&mut v, &tokens[0].span, target);
            if let Err(e) = res { e.push(); return; }
        }
        if do_math {
            let mut t = TokenList::new(&v);
            let expr = Expression::parse(&mut t, target);
            let value = expr.eval_const(target);
            let span = ContextStr::new(value.to_string(), LineInfo::custom(format!("<define at {}>", tokens[4].span.source().short())));
            target.defines.insert(name.to_string(), vec![Token::from_number(span, value as _)]);
            return;
        }

        let name = if def.is_define_escaped().unwrap() {
            if let Some(c) = lexer::expand_str(name, target) { c } else { return; }
        } else {
            name.to_string()
        };
        if append {
            let entry = target.defines.entry(name).or_insert(vec![]);
            entry.append(v.to_mut());
        } else if check_existing {
            if !target.defines.contains_key(&name) {
                target.defines.insert(name, v.to_vec());
            }
        } else {
            target.defines.insert(name, v.to_vec());
        }

        return;
    }
    if tokens.len() == 0 { return; }
    let i = tokens[0].span.clone(); // stopgap measure
    expanded |= match expand_defines(&mut tokens, &i, target) {
        Ok(b) => b,
        Err(e) => if ctx.enabled() {
            e.push();
            return;
        } else { false }
    };
    if expanded {
        glue_tokens(tokens.to_mut());

        let mut tokens = TokenList::new(&tokens);
        // Split statements and process them separately
        while let Some((part, nl)) = tokens.split_off(newline) {
            exec_cmd(part, nl, target, ctx, asm);
        }
    } else {
        let tokens = TokenList::new(&tokens);
        exec_cmd(tokens, newline, target, ctx, asm);
    }
}

fn expand_macro_args(
    mut tokens: &mut Vec<Token>,
    target: &mut Target
) -> bool {
    let mut expanded = false;
    if target.cur_macro_args.len() == 0 { return false; }
    while let Some(c) = tokens.iter().position(|c| c.is_macro_arg()) {
        let def = tokens[c].as_macro_arg().unwrap();
        if let Some(value) = target.cur_macro_args.get(&*def) {
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
        // attempt to glue $<thing>
        if cur + 1 < tokens.len() && tokens[cur].span.eq("$") && (tokens[cur+1].is_ident() || tokens[cur+1].is_decimal()) {
            if let Ok(c) = i64::from_str_radix(&tokens[cur+1].span, 16) {
                let mut span = tokens[cur].span.clone();
                let new_token = Token {
                    span,
                    kind: TokenKind::Number {
                        value: c,
                        radix: 16,
                        length: tokens[cur+1].span.len()
                    }
                };
                tokens.splice(cur..cur+2, std::iter::once(new_token));
                continue;
            }
        }
        if !tokens[cur].is_ident() && !(tokens[cur].is_define() && !tokens[cur].is_define_escaped().unwrap()) {
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
    if false {
        print!("{} ", if ctx.enabled() { "+" } else { " " });
        for i in tokens.rest().iter() {
            print!("{:?} ", i.span);
        }
        print!(" {:?} ", ctx.if_stack);
        println!("{}", if newline { "nl" } else { "" });
    }

    loop {
        let mut peek = tokens.clone();
        let mut unstructured = false;
        let mut global = false;
        // Parse labels
        while let Some(c) = peek.peek_non_wsp() {
            if c.span.eq("global") {
                peek.next_non_wsp();
                global = true;
            } else if c.span.eq("#") {
                peek.next_non_wsp();
                unstructured = true;
                break;
            } else {
                break;
            }
        }
        if let Some(c) = expression::parse_label(&mut peek, false, target) {
            let ate_colon = peek.peek().map(|n| n.span.eq(":")).unwrap_or(false);
            if ate_colon { peek.next(); }
            if c.1.no_colon() || ate_colon {
                tokens = peek;
                if ctx.enabled() {
                    let id = if unstructured {
                        target.label_id(c.1, global)
                    } else {
                        target.set_label(c.1, global, c.0.clone())
                    };
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
        if !tokens.peek_non_wsp().map(|c| c.span.eq("endif")).unwrap_or(false) {
            ctx.run_endif();
        }
    }
}
fn exec_disabled(mut tokens: TokenList, newline: bool, target: &mut Target, ctx: &mut ExecCtx, asm: &mut Assembler) {
    // todo: this sucks balls
    let cmd = if let Some(c) = tokens.next_non_wsp() { c } else { return; };
    match &*cmd.span.to_lowercase() {
        "if" => ctx.enter_skip(),
        "while" => ctx.enter_skip(),
        "elseif" if !ctx.skipping() => {
            if ctx.if_stack.len() == 0 {
                errors::stray_elseif(cmd.span.clone()).push();
                return;
            }
            // Are we in a while loop?
            if ctx.if_stack.last().unwrap().is_while() {
                errors::else_after_while(cmd.span.clone(), "elseif").push();
                return;
            }
            if let Some(_) = tokens.peek_non_wsp() {
                let expr = Expression::parse(&mut tokens, target);
                let val = expr.eval_const(target);
                ctx.enter_elseif(val != 0.0);
            } else {
                errors::if_no_condition(cmd.span.clone(), "elseif").push();
                return;
            }
        }
        "else" if !ctx.skipping() => {
            if ctx.if_stack.len() == 0 {
                errors::stray_else(cmd.span.clone()).push();
                return;
            }
            // Are we in a while loop?
            if ctx.if_stack.last().unwrap().is_while() {
                errors::else_after_while(cmd.span.clone(), "else").push();
                return;
            }
            ctx.enter_elseif(true);
        }
        "endif" => {
            if ctx.if_stack.len() == 0 {
                errors::stray_endif(cmd.span.clone()).push();
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
        let id = target.label_id(Label::Named { stack: vec![cmd.span.to_string()], invoke: None }, false);
        let expr = Expression::parse(&mut tokens, target);
        asm.set_label(id, expr, cmd.span.clone(), target);
        return;
    }

    // test mnemonic
    let is_mnemonic = <_ as std::convert::TryFrom<_>>::try_from(cmd.span.as_bytes())
        .map_or(false, |mut c: [u8;3]| { c.make_ascii_lowercase(); crate::instruction::MNEMONICS.contains(&c) });
    if is_mnemonic {
        if let Some(stmt) = crate::instruction::parse(cmd, &mut tokens, target) {
            asm.append(stmt, target);
        }
        return;
    }

    match &*cmd.span.to_ascii_lowercase() {
        "if" => {
            if let Some(_) = tokens.peek_non_wsp() {
                let expr = Expression::parse(&mut tokens, target);
                let val = expr.eval_const(target);
                ctx.enter_if(val != 0.0);
                ctx.if_inline = !newline;
            } else {
                errors::if_no_condition(cmd.span.clone(), "if").push();
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
                errors::if_no_condition(cmd.span.clone(), "while").push();
                return;
            }
        }
        "elseif" => {
            if ctx.if_stack.len() == 0 {
                errors::stray_elseif(cmd.span.clone()).push();
                return;
            }
            // Are we in a while loop?
            if ctx.if_stack.last().unwrap().is_while() {
                errors::else_after_while(cmd.span.clone(), "elseif").push();
                return;
            }
            if let Some(_) = tokens.peek_non_wsp() {
                let expr = Expression::parse(&mut tokens, target);
                let val = expr.eval_const(target);
                ctx.enter_elseif(val != 0.0);
            } else {
                errors::if_no_condition(cmd.span.clone(), "elseif").push();
                return;
            }
        }
        "else" => {
            if ctx.if_stack.len() == 0 {
                errors::stray_else(cmd.span.clone()).push();
                return;
            }
            // Are we in a while loop?
            if ctx.if_stack.last().unwrap().is_while() {
                errors::else_after_while(cmd.span.clone(), "else").push();
                return;
            }
            ctx.enter_elseif(true);
        }
        "endif" => {
            if ctx.if_stack.len() == 0 {
                errors::stray_endif(cmd.span.clone()).push();
                return;
            }
            ctx.run_endif();
        }
        "%" => {
            let name = match tokens.next_non_wsp() {
                Some(n) if n.is_ident() => n,
                _ => { errors::macro_no_name(cmd.span.clone()).push(); return; }
            };
            if !tokens.next_non_wsp().unwrap().span.eq("(") {
                errors::macro_malformed(cmd.span.clone()).push(); return;
            }
            let mut first = true;
            let mut call_args = vec![vec![]];
            let mut depth = 0;
            loop {
                let t = if std::mem::take(&mut first) {
                    tokens.next_non_wsp()
                } else {
                    tokens.next()
                };
                let t = if let Some(t) = t {
                    t
                } else {
                    errors::macro_malformed(cmd.span.clone()).push(); return;
                };
                if t.is_string() {
                    if call_args.last_mut().unwrap().len() == 0 {
                        // TODO: expand macro args too
                        let l = if let Some(c) = lexer::expand_str(t.span.clone(), target) { c } else { return; };
                        let span = ContextStr::new(lexer::display_str(&l), LineInfo::custom(format!("<string at {}>", t.span.source().short())));
                        let mut out = lexer::tokenize_stmt(span, target, false);
                        call_args.last_mut().unwrap().append(&mut out);
                    }
                } else if t.span.eq("(") {
                    depth += 1;
                    call_args.last_mut().unwrap().push(t.clone());
                } else if t.span.eq(",") && depth == 0 {
                    first = true;
                    call_args.push(vec![]);
                } else if t.span.eq(")") {
                    if depth != 0 {
                        depth -= 1;
                        call_args.last_mut().unwrap().push(t.clone());
                    } else {
                        break;
                    }
                } else {
                    call_args.last_mut().unwrap().push(t.clone());
                }
            }
            exec_macro(&name.span, call_args, cmd.span.clone(), target, asm);
        }
        "function" => {
            let name = if let Some(n) = tokens.next_non_wsp() { n } else {
                errors::function_no_name(cmd.span.clone()).push();
                return;
            };
            if !name.is_ident() {
                errors::function_no_name(name.span.clone()).push();
                return;
            }
            let args = if let Some(c) = parse_func_args(&mut tokens, target) { c } else {
                return;
            };
            let eq = tokens.next_non_wsp();
            if eq.map_or(true, |c| !c.span.eq("=")) {
                errors::function_no_eq(tokens.prev().unwrap().span.clone()).push();
                return;
            }
            let expr = Expression::parse_fn_body(&args, &mut tokens, target);
            if expr.is_empty() {
                errors::function_empty(cmd.span.clone()).push();
                return;
            }
            target.add_function(name.span.to_string(), args.len(), expr);
        }
        "rep" if tokens.peek_non_wsp().map_or(false, |c| !c.span.eq("#") && !c.span.eq(".")) => {
            if tokens.peek_non_wsp().is_none() {
                errors::cmd_no_arg(cmd.span.clone(), "rep", "repeat count", "5").push();
                return;
            }
            let expr = Expression::parse(&mut tokens, target);
            let mut count = expr.eval_const(target);
            //println!("rep expr: {:?}", expr);
            if count < 0.0 { count = 0.0; }
            ctx.rep_count = Some(count as usize);
        }
        "skip" => {
            if tokens.peek_non_wsp().is_none() {
                errors::cmd_no_arg(cmd.span.clone(), "skip", "skip count", "5").push();
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
                errors::cmd_no_arg(cmd.span.clone(), "incsrc", "filename", "\"other.asm\"").push();
                return;
            };
            let mut buf = ctx.path.clone();
            buf.push(c);
            exec_file(&buf.to_string_lossy(), cmd.span.clone(), target, asm);
        }
        "db" | "dw" | "dl" | "dd" => {
            let size = match &*cmd.span { "db" => 1, "dw" => 2, "dl" => 3, "dd" => 4, _ => 0 };
            loop {
                if tokens.peek_non_wsp().map_or(false, |c| c.is_string()) {
                    let s = tokens.next_non_wsp().unwrap();
                    let c = if let Some(c) = lexer::expand_str(s.span.clone(), target) { c } else { return; };
                    let c = lexer::display_str(&c);
                    let mut map = vec![];
                    for i in c.chars() {
                        if let Some(val) = target.table.get(&i) {
                            // If table entry present, truncate that
                            map.extend_from_slice(&val.to_le_bytes()[..size]);
                        } else {
                            if size == 1 {
                                // If db, encode as utf-8
                                let mut buf = [0;4];
                                map.extend_from_slice(i.encode_utf8(&mut buf).as_bytes());
                            } else {
                                // otherwise, encode as utf-32
                                map.extend_from_slice(&(i as u32).to_le_bytes()[..size]);
                            }
                        }
                    }
                    let stmt = Statement::binary(map, cmd.span.clone());
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
                        errors::cmd_unknown_sep(c.span.clone()).push();
                        break;
                    }
                }
            }
        }
        "header" => {},
        "lorom" => {},
        "bank" => {},
        "math" => {},
        "warnings" => {},
        "pushpc" => {
            let label_idx = target.phantom_label();
            target.pc_stack.push(label_idx);
            let stmt = Statement::label(label_idx, cmd.span.clone());
            asm.append(stmt, target);
        },
        "pullpc" => {
            let label = target.pc_stack.pop().unwrap();
            let c = Expression::label(cmd.span.clone(), label);
            asm.new_segment(cmd.span.clone(), crate::assembler::StartKind::Expression(c), target);
        }
        "pushbase" => {
            let base = asm.current_base();
            if let Some(base) = base {
                target.base_stack.push(base);
            } else {
                errors::pushbase_no_base(cmd.span.clone()).push();
            }
        }
        "pullbase" => {
            let val = target.base_stack.pop().unwrap();
            println!("{:?}", val);
            let stmt = Statement::base(Some(val), cmd.span.clone());
            asm.append(stmt, target);
        }
        "table" => {
            // TODO: handle ,ltr/,rtl
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

            // TODO: handle errors
            let table = String::from_utf8(std::fs::read(c).expect("can't find file")).expect("file not utf-8");
            for i in table.lines() {
                if i.len() == 0 { continue; }
                let mut c = i.chars();
                let ch = c.next().unwrap();
                assert_eq!(c.next().unwrap(), '=');
                let value = u32::from_str_radix(c.as_str(), 16).unwrap();
                target.table.insert(ch, value);
            }
        }
        "cleartable" => {
            target.table.clear();
        }
        "pushtable" => {
            target.table_stack.push(target.table.clone());
        }
        "pulltable" => {
            target.table = target.table_stack.pop().unwrap();
        }
        "@" => {},
        "{" | "}" => {},
        "namespace" => {
            if let Some(c) = tokens.peek_non_wsp() {
                if c.span.eq("nested") {
                    if let Some(c) = tokens.peek_non_wsp() {
                        target.namespace_nested = !c.span.eq("off");
                        return;
                    }
                }
                // todo: is ident
                if target.namespace_nested {
                    target.namespace.pop();
                } else {
                    target.namespace.clear();
                }
                if !c.span.eq("off") {
                    target.namespace.push(c.span.to_string());
                }
            } else {
                errors::cmd_no_arg(cmd.span.clone(), "namespace", "namespace", "test").push();
                return;
            }
        }
        "prot" => {},
        "autoclean" => {
            if let Some(c) = tokens.peek_non_wsp() {
                if c.as_number().is_some() {
                    return;
                }
            }
            // TODO: actually clean shit
            exec_enabled(tokens, newline, target, ctx, asm);
        }
        "freespace" | "freecode" | "freedata" => {
            let mut ram = None;
            let mut align = false;
            let mut cleaned = false;
            let mut static_ = false;
            let mut value = None;
            while let Some(c) = tokens.next_non_wsp() {
                match &*c.span {
                    "," => {},
                    "ram" => ram = Some(true),
                    "noram" => ram = Some(false),
                    "align" => align = true,
                    "cleaned" => cleaned = true,
                    "static" => static_ = true,
                    _ => {
                        let expr = Expression::parse(&mut tokens, target);
                        value = Some(expr.eval_const(target));
                        break;
                    }
                }
            }
            // suppress warnings
            let _ = (ram, cleaned, static_, value);
            asm.new_segment(cmd.span.clone(), crate::assembler::StartKind::Freespace { align }, target);
        }
        "org" => {
            if let Some(c) = tokens.peek_non_wsp() {
                let c = Expression::parse(&mut tokens, target);
                asm.new_segment(cmd.span.clone(), crate::assembler::StartKind::Expression(c), target);
            } else {
                errors::cmd_no_arg(cmd.span.clone(), "org", "address", "$008000").push();
                return;
            }
        }
        "incbin" => {
            let mut temp;
            let c = if let Some(c) = tokens.peek_non_wsp() {
                if c.is_string() {
                    tokens.next_non_wsp();
                    temp = if let Some(c) = lexer::expand_str(c.span.clone(), target) { c } else { return; };
                    &temp[1..temp.len()-1]
                } else {
                    temp = String::new();
                    while let Some(t) = tokens.peek() {
                        if t.span.eq(":") { break; }
                        tokens.next();
                        temp.push_str(&t.span);
                    }
                    temp.trim()
                }
            } else {
                panic!();
            };
            let (mut start, mut end) = (0, 0);
            if let Some(c) = tokens.peek_non_wsp() {
                if !c.span.eq(":") { panic!("{}", c.span); }
                tokens.next_non_wsp();
                // todo: handle exprs here
                start = usize::from_str_radix(&tokens.next_non_wsp().unwrap().span, 16).unwrap();
                let dash = tokens.next_non_wsp().unwrap();
                end = usize::from_str_radix(&tokens.next_non_wsp().unwrap().span, 16).unwrap();
            }
            let mut buf = ctx.path.clone();
            buf.push(c);
            let file = std::fs::read(buf).unwrap();
            if end == 0 { end = file.len(); }
            let file = file[start..end].to_vec();
            let stmt = Statement::binary(file, cmd.span.clone());
            asm.append(stmt, target);
        }
        "padbyte" => {
            if tokens.peek_non_wsp().is_none() {
                errors::cmd_no_arg(cmd.span.clone(), "padbyte", "padding byte", "$EA").push();
                return;
            }
            let expr = Expression::parse(&mut tokens, target);
            target.pad_byte = expr.eval_const(target) as u32 as u8;
        }
        "pad" => {
            let val = if let Some(c) = tokens.peek_non_wsp() {
                let c = Expression::parse(&mut tokens, target);
                asm.new_segment_padded(cmd.span.clone(), crate::assembler::StartKind::Expression(c), target.pad_byte, target);
            } else {
                errors::cmd_no_arg(cmd.span.clone(), "pad", "address", "$008123").push();
                return;
            };
        }
        "base" => {
            let val = if let Some(c) = tokens.peek_non_wsp() {
                if tokens.peek_non_wsp().unwrap().span.eq("off") {
                    None
                } else {
                    let expr = Expression::parse(&mut tokens, target);
                    let mut base = expr.eval_const(target) as usize;
                    Some(base)
                }
            } else {
                errors::cmd_no_arg(cmd.span.clone(), "base", "address", "$7E2000").push();
                return;
            };
            let stmt = Statement::base(val, cmd.span.clone());
            asm.append(stmt, target);
        }
        "check" => {
            let param = tokens.next_non_wsp();
            let value = tokens.next_non_wsp().unwrap();
            let stmt = Statement::bankcross(value.span.eq("off"), cmd.span.clone());
            asm.append(stmt, target);
        },
        "reset" => {},  // TODO
        "warnpc" => {
            //let c = tokens.next_non_wsp().map(|c| c.span.to_string()).unwrap_or("".into());
            //target.push_msg(Message::info(cmd.span.clone(), 0, c))
            let next = tokens.peek_non_wsp();
            if next.is_none() {
                errors::cmd_no_arg(cmd.span.clone(), "warnpc", "address", "$008123").push();
                return;
            }
            let c = Expression::parse(&mut tokens, target);
            let stmt = Statement::warnpc(c, cmd.span.clone());
            asm.append(stmt, target);
        },
        "assert" => {
            let next = tokens.peek_non_wsp();
            if next.is_none() {
                errors::cmd_no_arg(cmd.span.clone(), "assert", "condition", "2 + 2 == 4").push();
                return;
            }
            let c = Expression::parse(&mut tokens, target);
            tokens.next_non_wsp();  // skip comma
            let msg = if let Some(s) = tokens.peek_non_wsp() {
                Expression::parse(&mut tokens, target)
            } else {
                Expression::string(cmd.span.clone(), "Assertion failed")
            };
            let stmt = Statement::assert(c, msg, cmd.span.clone());
            asm.append(stmt, target);
        }
        "print" => {
            let mut exprs = vec![];
            while let Some(c) = tokens.peek_non_wsp() {
                match c {
                    c if c.span.eq("freespaceuse") => {
                        exprs.push(Expression::string(c.span.clone(), "freespaceuse"));
                        tokens.next_non_wsp();
                    }
                    c if c.span.eq("pc") => {
                        exprs.push(Expression::pc(c.span.clone()));
                        tokens.next_non_wsp();
                    }
                    c if c.span.eq(",") => { tokens.next_non_wsp(); }
                    _ => {
                        exprs.push(Expression::parse(&mut tokens, target));
                    }
                }
            }
            if exprs.len() == 0 {
                errors::cmd_no_arg(cmd.span.clone(), "print", "message", "\"hello world\"").push();
                return;
            }

            let stmt = Statement::print(exprs, cmd.span.clone());
            asm.append(stmt, target);
        }
        "const_print" => {
            // Temp hack until I get eager consteval for exprs
            let mut exprs = vec![];
            while let Some(c) = tokens.peek_non_wsp() {
                match c {
                    c if c.span.eq("freespaceuse") => {
                        exprs.push(Expression::string(c.span.clone(), "freespaceuse"));
                        tokens.next_non_wsp();
                    }
                    c if c.span.eq("pc") => {
                        exprs.push(Expression::pc(c.span.clone()));
                        tokens.next_non_wsp();
                    }
                    c if c.span.eq(",") => { tokens.next_non_wsp(); }
                    _ => {
                        exprs.push(Expression::parse(&mut tokens, target));
                    }
                }
            }
            if exprs.len() == 0 {
                errors::cmd_no_arg(cmd.span.clone(), "const_print", "message", "\"hello world\"").push();
                return;
            }
            let mut out = String::new();
            for i in exprs {
                if let Some((s, val)) = i.try_eval_const(target) {
                    let s = val.to_string();
                    out.push_str(&s);
                }
            }
            println!("{}", out);
        }
        "expr" => {
            let c = Expression::parse(&mut tokens, target);
            Message::info(cmd.span.clone(), format!("{:?}", c)).push();
        }
        "warn" => {
            let expr = Expression::parse(&mut tokens, target);
            let mut base = expr.eval_const_str(target);
            Message::warning(cmd.span.clone(), base).push();
        }
        "error" => {
            let expr = Expression::parse(&mut tokens, target);
            let mut base = expr.eval_const_str(target);
            Message::error(cmd.span.clone(), 0, base).push();
        }
        instr if instr.len() == 3 => {
            if let Some(stmt) = crate::instruction::parse(cmd, &mut tokens, target) {
                asm.append(stmt, target);
            } else {
                errors::cmd_unknown(cmd.span.clone()).push();
            }
        }
        c => {
            errors::cmd_unknown(cmd.span.clone()).push();
        }
    }
}
fn parse_func_args(tokens: &mut TokenList, target: &mut Target) -> Option<Vec<String>> {
    let paren = if let Some(c) = tokens.next_non_wsp() { c } else {
        errors::arglist_expected(tokens.last().unwrap().span.clone()).push();
        return None;
    };
    if !paren.span.eq("(") {
        errors::arglist_expected(paren.span.clone()).push();
        return None;
    }
    let mut args = vec![];
    let n = if let Some(c) = tokens.peek_non_wsp() { c } else {
        errors::arglist_unterminated(tokens.last().unwrap().span.clone()).push();
        return None;
    };
    if !n.span.eq(")") {
        loop {
            let arg = if let Some(c) = tokens.next_non_wsp() { c } else {
                errors::arglist_unterminated(tokens.last().unwrap().span.clone()).push();
                return None;
            };
            if !arg.is_ident() {
                errors::arglist_non_ident(arg.span.clone()).push();
                return None;
            }
            args.push(arg.span.to_string());
            match tokens.next_non_wsp() {
                Some(c) if &*c.span == "," => continue,
                Some(c) if &*c.span == ")" => break,
                None => {
                    errors::arglist_unterminated(tokens.last().unwrap().span.clone()).push();
                    return None;
                },
                Some(c) => {
                    errors::arglist_non_ident(c.span.clone()).push();
                    return None;
                }
            }
        }
    } else { tokens.next_non_wsp(); }
    Some(args)
}
