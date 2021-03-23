use std::rc::Rc;
use std::fmt::Display;
use std::collections::HashMap;

use crate::context::{LineInfo, LocalContext, ContextStr};
use crate::message::Message;

use crate::splitter::{self, Block};
use crate::lexer::{self, Token, TokenKind};

pub struct Target {
    files: HashMap<Rc<str>, ParsedFile>,
    messages: Vec<Message>,
    defines: HashMap<String, Vec<Token>>,
    has_error: bool
}
impl Target {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            messages: vec![],
            defines: HashMap::new(),
            has_error: false,
        }
    }
    pub fn push_msg(&mut self, msg: Message) {
        self.has_error |= msg.is_error();
        self.messages.push(msg);
    }
    pub fn push_error(&mut self, source: ContextStr, code: usize, data: String) {
        self.has_error = true;
        self.messages.push(Message::error(source, code, data));
    }
    pub fn push_info(&mut self, source: ContextStr, code: usize, data: String) {
        self.messages.push(Message::info(source, code, data));
    }
    pub fn iter_messages(&self) -> impl Iterator<Item=&Message> + '_ {
        self.messages.iter()
    }
    pub fn has_error(&self) -> bool { self.has_error }
    pub fn defines(&self) -> &HashMap<String, Vec<Token>> { &self.defines }
}

#[derive(Debug, Clone)]
struct ParsedFile {
    filename: Rc<str>,
    data: ContextStr,
    stmts: Rc<[Block]>
}
impl ParsedFile {
    fn stmt(&self, id: usize) -> Option<(ContextStr, bool)> {
        let block = self.stmts.get(id)?;
        Some((self.data.slice_local(block.context, self.filename.clone()), block.newline))
    }
}

#[derive(Default)]
struct ExecCtx {
    exec_ptr: usize,
    if_stack: Vec<Option<usize>>,
    if_depth: Option<usize>,
    if_branch_done: bool,
    if_inline: bool,
    rep_count: Option<usize>,
}

impl ExecCtx {
    fn enabled(&self) -> bool {
        self.if_depth.is_none()
    }
    fn run_endif(&mut self) {
        if let Some(c) = self.if_stack.pop().unwrap() {
            if self.enabled() {
                self.exec_ptr = c-1;
                return;
            }
        }
        if let Some(c) = self.if_depth {
            if c == self.if_stack.len() { self.if_depth = None; }
        }
    }
}

pub fn exec_file(filename: Rc<str>, source: ContextStr, target: &mut Target) {
    let file = if let Some(c) = target.files.get(&filename) {
        c
    } else {
        let file = match std::fs::read(&*filename) {
            Ok(c) => c,
            Err(e) => {
                target.push_error(source, 1, format!("Could not read file {}: {}", filename, e));
                return;
            }
        };

        let file_str = match String::from_utf8(file) {
            Ok(c) => c,
            Err(e) => {
                target.push_error(source, 2, format!("File {} is not utf-8: {}", filename, e));
                return;
            }
        };
        let mut file = ContextStr::new(file_str, LineInfo::file(filename.clone()));
        let stmts = splitter::split_file(file.clone(), target).into_boxed_slice().into();
        target.files.entry(filename.clone()).or_insert(ParsedFile {
            data: file, stmts, filename
        })
    }.clone();

    let mut ctx = ExecCtx::default();
    while let Some((i, newline)) = file.stmt(ctx.exec_ptr) {
        let rep = ctx.rep_count.take().unwrap_or(1);
        for _ in 0..rep {
            exec_stmt(i.clone(), newline, target, &mut ctx);
        }
        ctx.exec_ptr += 1;
    }
}

fn exec_stmt(i: ContextStr, newline: bool, target: &mut Target, ctx: &mut ExecCtx) {
    //println!("executing: {} at {}", i, i.source().short());
    let mut tokens = lexer::tokenize_stmt(i.clone(), target);

    // Parse setting defines as special syntax
    if matches!(tokens.as_slice(),
            [def,ws1,op,ws2,value,..]
            if def.is_define()
            && ws1.is_whitespace()
            && op.is_symbol()
            && ws2.is_whitespace()) {
        // mostly for endifs to work properly
        exec_cmd(&[], newline, target, ctx);
        if !ctx.enabled() { return; }
        let mut exp_defines = false;
        let mut check_existing = false;
        let mut append = false;
        match &*tokens[2].span {
            "=" => {},
            ":=" => exp_defines = true,
            "?=" => check_existing = true,
            "+=" => append = true,
            "#=" => exp_defines = true,
            _ => {
                target.push_error(tokens[2].span.clone(), 9, format!("Unknown define operator"));
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
            v = lexer::tokenize_stmt(ctx, target);
        } else {
            v = tokens[4..].to_vec();
        }

        if exp_defines { expand_defines(&mut v, &i, target); }

        let name = tokens[0].as_define().unwrap();
        let name = if tokens[0].is_define_escaped().unwrap() {
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
    let expanded = expand_defines(&mut tokens, &i, target);

    for i in tokens.iter_mut() {
        if let Some(c) = i.as_string_mut() {
            c.advance(1);
            c.cut(1);
            let out = format!("\"{}\"", lexer::expand_str(c.clone(), target).unwrap_or("".into()));
            i.span = ContextStr::new(out, i.span.source().clone());
        }
    }
    if expanded {
        // Glue tokens
        let mut cur = 0;
        while cur < tokens.len() {
            if !tokens[cur].is_ident() {
                cur += 1;
                continue;
            }

            let t = tokens[cur..].iter().take_while(|c| c.is_ident() || c.is_decimal()).cloned().collect::<Vec<_>>();
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
                    kind: TokenKind::Ident
                };
                tokens.splice(cur..cur+t.len(), std::iter::once(new_token));
            }
            cur += 1;
        }
        // Split statements and process them separately
        let mut start = 0;
        for i in 0..tokens.len().saturating_sub(2) {
            if tokens[i].is_whitespace() && &*tokens[i+1].span == ":" && tokens[i+2].is_whitespace() {
                let mut t = &tokens[start..i];
                exec_cmd(t, false, target, ctx);
                start = i + 2;
            }
        }
        exec_cmd(&tokens[start..], newline, target, ctx);
    } else {
        exec_cmd(&tokens, newline, target, ctx);
    }
}

pub fn expand_defines(mut tokens: &mut Vec<Token>, line: &ContextStr, target: &mut Target) -> bool {
    let mut recursion = 0;
    let mut expanded = false;

    let mut expand_history: Vec<Token> = vec![];    // Recursion detection
    while let Some(c) = tokens.iter().position(|c| c.is_define()) {
        if recursion > 128 {
            let token_str = tokens.iter().map(|c| c.span.to_string()).collect::<Vec<_>>().concat();
            target.push_msg(Message::error(line.clone(), 11, format!("Define expansion limit reached"))
                //.with_help(format!("expansion state: {}", token_str))
                .with_help(format!("while expanding statement: {} at {}", line, line.source().short()))
            );
            break;
        }
        let def = tokens[c].as_define().unwrap();


        let temp;
        let def_s = if tokens[c].is_define_escaped().unwrap() {
            temp = if let Some(c) = lexer::expand_str(def.clone(), target) { c } else { return false; };
            &*temp
        } else {
            &*def
        };

        // this is broken
        /*if let Some(d) = expand_history.iter().find(|c| c.as_define().unwrap() == def) {
            let token_str = tokens.iter().map(|c| c.span.to_string()).collect::<Vec<_>>().concat();
            target.push_msg(Message::error(d.span.clone(), 12, format!("Use of infinitely recursive define {:?}", def))
                .with_help(format!("expansion state: {}", token_str))
                .with_help(format!("while expanding statement: {} at {}", line, line.source().short()))
            );
            break;
        } else {
            expand_history.push(tokens[c].clone());
        }*/
        if let Some(value) = target.defines.get(def_s) {
            expanded = true;
            recursion += 1;
            let mut value = value.clone();
            value.iter_mut().for_each(|c| c.span.set_parent(def.clone()));
            tokens.splice(c..c+1, value);
        } else {
            let token_str = tokens.iter().map(|c| c.span.to_string()).collect::<Vec<_>>().concat();
            target.push_msg(Message::error(def.clone(), 8, format!("Define {:?} not found in this scope", def_s))
                .with_help(format!("expansion state: {}", token_str))
                .with_help(format!("while expanding statement: {} at {}", line, line.source().short()))
            );
            break;
        }
    }
    expanded
}

struct TokenList<'a> {
    inner: &'a [Token],
    pos: usize
}

impl<'a> TokenList<'a> {
    fn new(inner: &'a [Token]) -> Self {
        Self { inner, pos: 0 }
    }
    fn next_non_wsp(&mut self) -> Option<&'a Token> {
        while self.inner.get(self.pos)?.is_whitespace() { self.pos += 1; }
        let res = self.inner.get(self.pos);
        self.pos += 1;
        res
    }
}


fn exec_cmd(mut tokens: &[Token], newline: bool, target: &mut Target, ctx: &mut ExecCtx) {
    print!("{} ", if ctx.enabled() { "e" } else { "n" });
    for i in tokens.iter() {
        print!("{}", i.span);
    }
    println!();

    let mut tokens = TokenList::new(tokens);


    let if_inline = ctx.if_inline;
    if ctx.if_inline && newline {
        ctx.if_inline = false;
        ctx.run_endif();
    }
    let cmd = if let Some(c) = tokens.next_non_wsp() { c } else { return; };
    if cmd.is_ident() {
        match &*cmd.span {
            "if" => {
                if let Some(c) = tokens.next_non_wsp() {
                    if ctx.enabled() && &*c.span != "1" {  // todo: proper expr handling
                        ctx.if_depth = Some(ctx.if_stack.len());
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
                if let Some(c) = tokens.next_non_wsp() {
                    if ctx.enabled() && &*c.span != "1" {  // todo: proper expr handling
                        ctx.if_depth = Some(ctx.if_stack.len());
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
                if let Some(c) = tokens.next_non_wsp() {
                    if ctx.enabled() {
                        ctx.if_branch_done = true;
                    }
                    if ctx.if_branch_done {
                        ctx.if_depth = Some(ctx.if_stack.len());
                    } else {
                        if &*c.span != "1" {  // todo: proper expr handling
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
            "repeat" if ctx.enabled() => {
                let count = if let Some(c) = tokens.next_non_wsp() { c } else {
                    target.push_msg(Message::error(cmd.span.clone(), 17, format!("`repeat` statement with no loop count"))
                        .with_help(format!("add a loop count, like `repeat 5`")));
                    return;
                };
                let mut count = if let Some(c) = count.as_number() { c } else {
                    target.push_msg(Message::error(cmd.span.clone(), 18, format!("`repeat` statement with non-numeric loop count"))
                        .with_help(format!("use a number as a loop count, like `repeat 5`")));
                    return;
                };
                if count < 0 { count = 0; }
                ctx.rep_count = Some(count as usize);
            }
            "print" if ctx.enabled() => {
                let c = tokens.next_non_wsp().map(|c| c.span.to_string()).unwrap_or("".into());
                target.push_msg(Message::info(cmd.span.clone(), 0, c))
            }
            _ => {}
        }
    }
}