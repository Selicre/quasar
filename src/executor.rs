use std::rc::Rc;
use std::fmt::Display;
use std::collections::HashMap;

use indexmap::IndexSet;

use crate::context::{LineInfo, LocalContext, ContextStr};
use crate::message::Message;

use crate::splitter::{self, Block};
use crate::lexer::{self, Token, TokenKind, TokenList};
use crate::expression::{self, Label, Expression};
use crate::assembler::{Statement, Assembler};

use parser::exec_cmd;

mod parser;

pub struct Target {
    files: HashMap<Rc<str>, ParsedFile>,
    messages: Vec<Message>,
    defines: HashMap<String, Vec<Token>>,
    macros: HashMap<String, Vec<Token>>,
    functions: HashMap<String, (usize, Expression)>,
    label_idx: IndexSet<Label>,
    label_ctx: LabelCtx,
    has_error: bool,
    profiler: std::time::Instant,
}
impl Target {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            messages: vec![],
            defines: HashMap::new(),
            functions: HashMap::new(),
            macros: HashMap::new(),
            label_idx: IndexSet::new(),
            label_ctx: LabelCtx::default(),
            has_error: false,
            profiler: std::time::Instant::now()
        }
    }
    pub fn profiler(&self, msg: &str) {
        eprintln!("[{:>6}µs] {}", self.profiler.elapsed().as_micros(), msg);
    }
    pub fn push_msg(&mut self, msg: Message) {
        self.has_error |= msg.is_error();
        self.messages.push(msg);
    }
    pub fn push_error(&mut self, source: ContextStr, code: usize, data: String) {
        self.has_error = true;
        self.messages.push(Message::error(source, code, data));
    }
    pub fn push_warning(&mut self, source: ContextStr, code: usize, data: String) {
        self.messages.push(Message::warning(source, code, data));
    }
    pub fn push_info(&mut self, source: ContextStr, code: usize, data: String) {
        self.messages.push(Message::info(source, code, data));
    }
    pub fn iter_messages(&self) -> impl Iterator<Item=&Message> + '_ {
        self.messages.iter()
    }
    pub fn clear_messages(&mut self) {
        self.messages.clear()
    }
    pub fn has_error(&self) -> bool { self.has_error }
    pub fn defines(&self) -> &HashMap<String, Vec<Token>> { &self.defines }
    pub fn label_id(&mut self, label: Label) -> usize {
        self.label_idx.insert_full(label).0
    }
    pub fn label_name(&mut self, id: usize) -> Option<&Label> {
        self.label_idx.get_index(id)
    }
    pub fn segment_label(&mut self, seg: usize) -> usize {
        let (id, _) = self.label_idx.insert_full(Label::Segment(seg));
        id
    }
    pub fn add_function(&mut self, name: String, arity: usize, expr: Expression) {
        let c = self.functions.insert(name, (arity, expr));
        if c.is_some() { panic!(); }
    }
    pub fn function(&mut self, name: &str) -> Option<&(usize, Expression)> {
        self.functions.get(name)
    }
    pub fn set_label(&mut self, mut label: Label, context: ContextStr) -> usize {
        match &mut label {
            Label::Named { stack } => self.label_ctx.named = stack.clone(),
            Label::AnonPos { depth, pos } => {
                if self.label_ctx.pos.len() <= *depth {
                    self.label_ctx.pos.resize(*depth+1, 0);
                }
                self.label_ctx.pos[*depth] += 1;
                *pos = self.label_ctx.pos[*depth];
            }
            Label::AnonNeg { depth, pos } => {
                if self.label_ctx.neg.len() <= *depth {
                    self.label_ctx.neg.resize(*depth+1, 0);
                }
                self.label_ctx.neg[*depth] += 1;
                *pos = self.label_ctx.neg[*depth];
            },
            _ => {}
        }
        let (id, _) = self.label_idx.insert_full(label.clone());
        //println!("set label {} to {:?}", id, label);
        id
    }
    pub fn resolve_sub(&mut self, depth: usize, label: ContextStr) -> Vec<String> {
        if depth > self.label_ctx.named.len() {
            self.push_error(label, 24, "Label has no parent".into());
            vec![]
        } else {
            let mut c = self.label_ctx.named[..depth].to_vec();
            c.push(label.to_string());
            c
        }
    }
    pub fn resolve_anon(&mut self, depth: usize, pos: bool, label: ContextStr) -> usize {
        if pos {
            if self.label_ctx.pos.len() <= depth {
                self.label_ctx.pos.resize(depth+1, 0);
            }
            self.label_ctx.pos[depth]+1
        } else {
            if self.label_ctx.neg.len() <= depth {
                self.label_ctx.neg.resize(depth+1, 0);
            }
            self.label_ctx.neg[depth]
        }
    }
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
struct LabelCtx {
    named: Vec<String>,
    neg: Vec<usize>,
    pos: Vec<usize>,
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
    pub fn enabled(&self) -> bool {
        self.if_depth.is_none()
    }
    pub fn run_endif(&mut self) {
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

pub fn exec_file(filename: Rc<str>, source: ContextStr, target: &mut Target, asm: &mut Assembler) {
    let ff = filename.clone();
    target.profiler(&format!("executing {}", ff));
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
    target.profiler(&format!("split {}", ff));

    let mut ctx = ExecCtx::default();
    while let Some((i, newline)) = file.stmt(ctx.exec_ptr) {
        let rep = ctx.rep_count.take().unwrap_or(1);
        for _ in 0..rep {
            exec_stmt(i.clone(), newline, target, &mut ctx, asm);
        }
        ctx.exec_ptr += 1;
    }
    target.profiler(&format!("done {}", ff));
}

fn exec_stmt(i: ContextStr, newline: bool, target: &mut Target, ctx: &mut ExecCtx, asm: &mut Assembler) {
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
        exec_cmd(&[], newline, target, ctx, asm);
        if !ctx.enabled() { return; }
        let mut exp_defines = false;
        let mut check_existing = false;
        let mut append = false;
        let mut do_math = false;
        match &*tokens[2].span {
            "=" => {},
            ":=" => exp_defines = true,
            "?=" => check_existing = true,
            "+=" => append = true,
            "#=" => { do_math = true; exp_defines = true; },
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

        let name = tokens[0].as_define().unwrap();
        if exp_defines { expand_defines(&mut v, &i, target); }
        if do_math {
            let mut t = TokenList::new(&v);
            let expr = Expression::parse(&mut t, target);
            let value = expr.eval_const(target);
            target.defines.insert(name.to_string(), vec![Token::from_number(tokens[0].span.clone(), value as _)]);
            return;
        }

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
                exec_cmd(t, false, target, ctx, asm);
                start = i + 2;
            }
        }
        exec_cmd(&tokens[start..], newline, target, ctx, asm);
    } else {
        exec_cmd(&tokens, newline, target, ctx, asm);
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

