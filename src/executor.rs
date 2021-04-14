use std::rc::Rc;
use std::fmt::Display;
use std::collections::HashMap;
use std::path::PathBuf;
use std::borrow::Cow;

use indexmap::IndexSet;

use crate::context::{LineInfo, LocalContext, ContextStr};
use crate::message::{errors, Message};

//use crate::splitter::{self, Block};
use crate::lexer::{self, Token, TokenKind, TokenList};
use crate::expression::{self, Label, Expression};
use crate::assembler::{Statement, Assembler};
use crate::rom::Rom;

use parser::exec_stmt;

mod parser;

pub struct Target {
    file_cache: HashMap<String, Vec<u8>>,
    rom_cache: Rom,
    defines: HashMap<String, Vec<Token>>,
    macros: HashMap<String, Macro>,
    cur_macro: Option<(String, Macro)>,
    macro_label_ctx: Vec<(usize, LabelCtx)>,    // (invoke_id, label_ctx)
    macro_invoke: usize,
    functions: HashMap<String, (usize, Expression)>,
    label_idx: IndexSet<Label>,
    label_ctx: LabelCtx,
    phantom_label_idx: usize,
    table: HashMap<char, u32>,
    table_stack: Vec<HashMap<char, u32>>,
    pc_stack: Vec<usize>,
    namespace: Vec<String>,
    pad_byte: u8,
    profiler: std::time::Instant,
}
impl Target {
    pub fn new(rom_cache: Rom) -> Self {
        Self {
            file_cache: HashMap::new(),
            rom_cache,
            defines: HashMap::new(),
            functions: HashMap::new(),
            macros: HashMap::new(),
            macro_label_ctx: vec![],
            macro_invoke: 0,
            cur_macro: None,
            label_idx: IndexSet::new(),
            label_ctx: LabelCtx::default(),
            phantom_label_idx: 0,
            table: HashMap::new(),
            table_stack: vec![],
            pc_stack: vec![],
            namespace: vec![],
            pad_byte: 0,
            profiler: std::time::Instant::now()
        }
    }
    // TODO: read relative to cwd
    pub fn read_file<'a>(&'a mut self, source: &ContextStr, filename: &str) -> Result<&'a [u8], Message> {
        if self.file_cache.contains_key(filename) {
            Ok(self.file_cache.get(filename).map(|c| &**c).unwrap())
        } else {
            let c = std::fs::read(filename).map_err(|e| {
                errors::file_read(source.clone(), &filename, &e)
            })?;
            Ok(self.file_cache.entry(filename.to_string()).or_insert(c))
        }
    }
    pub fn read_file_n(&mut self, source: &ContextStr, filename: &str, offset: usize, len: usize) -> Result<f64, Message> {
        self.read_file(source, filename)?.get(offset..offset+len)
        .ok_or(errors::expr_read_file_oob(source.clone()))
        .map(|source| {
            let mut v = [0; 4];
            v[..len].copy_from_slice(source);
            u32::from_le_bytes(v) as f64
        })
    }
    pub fn read_rom(&mut self, source: &ContextStr, addr: u32, len: usize) -> Result<f64, Message> {
        self.rom_cache.read_at(addr, len, source)
    }
    pub fn profiler(&self, msg: &str) {
        eprintln!("[{:>6}µs] {}", self.profiler.elapsed().as_micros(), msg);
    }
    pub fn push_msg(&mut self, msg: Message) {
        msg.push();
    }
    pub fn push_error(&mut self, source: ContextStr, code: usize, data: String) {
        Message::error(source, code, data).push();
    }
    pub fn push_warning(&mut self, source: ContextStr, data: String) {
        Message::warning(source, data).push();
    }
    pub fn macro_invoke(&self) -> Option<usize> { self.macro_label_ctx.last().map(|c| c.0) }
    pub fn defines(&self) -> &HashMap<String, Vec<Token>> { &self.defines }
    pub fn label_id(&mut self, mut label: Label) -> usize {
        label.glue_sub();
        if self.namespace.len() > 0 {
            label.glue_namespace(&self.namespace);
        }
        self.label_idx.insert_full(label).0
    }
    pub fn label_name(&mut self, id: usize) -> Option<&Label> {
        self.label_idx.get_index(id)
    }
    pub fn segment_label(&mut self, seg: usize) -> usize {
        let (id, _) = self.label_idx.insert_full(Label::Segment(seg));
        id
    }
    pub fn phantom_label(&mut self) -> usize {
        let (id, _) = self.label_idx.insert_full(Label::Phantom(self.phantom_label_idx));
        self.phantom_label_idx += 1;
        id
    }
    pub fn add_function(&mut self, name: String, arity: usize, expr: Expression) {
        let c = self.functions.insert(name, (arity, expr));
        if c.is_some() { panic!(); }
    }
    pub fn function(&mut self, name: &str) -> Option<&(usize, Expression)> {
        self.functions.get(name)
    }
    pub fn start_macro(&mut self, name: String, args: Vec<String>, path: PathBuf) {
        self.cur_macro = Some((name, Macro { args, blocks: vec![], path }));
    }
    pub fn finish_macro(&mut self) {
        let m = self.cur_macro.take().unwrap();
        //println!("got macro: {:?}", m.1.blocks);
        self.macros.insert(m.0, m.1);
    }
    pub fn get_macro(&self, name: &str) -> Option<&Macro> {
        self.macros.get(name)
    }
    pub fn labels(&self) -> &IndexSet<Label> {
        &self.label_idx
    }
    fn label_ctx(&mut self, is_macro: Option<usize>) -> &mut LabelCtx {
        if let Some(c) = is_macro {
            if let Some((d,e)) = self.macro_label_ctx.last_mut() {
                if *d != c { panic!("uhhh internal error with macros ({} != {})", d, c); }
                e
            } else {
                panic!("internal error: not in a macro");
            }
        } else {
            &mut self.label_ctx
        }
    }
    pub fn set_label(&mut self, mut label: Label, context: ContextStr) -> usize {
        match &mut label {
            Label::Named { stack, invoke } => self.label_ctx(*invoke).named = stack.clone(),
            Label::AnonPos { depth, pos, invoke } => {
                let lc = self.label_ctx(*invoke);
                if lc.pos.len() <= *depth {
                    lc.pos.resize(*depth+1, 0);
                }
                lc.pos[*depth] += 1;
                *pos = lc.pos[*depth];
            }
            Label::AnonNeg { depth, pos, invoke } => {
                let lc = self.label_ctx(*invoke);
                if lc.neg.len() <= *depth {
                    lc.neg.resize(*depth+1, 0);
                }
                lc.neg[*depth] += 1;
                *pos = lc.neg[*depth];
            },
            _ => {}
        }
        //label.glue_sub();
        //let (id, _) = self.label_idx.insert_full(label.clone());
        let id = self.label_id(label.clone());
        //println!("set label {} to {:?}", id, label);
        id
    }
    pub fn resolve_sub(&mut self, depth: usize, label: ContextStr) -> Vec<String> {
        if depth > self.label_ctx.named.len() {
            //self.push_error(label, 24, "Label has no parent".into());
            errors::label_no_parent(label).push();
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
/*
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
}*/

#[derive(Default)]
struct LabelCtx {
    named: Vec<String>,
    neg: Vec<usize>,
    pos: Vec<usize>,
}

#[derive(Copy,Clone,Debug,PartialEq,Eq)]
enum CondLayer {
    IfTrue,
    IfFalse,
    IfDone,     // used to designate being done with the current layer
    IfSkip,     // used to skip over the if block entirely
    WhileTrue(usize),
    WhileFalse
}

impl CondLayer {
    fn is_while(&self) -> bool {
        matches!(self, CondLayer::WhileTrue(_)) || matches!(self, CondLayer::WhileFalse)
    }
    fn is_true(&self) -> bool {
        matches!(self, CondLayer::IfTrue) || matches!(self, CondLayer::WhileTrue(_))
    }
}

#[derive(Default)]
struct ExecCtx {
    exec_ptr: usize,
    next_exec_ptr: usize,
    if_stack: Vec<CondLayer>,
    if_inline: bool,
    rep_count: Option<usize>,
    path: PathBuf,
}

impl ExecCtx {
    fn enabled(&self) -> bool {
        self.current().is_true()
    }
    fn skipping(&self) -> bool {
        self.current() == CondLayer::IfSkip
    }
    fn current(&self) -> CondLayer {
        self.if_stack.last().cloned().unwrap_or(CondLayer::IfTrue)
    }
    fn run_endif(&mut self) {
        if let CondLayer::WhileTrue(c) = self.current() {
            self.next_exec_ptr = c;
        }
        self.if_stack.pop();
    }
    fn enter_if(&mut self, cond: bool) {
        self.if_stack.push(if cond { CondLayer::IfTrue } else { CondLayer::IfFalse });
    }
    fn enter_while(&mut self, cond: bool) {
        self.if_stack.push(if cond { CondLayer::WhileTrue(self.exec_ptr-1) } else { CondLayer::WhileFalse });
    }
    fn enter_skip(&mut self) {
        // If the current context is disabled, then the whole stack becomes disabled.
        self.if_stack.push(CondLayer::IfSkip);
    }
    fn enter_elseif(&mut self, cond: bool) {
        if let Some(c) = self.if_stack.last_mut() {
            *c = match *c {
                CondLayer::IfTrue => CondLayer::IfDone,
                CondLayer::IfFalse if cond => CondLayer::IfTrue,
                CondLayer::IfFalse => CondLayer::IfFalse,
                c => c, // done, skip
            }
        } else {
            panic!("elseif for while?");
        }
    }
}

pub fn exec_file(filename: &str, source: ContextStr, target: &mut Target, asm: &mut Assembler) {
    let ff = filename.clone();
    target.profiler(&format!("executing {}", ff));
    let file = {
        let file = match std::fs::read(&*filename) {
            Ok(c) => c,
            Err(e) => {
                errors::file_read(source, &filename, &e).push();
                return;
            }
        };

        let mut file_str = match String::from_utf8(file) {
            Ok(c) => c,
            Err(e) => {
                errors::file_non_utf8(source, &filename, &e).push();
                return;
            }
        };
        if !file_str.ends_with("\n") { file_str.push('\n'); }
        let mut file = ContextStr::new(file_str, LineInfo::file(filename.into()));
        file
    };
    let tokens = lexer::tokenize_stmt(file, target, true);
    target.profiler(&format!("tokenized {}", ff));

    let mut tokens = TokenList::new(&tokens);

    let mut ctx = ExecCtx::default();
    ctx.path = ff.into();
    ctx.path.pop();
    while let Some((i, nl)) = { tokens.seek(ctx.exec_ptr); tokens.split_off(true) } {
        ctx.next_exec_ptr = tokens.pos();
        exec_stmt(i.rest(), nl, target, &mut ctx, &HashMap::new(), asm);
        ctx.exec_ptr = ctx.next_exec_ptr;
    }
    target.profiler(&format!("done {}", ff));
}

#[derive(Clone)]
pub struct Macro {
    args: Vec<String>,
    blocks: Vec<(Vec<Token>, bool)>,
    path: PathBuf,
}

pub fn exec_macro(name: &str, args: Vec<Vec<Token>>, source: ContextStr, target: &mut Target, asm: &mut Assembler) {
    let mut ctx = ExecCtx::default();
    let mac = target.get_macro(name).unwrap().clone();
    ctx.path = mac.path.clone();
    let args = mac.args.iter().cloned().zip(args.into_iter()).collect::<HashMap<_,_>>();
    target.macro_label_ctx.push((target.macro_invoke, LabelCtx::default()));
    target.macro_invoke += 1;
    while let Some((t, newline)) = mac.blocks.get(ctx.exec_ptr) {
        exec_stmt(t, *newline, target, &mut ctx, &args, asm);
        ctx.exec_ptr += 1;
    }
    target.macro_label_ctx.pop();
}

pub fn expand_defines(tokens: &mut Cow<'_, [Token]>, line: &ContextStr, target: &mut Target) -> Result<bool, Message> {
    let mut recursion = 0;
    let mut expanded = false;

    let mut expand_history: Vec<Token> = vec![];    // Recursion detection
    while let Some(c) = tokens.iter().position(|c| c.is_define()) {
        if recursion > 128 {
            //let token_str = tokens.iter().map(|c| c.span.to_string()).collect::<Vec<_>>().concat();
            return Err(errors::define_rec_limit(line.clone()));
        }
        let token = tokens[c].clone();
        let def = token.as_define().unwrap();


        let temp;
        let def_s = if tokens[c].is_define_escaped().unwrap() {
            temp = if let Some(c) = lexer::expand_str(def.clone(), target) { c } else { panic!("what") };
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
            value.iter_mut().for_each(|c| c.span.set_parent(token.span.clone()));
            tokens.to_mut().splice(c..c+1, value);
        } else {
            //let token_str = tokens.iter().map(|c| c.span.to_string()).collect::<Vec<_>>().concat();
            return Err(errors::define_unknown(token.span.clone()));
        }
    }
    Ok(expanded)
}
