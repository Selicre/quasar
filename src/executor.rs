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

use parser::exec_stmt;

mod parser;

pub struct Target {
    files: HashMap<Rc<str>, ParsedFile>,
    messages: Vec<Message>,
    defines: HashMap<String, Vec<Token>>,
    macros: HashMap<String, Macro>,
    cur_macro: Option<(String, Macro)>,
    macro_label_ctx: Vec<(usize, LabelCtx)>,    // (invoke_id, label_ctx)
    macro_invoke: usize,
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
            macro_label_ctx: vec![],
            macro_invoke: 0,
            cur_macro: None,
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
    pub fn macro_invoke(&self) -> Option<usize> { self.macro_label_ctx.last().map(|c| c.0) }
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
    pub fn start_macro(&mut self, name: String, args: Vec<String>) {
        self.cur_macro = Some((name, Macro { args, blocks: vec![] }));
    }
    pub fn finish_macro(&mut self) {
        let m = self.cur_macro.take().unwrap();
        println!("got macro: {:?}", m.1.blocks);
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
                panic!("not in a macro");
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
        let (id, _) = self.label_idx.insert_full(label.clone());
        println!("set label {} to {:?}", id, label);
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
    if_stack: Vec<CondLayer>,
    if_inline: bool,
    rep_count: Option<usize>,
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
            self.exec_ptr = c;
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

        let mut file_str = match String::from_utf8(file) {
            Ok(c) => c,
            Err(e) => {
                target.push_error(source, 2, format!("File {} is not utf-8: {}", filename, e));
                return;
            }
        };
        if !file_str.ends_with("\n") { file_str.push('\n'); }
        let mut file = ContextStr::new(file_str, LineInfo::file(filename.clone()));
        let stmts = splitter::split_file(file.clone(), target).into_boxed_slice().into();
        target.files.entry(filename.clone()).or_insert(ParsedFile {
            data: file, stmts, filename
        })
    }.clone();
    target.profiler(&format!("split {}", ff));

    let mut ctx = ExecCtx::default();
    while let Some((i, newline)) = file.stmt(ctx.exec_ptr) {
        exec_stmt(i.clone(), newline, target, &mut ctx, &HashMap::new(), asm);
        ctx.exec_ptr += 1;
    }
    target.profiler(&format!("done {}", ff));
}

#[derive(Clone)]
pub struct Macro {
    args: Vec<String>,
    blocks: Vec<(ContextStr, bool)>
}

pub fn exec_macro(name: &str, args: Vec<Vec<Token>>, source: ContextStr, target: &mut Target, asm: &mut Assembler) {
    let mut ctx = ExecCtx::default();
    let mac = target.get_macro(name).unwrap().clone();
    let args = mac.args.iter().cloned().zip(args.into_iter()).collect::<HashMap<_,_>>();
    target.macro_label_ctx.push((target.macro_invoke, LabelCtx::default()));
    target.macro_invoke += 1;
    while let Some((i, newline)) = mac.blocks.get(ctx.exec_ptr) {
        exec_stmt(i.clone(), *newline, target, &mut ctx, &args, asm);
        ctx.exec_ptr += 1;
    }
    target.macro_label_ctx.pop();
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
