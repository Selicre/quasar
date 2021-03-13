use std::rc::Rc;
use std::fmt::Display;
use std::collections::HashMap;

use crate::context::{LineInfo, LocalContext, ContextStr};
use crate::message::Message;

use crate::parser::parse_file;

pub struct Target {
    files: HashMap<Rc<str>, ParsedFile>,
    messages: Vec<Message>,
    has_error: bool
}
impl Target {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            messages: vec![],
            has_error: false,
        }
    }
    pub fn push_error(&mut self, source: LineInfo, code: usize, data: String) {
        self.has_error = true;
        self.messages.push(Message::error(source, code, data));
    }
    pub fn push_info(&mut self, source: LineInfo, code: usize, data: String) {
        self.messages.push(Message::info(source, code, data));
    }
    pub fn iter_messages(&self) -> impl Iterator<Item=&Message> + '_ {
        self.messages.iter()
    }
}

#[derive(Debug, Clone)]
struct ParsedFile {
    filename: Rc<str>,
    data: ContextStr,
    stmts: Rc<[LocalContext]>
}
impl ParsedFile {
    fn iter(&self) -> impl Iterator<Item=ContextStr> + '_ {
        self.stmts.iter().map(move |c| {
            self.data.slice_local(*c, self.filename.clone())
        })
    }
}

pub fn exec_file(filename: Rc<str>, source: LineInfo, target: &mut Target) {
    let file = if let Some(c) = target.files.get(&filename) {
        c
    } else {
        let file = match std::fs::read(&*filename) {
            Ok(c) => c,
            Err(e) => {
                target.push_error(source, 0, format!("Could not read file {}: {}", filename, e));
                return;
            }
        };

        let file_str = match String::from_utf8(file) {
            Ok(c) => c,
            Err(e) => {
                target.push_error(source, 1, format!("File {} is not utf-8: {}", filename, e));
                return;
            }
        };

        let mut file = ContextStr::new(file_str, LineInfo::file(filename.clone()));
        let stmts = parse_file(file.clone(), target).into_boxed_slice().into();
        target.files.entry(filename.clone()).or_insert(ParsedFile {
            data: file, stmts, filename
        })
    }.clone();
    for i in file.iter() {
        exec_stmt(i, target);
    }
}

pub fn exec_stmt(i: ContextStr, target: &mut Target) {
    println!("executing: {} at {}", i, i.source().short());
}
