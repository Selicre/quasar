use std::rc::Rc;
use std::fmt::Display;
use std::collections::HashMap;

use crate::context::{LineInfo, LocalContext, ContextStr};
use crate::message::Message;

use crate::splitter;
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
    stmts: Rc<[LocalContext]>
}
impl ParsedFile {
    fn iter(&self) -> impl Iterator<Item=ContextStr> + '_ {
        self.stmts.iter().map(move |c| {
            self.data.slice_local(*c, self.filename.clone())
        })
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
    for i in file.iter() {
        exec_stmt(i, target);
    }
}

pub fn exec_stmt(i: ContextStr, target: &mut Target) {
    //println!("executing: {} at {}", i, i.source().short());
    let mut tokens = lexer::tokenize_stmt(i.clone(), target);

    // Parse setting defines as special syntax
    if matches!(tokens.as_slice(),
            [def,ws1,op,ws2,value,..]
            if def.is_define()
            && ws1.is_whitespace()
            && op.is_symbol()
            && ws2.is_whitespace()) {
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
                let new_token = Token {
                    span: ContextStr::new(new_token, LineInfo::custom(format!("<glued from {}>", locations))),
                    kind: TokenKind::Ident
                };
                tokens.splice(cur..cur+t.len(), std::iter::once(new_token));
            }
            cur += 1;
        }
        // Split statements and process them separately
        let mut start = 0;
        for i in 0..tokens.len()-2 {
            if tokens[i].is_whitespace() && &*tokens[i+1].span == ":" && tokens[i+2].is_whitespace() {
                let mut t = &tokens[start..i];
                exec_cmd(t, target);
                start = i + 2;
            }
        }
        exec_cmd(&tokens[start..], target);
    } else {
        exec_cmd(&tokens, target);
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
            temp = if let Some(c) = lexer::expand_str(def, target) { c } else { return false; };
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
            tokens.splice(c..c+1, value.clone());
        } else {
            let token_str = tokens.iter().map(|c| c.span.to_string()).collect::<Vec<_>>().concat();
            target.push_msg(Message::error(tokens[c].span.clone(), 8, format!("Define {:?} not found in this scope", &*tokens[c].span))
                .with_help(format!("expansion state: {}", token_str))
                .with_help(format!("while expanding statement: {} at {}", line, line.source().short()))
            );
            break;
        }
    }
    expanded
}

fn exec_cmd(mut tokens: &[Token], target: &mut Target) {
    // trim whitespace
    if let (Some(start), Some(end)) = (tokens.iter().position(|c| !c.is_whitespace()), tokens.iter().rposition(|c| !c.is_whitespace())) {
        tokens = &tokens[start..end+1];
    } else { return; }

    for i in tokens {
        print!("{}", i.span);
    }
    println!();
}
