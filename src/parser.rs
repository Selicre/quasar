use std::rc::Rc;
use std::fmt::Display;
use std::collections::HashMap;

use crate::context::{LineInfo, LocalContext, ContextStr};
use crate::message::Message;

use crate::executor::Target;


pub fn parse_file(mut file: ContextStr, target: &mut Target) -> Vec<LocalContext> {
    let mut stmts = vec![];

    // Split file into statements
    loop {
        // Strip excess separators
        let next_stmt = file.find(|c| !(c == ' ' || c == '\n'));
        if let Some(next_stmt) = next_stmt {
            file.advance(next_stmt);
        } else {
            break;
        }

        // Find the end of the statement
        if let Some(next_stmt) = file.find('\n') {
            let mut stmt = file.advance(next_stmt);
            parse_line(stmt, target, &mut stmts);
        } else {
            parse_line(file, target, &mut stmts);
            break;
        }
    }
    stmts
}

pub fn parse_line(mut line: ContextStr, target: &mut Target, stmts: &mut Vec<LocalContext>) {
    if let Some(idx) = line.find(';') {
        let l = line.advance(idx);
        if let Some(c) = line.strip_prefix(";@") {
            target.push_info(line.source().clone(), 0, format!("Special command: {}", c));
        }
        line = l;
    }
    if line.len() == 0 { return; }

    while let Some(next_stmt) = line.find(" : ") {
        let mut stmt = line.advance(next_stmt);
        preparse_stmt(stmt, target, stmts);
        line.advance(2);    // skip the separator
    }
    preparse_stmt(line, target, stmts);
}

pub fn preparse_stmt(mut stmt: ContextStr, target: &mut Target, stmts: &mut Vec<LocalContext>) {
    stmt.trim();
    stmts.push(stmt.local());
}
