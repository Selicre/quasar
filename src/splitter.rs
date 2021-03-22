use std::rc::Rc;
use std::fmt::Display;
use std::collections::HashMap;

use crate::context::{LineInfo, LocalContext, ContextStr};
use crate::message::Message;

use crate::executor::Target;


pub fn split_file(mut file: ContextStr, target: &mut Target) -> Vec<LocalContext> {
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
        if let Some(stmt) = split_lines(&mut file) {
            parse_line(stmt, target, &mut stmts);
        } else {
            parse_line(file, target, &mut stmts);
            break;
        }
    }
    stmts
}

fn split_lines(line: &mut ContextStr) -> Option<ContextStr> {
    let needle = line.needle();
    loop {
        let next_line = line.find("\n")?;
        let stmt = line.advance(next_line);
        if stmt.ends_with("\\") || stmt.ends_with(",") {
            line.advance(1);
            continue;
        }
        break;
    }
    Some(line.prefix_from(needle))
}

pub fn parse_line(mut line: ContextStr, target: &mut Target, stmts: &mut Vec<LocalContext>) {
    if let Some(idx) = line.find(';') {
        let l = line.advance(idx);
        if let Some(c) = line.strip_prefix(";@") {
            target.push_info(line.clone(), 0, format!("Special command: {}", c));
        }
        line = l;
    }
    if line.len() == 0 { return; }


    // Right now this parses strings twice. Need to figure out some better solution

    let mut needle = line.needle();
    while line.len() != 0 {
        if line.starts_with("\"") {
            let str_needle = line.needle();
            line.advance_char();
            loop {
                if let Some(c) = line.advance_char() {
                    match c {
                        '\\' => { line.advance_char(); }
                        '"' => { break },
                        _ => {}
                    }
                } else {
                    let mut stmt = line.prefix_from(str_needle);
                    target.push_error(stmt, 5, format!("Unclosed string literal"));
                    return;
                }
            }
        } else if line.starts_with(" : ") {
            let mut stmt = line.prefix_from(needle);
            preparse_stmt(stmt, target, stmts);
            line.advance(2);    // skip the separator
            needle = line.needle();
        } else {
            line.advance_char();
        }
    }
    let mut stmt = line.prefix_from(needle);
    preparse_stmt(stmt, target, stmts);
    // Problem with the code below: does not respect strings. It is much faster though
    /*
    while let Some(next_stmt) = line.find(" : ") {
        let mut stmt = line.advance(next_stmt);
        preparse_stmt(stmt, target, stmts);
        line.advance(2);    // skip the separator
    }
    preparse_stmt(line, target, stmts);*/
}

pub fn preparse_stmt(mut stmt: ContextStr, target: &mut Target, stmts: &mut Vec<LocalContext>) {
    stmt.trim();
    stmts.push(stmt.local());
}
