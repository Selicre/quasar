use crate::context::{LineInfo, ContextStr};
use crate::executor::Target;
use crate::message::errors;
use std::borrow::Cow;

#[derive(Debug, Clone)]
pub struct Token {
    pub span: ContextStr,
    pub kind: TokenKind
}

impl Token {
    pub fn is_define(&self) -> bool {
        matches!(self.kind, TokenKind::Define { .. })
    }
    pub fn as_define(&self) -> Option<ContextStr> {
        match self.kind {
            TokenKind::Define { escaped } => Some(define_name(&self.span, escaped)),
            _ => None
        }
    }
    pub fn is_macro_arg(&self) -> bool {
        matches!(self.kind, TokenKind::MacroArg)
    }
    pub fn as_macro_arg(&self) -> Option<ContextStr> {
        match self.kind {
            TokenKind::MacroArg => {
                let mut d = self.span.clone();
                d.advance(1);
                d.cut(1);
                Some(d)
            },
            _ => None
        }
    }
    pub fn is_define_escaped(&self) -> Option<bool> {
        match self.kind {
            TokenKind::Define { escaped } => Some(escaped),
            _ => None
        }
    }
    pub fn is_whitespace(&self) -> bool {
        matches!(self.kind, TokenKind::Whitespace { .. })
    }
    pub fn is_non_nl_wsp(&self) -> bool {
        matches!(self.kind, TokenKind::Whitespace { newline: false })
    }
    pub fn is_newline(&self) -> bool {
        matches!(self.kind, TokenKind::Whitespace { newline: true })
    }
    pub fn is_symbol(&self) -> bool {
        matches!(self.kind, TokenKind::Symbol)
    }
    pub fn is_string(&self) -> bool {
        matches!(self.kind, TokenKind::String)
    }
    pub fn is_ident(&self) -> bool {
        matches!(self.kind, TokenKind::Ident)
    }
    pub fn is_decimal(&self) -> bool {
        matches!(self.kind, TokenKind::Number { radix: 10, .. })
    }
    pub fn as_char(&self) -> Option<char> {
        match self.kind {
            TokenKind::Char => Some(self.span.chars().nth(1).unwrap()),
            _ => None
        }
    }
    pub fn as_number(&self) -> Option<i64> {
        match self.kind {
            TokenKind::Number { value, .. } => Some(value),
            _ => None
        }
    }
    pub fn as_string(&self) -> Option<ContextStr> {
        if self.is_string() {
            Some(self.span.clone())
        } else {
            None
        }
    }
    pub fn as_string_mut(&mut self) -> Option<&mut ContextStr> {
        if self.is_string() {
            Some(&mut self.span)
        } else {
            None
        }
    }
    pub fn from_number(src: ContextStr, value: i64) -> Self {
        Token {
            span: src,
            kind: TokenKind::Number { value, length: 0, radix: 10 }
        }
    }
    pub fn eq(&self, other: &str) -> bool { self.span.eq(other) }
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    Number {        // $1234 or 1234
        value: i64,
        length: usize,
        radix: i32
    },
    Ident,          // thing
    Symbol,         // & or +=
    String,         // "thing"
    Char,
    MacroArg,       // <thing>
    Define { escaped: bool }, // !thing or !{thing}
    Whitespace { newline: bool },
}

pub fn tokenize_stmt(mut input: ContextStr, target: &mut Target, in_macro: bool) -> Vec<Token> {
    let is_ident_start = |c: char| c.is_alphabetic() || c == '_';
    let is_ident = |c: char| c.is_alphanumeric() || c == '_';
    let is_number_start = |c: char| c.is_numeric() || c == '$' || c == '%';
    let is_hex_number = |c: char| c.is_ascii_hexdigit();
    let is_number = |c: char| c.is_numeric();

    let mut out = vec![];
    while let Some(c) = input.chars().next() {
        if c == ';' {
            // eat comments
            input.advance_some(input.find(|c| c == '\n'));
            continue;
        }
        if c == '<' {
            let mut peek = input.clone();
            let needle = peek.needle();
            let open = peek.advance(1);
            let span = peek.advance_some(peek.find(|c| !is_ident(c)));
            let close = peek.advance(1);
            if &*close == ">" {
                input = peek;
                let span = input.prefix_from(needle);
                out.push(Token { span, kind: TokenKind::MacroArg });
                continue;
            }
        }   // try to parse macro arg
        if is_ident_start(c) {
            let span = input.advance_some(input.find(|c| !is_ident(c)));
            out.push(Token { span, kind: TokenKind::Ident });
        } else if is_number_start(c) {
            let needle = input.needle();
            let num;
            let radix;
            if input.skip_if("0x") || input.skip_if("$") {
                num = input.advance_some(input.find(|c| !is_hex_number(c)));
                radix = 16;
            } else if input.skip_if("0b") || input.skip_if("%") {
                num = input.advance_some(input.find(|c| c != '0' && c != '1'));
                radix = 2;
            } else {
                num = input.advance_some(input.find(|c| !is_number(c)));
                radix = 10;
                // Is this a malformed number?
                if let Some(c) = input.chars().next() {
                    if is_ident(c) {
                        input.advance_some(input.find(|c| !is_ident(c)));
                        let span = input.prefix_from(needle);
                        out.push(Token { span, kind: TokenKind::Ident });
                        continue;
                    }
                }
            }
            let mut value = 0i64;
            let mut overflowed = false;
            // todo: not garbage?
            for i in num.chars() {
                let (v,b) = value.overflowing_mul(radix as i64);
                overflowed |= b;
                value = v;
                let (v,b) = value.overflowing_add(i.to_digit(radix as u32).unwrap() as i64);
                overflowed |= b;
                value = v;
            }
            let span = input.prefix_from(needle);
            if overflowed {
                errors::numeric_overflow(span.clone()).push();
                return vec![]
            }
            if num.len() == 0 {
                out.push(Token { span, kind: TokenKind::Symbol });
            } else {
                out.push(Token { span, kind: TokenKind::Number { value, radix, length: num.len() } });
            }
        } else if c.is_whitespace() {
            let span = input.advance_some(input.find(|c: char| !c.is_whitespace()));
            let mut temp = Token {
                span: ContextStr::empty(),
                kind: TokenKind::Whitespace { newline: false }
            };
            let last = out.last_mut().unwrap_or(&mut temp);
            if let TokenKind::Whitespace { ref mut newline } = &mut last.kind {
                // coerce to the newline
                *newline |= span.contains("\n");
            } else if last.span.eq(",") {
                // commas do not act as newlines
                out.push(Token { span, kind: TokenKind::Whitespace { newline: false }});
            } else {
                let newline = span.contains("\n");
                out.push(Token { span, kind: TokenKind::Whitespace { newline }});
            }
        } else if c == '"' {
            let needle = input.needle();
            input.advance(1);
            loop {
                if let Some(c) = input.advance_char() {
                    match c {
                        '\\' => { input.advance_char(); }
                        '"' => if input.skip_if("\"") { continue; } else { break },
                        _ => {}
                    }
                } else {
                    errors::str_literal_unclosed(input.clone()).push();
                    return vec![]
                }
            }
            let span = input.prefix_from(needle);
            out.push(Token { span, kind: TokenKind::String });
        } else if c == '!' && !input.starts_with("!=") {
            if input.starts_with("!<") {
                let span = input.advance(1);
                out.push(Token { span, kind: TokenKind::Define { escaped: false } });
            } else if let Some((span, escaped)) = parse_define(&mut input, target) {
                out.push(Token { span, kind: TokenKind::Define { escaped } });
            } else {
                return out;
            }
        } else if c == '\\' {
            let needle = input.needle();
            if input.skip_if("\\!") { //|| input.skip_if("\\\\") {
                let span = input.prefix_from(needle);
                out.push(Token { span, kind: TokenKind::Symbol });
            } else if input.skip_if("\\\n") || input.skip_if("\\\r\n") {
                // do nothing
            } else {
                let span = input.advance(1);
                // see if there's a comment on this line
                let mut peek = input.clone();
                let wsp = peek.advance_some(peek.find(|c: char| !c.is_whitespace()));
                if peek.skip_if(";") {
                    peek.advance_some(peek.find(|c| c == '\n'));
                    peek.advance(1);
                    input = peek;
                } else {
                    out.push(Token { span, kind: TokenKind::Symbol });
                }
                //Message::warning(span.clone(), format!("Stray backslash"));
            }
        } else if c == ',' {
            let span = input.advance(1);
            out.push(Token { span, kind: TokenKind::Symbol });
            // see if there's a comment on this line
            let mut peek = input.clone();
            let wsp = peek.advance_some(peek.find(|c: char| !c.is_whitespace()));
            if peek.skip_if(";") {
                peek.advance_some(peek.find(|c| c == '\n'));
                peek.advance(1);
                input = peek;
            }
        } else if c == '\'' {
            let span = input.advance(3);
            out.push(Token { span, kind: TokenKind::Char });
        } else if c == '+' || c == '-' || c == '.' {
            let needle = input.needle();
            if input.skip_if("+=") {
                let span = input.prefix_from(needle);
                out.push(Token { span, kind: TokenKind::Symbol });
            } else {
                let span = input.advance_some(input.find(|n: char| n != c));
                out.push(Token { span, kind: TokenKind::Symbol });
            }
        } else {
            let tokens = [
                "#=", "**", "<:",
                ":=", "?=", ";@",
                "+=",
                // Java tokens, reserved for ease of use
                "<<=", ">>=",
                "::", "->", "==", ">=", "<=",
                "!=", "&&", "||", // "++", "--",
                "<<", ">>", "-=", "*=",
                "/=", "&=", "|=", "^=", "%=",
                ""
            ];
            let needle = input.needle();
            for &i in &tokens {
                if i.len() == 0 {
                    input.advance(c.len_utf8());
                } else if input.skip_if(i) {
                    break;
                }
            }
            let span = input.prefix_from(needle);
            out.push(Token { span, kind: TokenKind::Symbol });
        }
    }
    out
}

fn parse_define(input: &mut ContextStr, target: &mut Target) -> Option<(ContextStr, bool)> {
    let is_ident = |c: char| c.is_alphanumeric() || c == '_';
    let needle = input.needle();
    let escaped = input.skip_if("!{");
    if escaped {
        let mut layers = 0;
        let end = if let Some(c) = input.chars().position(|c| match c {
            '{' => { layers += 1; false },
            '}' if layers > 0 => { layers -= 1; false },
            '}' => true,
            _ => false
        }) {
            c
        } else {
            errors::define_unclosed(input.clone()).push();
            return None;
        };
        input.advance(end+1);
    } else {
        input.advance(1);
        input.advance_some(input.find(|c| !is_ident(c)));
    }
    Some((input.prefix_from(needle), escaped))
}
fn define_name(input: &ContextStr, escaped: bool) -> ContextStr {
    let mut i = input.clone();
    if escaped {
        i.advance(2);
        i.cut(1);
    } else {
        i.advance(1);
    }
    i
}
pub fn expand_str(mut input: ContextStr, target: &mut Target) -> Option<String> {
    let orig = input.clone();
    let mut out = String::new();

    while let Some(mut c) = input.chars().next() {
        let next_ident = input.chars().nth(1).map(|c| c.is_alphanumeric() || c == '_' || c == '{').unwrap_or(false);
        if c == '!' && next_ident {
            let (s, escaped) = parse_define(&mut input, target)?;
            let name = define_name(&s, escaped);
            let temp;
            let name_s = if escaped {
                temp = expand_str(name.clone(), target)?;
                &temp
            } else { &*name };
            if let Some(value) = target.defines().get(name_s) {
                let mut value = Cow::from(value.to_vec());
                let res = crate::executor::expand_defines(&mut value, &orig, target);
                if let Err(e) = res { e.push(); return None; }
                let value = value.iter().map(|c| &*c.span).collect::<Vec<_>>().concat();
                out.push_str(&value);
                continue;
            } else {
                errors::define_unknown(name.clone()).push();
                return None;
            }
        }
        input.advance_char();
        out.push(c);
        if c == '\\' {
            let c = input.advance_char().unwrap();
            out.push(c);
        }
    }
    Some(out)
}
// Assumes the string has already been expanded, thus no error handling
pub fn display_str(input: &str) -> String {
    let mut out = String::new();
    let mut iter = input[1..input.len()-1].chars();
    while let Some(c) = iter.next() {
        if c == '\\' || c == '\"' {
            let c = iter.next().expect("uh oh");
            out.push(c);
        } else {
            out.push(c);
        }
    }
    out
}

#[derive(Clone)]
pub struct TokenList<'a> {
    inner: &'a [Token],
    pos: usize
}

impl<'a> TokenList<'a> {
    pub fn new(inner: &'a [Token]) -> Self {
        Self { inner, pos: 0 }
    }
    pub fn next_non_wsp(&mut self) -> Option<&'a Token> {
        while self.inner.get(self.pos)?.is_whitespace() { self.pos += 1; }
        let res = self.inner.get(self.pos);
        self.pos += 1;
        res
    }
    pub fn prev(&self) -> Option<&'a Token> {
        self.inner.get(self.pos.saturating_sub(1))
    }
    pub fn next(&mut self) -> Option<&'a Token> {
        let res = self.inner.get(self.pos);
        self.pos += 1;
        res
    }
    pub fn peek(&mut self) -> Option<&'a Token> {
        self.clone().next()
    }
    pub fn peek_non_wsp(&mut self) -> Option<&'a Token> {
        self.clone().next_non_wsp()
    }
    pub fn rest(&self) -> &'a [Token] {
        &self.inner[self.pos..]
    }
    pub fn last(&self) -> Option<&'a Token> {
        self.inner.last()
    }
    pub fn pos(&self) -> usize { self.pos }
    pub fn seek(&mut self, pos: usize) { self.pos = pos; }
    pub fn split_off(&mut self, newline: bool) -> Option<(TokenList<'a>, bool)> {  // tokens, has newline
        if self.pos == self.inner.len() { return None; }
        /*
        let c1 = self.inner[self.pos..].windows(3)
            .position(|i| i[0].is_non_nl_wsp() && &*i[1].span == ":" && i[2].is_non_nl_wsp())
            .map(|c| (c, c+2, false));
        let c2 = self.inner[self.pos..].iter().position(|i| i.is_newline()).map(|c| (c,c+1, true));
        let v = c1.into_iter().chain(c2.into_iter()).min();
        */
        let mut v = self.inner[self.pos..].windows(3).enumerate().find_map(|(c,i)| {
            if i[0].is_newline() {
                Some((c, c+1, true))
            } else if i[0].is_non_nl_wsp() && &*i[1].span == ":" && i[2].is_non_nl_wsp() {
                Some((c, c+2, false))
            } else {
                None
            }
        });
        let l = self.inner.len();
        if l > 2 && v.is_none() {
            if self.inner[l-2].is_newline() {
                let c = l-2-self.pos;
                v = Some((c, c+1, true))
            } else if self.inner[l-1].is_newline() {
                let c = l-1-self.pos;
                v = Some((c, c+1, true))
            }
        }
        if let Some((c,e,nl)) = v {
            let pos = self.pos;
            let sp = TokenList { inner: &self.inner[..pos+c], pos };
            self.pos += e;
            Some((sp, nl))
        } else {
            let pos = self.pos;
            let sp = TokenList { inner: &self.inner[..], pos };
            self.pos = self.inner.len();
            Some((sp, newline))
        }
    }
}

