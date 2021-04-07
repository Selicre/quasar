use crate::context::{LineInfo, ContextStr};
use crate::executor::Target;
use crate::message::Message;

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
        matches!(self.kind, TokenKind::Whitespace)
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
    Whitespace
}

pub fn tokenize_stmt(mut input: ContextStr, target: &mut Target, in_macro: bool) -> Vec<Token> {
    let is_ident_start = |c: char| c.is_alphabetic() || c == '_';
    let is_ident = |c: char| c.is_alphanumeric() || c == '_';
    let is_number_start = |c: char| c.is_numeric() || c == '$' || c == '%';
    let is_hex_number = |c: char| c.is_ascii_hexdigit();
    let is_number = |c: char| c.is_numeric();

    let mut out = vec![];
    while let Some(c) = input.chars().next() {
        if c == '<' {
            let mut peek = input.clone();
            let needle = peek.needle();
            let open = peek.advance(1);
            let span = peek.advance_some(peek.find(|c| !is_ident(c)));
            let close = peek.advance(1);
            if &*close == ">" {
                input = peek;
                let span = input.prefix_from(needle);
                if in_macro {
                    out.push(Token { span, kind: TokenKind::MacroArg });
                    continue;
                } else {
                    target.push_error(span.clone(), 0, format!("Macro token outside of macro"));
                    return vec![]
                }
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
                target.push_error(span.clone(), 3, format!("Number overflows a 64-bit value"));
                return vec![]
            }
            if num.len() == 0 {
                out.push(Token { span, kind: TokenKind::Symbol });
            } else {
                out.push(Token { span, kind: TokenKind::Number { value, radix, length: num.len() } });
            }
        } else if c.is_whitespace() {
            let span = input.advance_some(input.find(|c: char| !c.is_whitespace()));
            out.push(Token { span, kind: TokenKind::Whitespace });
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
                    target.push_error(input.clone(), 5, format!("Unclosed string literal"));
                    return vec![]
                }
            }
            let span = input.prefix_from(needle);
            out.push(Token { span, kind: TokenKind::String });
        } else if c == '!' {
            if let Some((span, escaped)) = parse_define(&mut input, target) {
                out.push(Token { span, kind: TokenKind::Define { escaped } });
            } else {
                return out;
            }
        } else if c == '\\' {
            let needle = input.needle();
            if input.skip_if("\\!") { //|| input.skip_if("\\\\") {
                let span = input.prefix_from(needle);
                out.push(Token { span, kind: TokenKind::Symbol });
            } else if input.skip_if("\\\n") {
                // do nothing
            } else {
                let span = input.advance(1);
                target.push_warning(span.clone(), 10, format!("Stray backslash"));
                out.push(Token { span, kind: TokenKind::Symbol });
            }
        } else if c == '+' || c == '-' || c == '.' {
            let span = input.advance_some(input.find(|n: char| n != c));
            out.push(Token { span, kind: TokenKind::Symbol });
        } else {
            let tokens = [
                "#=", "**", "<:",
                ":=", "?=", ";@",
                // Java tokens, reserved for ease of use
                "<<=", ">>=",
                "::", "->", "==", ">=", "<=",
                "!=", "&&", "||", // "++", "--",
                "<<", ">>", "+=", "-=", "*=",
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
            target.push_error(input.clone(), 6, format!("Unclosed escaped define"));
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
                let mut value = value.clone();
                crate::executor::expand_defines(&mut value, &orig, target);
                let value = value.iter().map(|c| &*c.span).collect::<Vec<_>>().concat();
                out.push_str(&value);
                continue;
            } else {
                target.push_msg(Message::error(name.clone(), 8, format!("Define {:?} not found in this scope", name_s)));
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
    pub fn split_off(&mut self) -> Option<TokenList<'a>> {
        self.inner[self.pos..].windows(3)
            .position(|i| i[0].is_whitespace() && &*i[1].span == ":" && i[2].is_whitespace())
            .map(|c| {
                let pos = self.pos;
                let sp = TokenList { inner: &self.inner[..pos+c], pos };
                self.pos += c;
                sp
            })
    }
}

