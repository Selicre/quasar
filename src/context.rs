use std::rc::Rc;
use std::fmt::Display;

#[derive(Clone, Debug)]
pub struct LineInfo {
    pub line: u32,
    pub col: u32,
    pub filename: Rc<str>
}
impl LineInfo {
    pub fn short(&self) -> impl Display + '_ {
        struct Formatter<'a>(&'a LineInfo);
        impl std::fmt::Display for Formatter<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                if self.0.line != 0 {
                    write!(f, "{}:{}:{}", self.0.filename, self.0.line, self.0.col)
                } else {
                    write!(f, "{}", self.0.filename)
                }
            }
        }
        Formatter(self)
    }
    fn advance(&mut self, by: &str) {
        for i in by.chars() {
            if i == '\n' {
                self.line += 1; self.col = 1;
            } else {
                self.col += 1;
            }
        }
    }
    pub fn file(filename: Rc<str>) -> Self {
        LineInfo {
            line: 1,
            col: 1,
            filename
        }
    }
    pub fn custom(source: String) -> Self {
        LineInfo {
            line: 0,
            col: 0,
            filename: source.into()
        }
    }
    pub fn cli() -> Self {
        LineInfo {
            line: 0,
            col: 0,
            filename: "<cli>".into()
        }
    }
}


// Context with known filename.
#[derive(Copy, Clone, Debug)]
pub struct LocalContext {
    pub start: usize,
    pub end: usize,
    pub line: u32,
    pub col: u32
}

pub struct Needle {
    start: usize,
    source: LineInfo
}

#[derive(Clone)]
pub struct ContextStr {
    full: Rc<str>,
    range: std::ops::Range<usize>,
    source: LineInfo,
    parent: Option<Box<ContextStr>>
}

impl ContextStr {
    pub fn new(full: String, source: LineInfo) -> Self {
        let range = 0..full.len();
        let full = full.into_boxed_str().into();
        ContextStr { full, source, range, parent: None }
    }
    pub fn empty() -> Self {
        let range = 0..0;
        let source = LineInfo::custom("<empty>".into());
        let full = "".into();
        ContextStr { full, source, range, parent: None }
    }
    pub fn set_parent(&mut self, parent: ContextStr) { self.parent = Some(Box::new(parent)); }
    pub fn parent(&self) -> Option<&ContextStr> {
        self.parent.as_deref()
    }
    pub fn cli() -> Self {
        ContextStr {
            full: "".into(),
            range: 0..0,
            source: LineInfo::cli(),
            parent: None,
        }
    }
    pub fn line_highlight(&self, severity: crate::message::Severity) -> String {
        // this whole thing is stupid and temporary
        let start = self.full[..self.range.start].rfind('\n').map(|c| c+1).unwrap_or(0);
        let end = self.full[self.range.end..].find('\n').map(|c| c + self.range.end).unwrap_or(self.full.len() - self.range.end);
        if self.range.start >= start && end >= self.range.end && self.range.end > self.range.start {
            let pad_start = " ".repeat(self.range.start - start);
            let pad_end = " ".repeat(end - self.range.end);
            let highlight = "^".repeat(self.range.end - self.range.start);
            let pad_line_num = " ".repeat(format!("{}", self.source.line).len());
            format!(" {} |\n {} | {}\n {} | \x1B[1;38;5;{}m{}{}{}\x1B[0m", pad_line_num, self.source.line, &self.full[start..end], pad_line_num, severity.color(), pad_start, highlight, pad_end)
        } else {
            format!("[formatting error: {}, {}] {:?} {:?}", start, end, &self.full[start..end], &self.full)
        }
    }
    pub fn local(&self) -> LocalContext {
        LocalContext {
            start: self.range.start,
            end: self.range.end,
            line: self.source.line,
            col: self.source.col
        }
    }
    pub fn slice_local(&self, local: LocalContext, filename: Rc<str>) -> ContextStr {
        ContextStr {
            full: self.full.clone(),
            range: local.start..local.end,
            source: LineInfo {
                line: local.line,
                col: local.col,
                filename,
            },
            parent: self.parent.clone(),
        }
    }
    pub fn start(&self) -> usize {
        self.range.start
    }
    pub fn source(&self) -> &LineInfo {
        &self.source
    }
    pub fn cut(&mut self, by: usize) -> ContextStr {
        let cx = ContextStr {
            full: self.full.clone(),
            range: self.range.end-by..self.range.end,
            source: self.source.clone(),
            parent: self.parent.clone(),
        };
        self.range.end -= by;
        cx
    }
    pub fn advance(&mut self, by: usize) -> ContextStr {
        let cx = ContextStr {
            full: self.full.clone(),
            range: self.range.start..self.range.start+by,
            source: self.source.clone(),
            parent: self.parent.clone(),
        };

        let start = self.range.start;
        self.range.start += by;
        let prefix = &self.full[start..][..by];
        self.source.advance(prefix);
        cx
    }
    pub fn advance_some(&mut self, by: Option<usize>) -> ContextStr {
        let by = if let Some(by) = by {
            by
        } else {
            self.range.end - self.range.start
        };
        self.advance(by)
    }
    pub fn advance_char(&mut self) -> Option<char> {
        self.chars().next().map(|c| { self.advance(c.len_utf8()); c })
    }
    pub fn prefix_from(&mut self, from: Needle) -> ContextStr {
        ContextStr {
            full: self.full.clone(),
            range: from.start..self.range.start,
            source: from.source,
            parent: self.parent.clone(),
        }
    }
    pub fn needle(&self) -> Needle {
        Needle {
            start: self.range.start,
            source: self.source.clone()
        }
    }
    pub fn skip_if(&mut self, src: &str) -> bool {
        if let Some(c) = self.strip_prefix(src) {
            let range = c.as_bytes().as_ptr_range();
            self.restrict_to(range);
            true
        } else {
            false
        }
    }
    fn restrict_to(&mut self, range: std::ops::Range<*const u8>) {
        let s = &*self;
        let s_range = s.as_bytes().as_ptr_range();
        let len = range.start as usize - s_range.start as usize;
        self.advance(len);
        self.range.end -= s_range.end as usize - range.end as usize;
    }
    pub fn apply(&mut self, f: impl FnOnce(&str) -> &str) {
        let sub = f(&*self);
        let range = sub.as_bytes().as_ptr_range();
        self.restrict_to(range);
    }
    pub fn trim(&mut self) {
        self.apply(|c| c.trim())
    }
}
impl std::fmt::Display for ContextStr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.pad(&self)
    }
}
impl std::fmt::Debug for ContextStr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use std::fmt::Write;
        write!(f, "\"{}\"", self.escape_debug())
    }
}

impl std::ops::Deref for ContextStr {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.full[self.range.clone()]
    }
}
/*impl std::cmp::PartialEq<str> for ContextStr {
    fn eq(&self, other: &str) -> bool {
        &*self == other
    }
}*/
