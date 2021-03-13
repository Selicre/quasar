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

#[derive(Clone, Debug)]
pub struct ContextStr {
    full: Rc<str>,
    range: std::ops::Range<usize>,
    source: LineInfo,
}

impl ContextStr {
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
                filename
            }
        }
    }
    pub fn source(&self) -> &LineInfo {
        &self.source
    }
    pub fn new(full: String, source: LineInfo) -> Self {
        let range = 0..full.len();
        let full = full.into_boxed_str().into();
        ContextStr { full, source, range }
    }
    pub fn advance(&mut self, by: usize) -> ContextStr {
        let cx = ContextStr {
            full: self.full.clone(),
            range: self.range.start..self.range.start+by,
            source: self.source.clone()
        };

        let start = self.range.start;
        self.range.start += by;
        let prefix = &self.full[start..][..by];
        self.source.advance(prefix);
        cx
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

impl std::ops::Deref for ContextStr {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.full[self.range.clone()]
    }
}
