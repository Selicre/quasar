use crate::context::ContextStr;

#[derive(Clone, Copy, Debug)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Debug
}

impl Severity {
    pub fn name(&self) -> &'static str {
        match self {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Info => "info",
            Severity::Debug => "debug",
        }
    }
    pub fn symbol(&self) -> &'static str {
        match self {
            Severity::Error => "E",
            Severity::Warning => "W",
            Severity::Info => "I",
            Severity::Debug => "D"
        }
    }
    pub fn color(&self) -> i32 {
        match self {
            Severity::Error => 9,
            Severity::Warning => 11,
            Severity::Info => 12,
            Severity::Debug => 10
        }
    }
}

#[derive(Debug)]
pub struct Message {
    source: ContextStr,
    code: usize,
    severity: Severity,
    data: String,
    help: Vec<String>
}

impl std::fmt::Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "\x1B[1;38;5;{}m{}", self.severity.color(), self.severity.name())?;
        if self.code != 0 {
            write!(f, "[{}{:04}]", self.severity.symbol(), self.code)?;
        }
        writeln!(f, "\x1B[38;5;15m: {}\x1B[0m", self.data)?;
        writeln!(f, "    => at {}", self.source.source().short())?;
        if self.source.source().line != 0 {
            writeln!(f, "{}", self.source.line_highlight(self.severity))?;
        }
        for i in self.help.iter() {
            writeln!(f, "    => help: {}", i)?;
        }
        Ok(())
    }
}

impl Message {
    pub fn is_error(&self) -> bool { matches!(self.severity, Severity::Error) }
    pub fn error(source: ContextStr, code: usize, data: String) -> Self {
        let severity = Severity::Error;
        Message {
            source, code, severity, data, help: vec![]
        }
    }
    pub fn with_help(mut self, msg: String) -> Self {
        self.help.push(msg);
        self
    }
    pub fn info(source: ContextStr, code: usize, data: String) -> Self {
        let severity = Severity::Info;
        Message {
            source, code, severity, data, help: vec![]
        }
    }
}
