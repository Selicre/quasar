use crate::context::LineInfo;

#[derive(Debug)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Debug
}

#[derive(Debug)]
pub struct Message {
    source: LineInfo,
    code: usize,
    severity: Severity,
    data: String,
}

impl std::fmt::Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} {:04} at {}\n\t{}", self.severity, self.code, self.source.short(), self.data)
    }
}

impl Message {
    pub fn error(source: LineInfo, code: usize, data: String) -> Self {
        let severity = Severity::Error;
        Message {
            source, code, severity, data
        }
    }
    pub fn info(source: LineInfo, code: usize, data: String) -> Self {
        let severity = Severity::Info;
        Message {
            source, code, severity, data
        }
    }
}
