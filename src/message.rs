use crate::context::ContextStr;
use std::cell::{RefCell, RefMut};
use std::fmt::Display;

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
#[derive(Default)]
pub struct MsgQueue {
    queue: Vec<Message>,
    has_error: bool,
}

impl MsgQueue {
    pub fn has_error() -> bool {
        MESSAGES.with(|c| {
            c.borrow().has_error
        })
    }
    pub fn drain(mut f: impl FnMut(Message)) {
        MESSAGES.with(|c| {
            for i in c.borrow_mut().queue.drain(..) {
                f(i);
            }
        })
    }
}

thread_local! {
    static MESSAGES: RefCell<MsgQueue> = RefCell::new(MsgQueue::default());
}

#[must_use = "you probably want to .push() this error"]
#[derive(Debug)]
pub struct Message {
    source: ContextStr,
    severity: Severity,
    data: String,
    help: Vec<String>
}

impl std::fmt::Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "\x1B[1;38;5;{}m{}", self.severity.color(), self.severity.name())?;
        /*if self.code != 0 {
            write!(f, "[{}{:04}]", self.severity.symbol(), self.code)?;
        }*/
        writeln!(f, "\x1B[38;5;15m: {}\x1B[0m", self.data)?;

        let mut cur = Some(self.source.clone());
        while let Some(c) = cur {
            writeln!(f, "    => at {}", c.source().short())?;
            if c.source().line != 0 {
                writeln!(f, "{}", c.line_highlight(self.severity))?;
            }
            cur = c.parent().cloned();
        }
        for i in self.help.iter() {
            writeln!(f, "    => help: {}", i)?;
        }
        Ok(())
    }
}

impl Message {
    pub fn is_error(&self) -> bool { matches!(self.severity, Severity::Error) }
    pub fn push(self) {
        MESSAGES.with(|c| {
            let mut c = c.borrow_mut();
            c.has_error |= self.is_error();
            c.queue.push(self)
        });
    }
    pub fn error(source: ContextStr, code: usize, data: String) -> Self {
        let severity = Severity::Error;
        Message {
            source, severity, data, help: vec![]
        }
    }
    pub fn warning(source: ContextStr, data: String) -> Self {
        let severity = Severity::Warning;
        Message {
            source, severity, data, help: vec![]
        }
    }
    pub fn info(source: ContextStr, data: String) -> Self {
        let severity = Severity::Info;
        Message {
            source, severity, data, help: vec![]
        }
    }
    pub fn debug(source: ContextStr, data: String) -> Self {
        let severity = Severity::Debug;
        Message {
            source, severity, data, help: vec![]
        }
    }
    pub fn with_help(mut self, msg: String) -> Self {
        self.help.push(msg);
        self
    }
}

pub mod errors {
    use super::*;
    macro_rules! err_list {
        ($($fnname:ident ($($args:tt)*) { $($msg:tt)* } $({ $($help:tt)* })*)*) => {
            $(
            pub fn $fnname(span: ContextStr, $($args)*) -> Message {
                Message::error(
                    span,
                    0,
                    format!($($msg)*)
                )
                $(.with_help(format!($($help)*)))*
            }
            )*
        }
    }
    // The messages are formatted largely as `topic_issue`
    err_list! {
        // files
        file_read(file: &str, err: impl Display) { "Can't open file {:?} for reading", file } { "{}", err }
        file_non_utf8(file: &str, err: impl Display) { "File {:?} is not utf-8", file } { "{}", err }

        // lexer
        numeric_overflow() { "Number overflows a 64-bit value" } { "quasar can only represent values as 64-bit IEEE floating point numbers" }
        str_literal_unclosed() { "Unclosed string literal" } { "add a quote character at the same line, or use \\ for multiline strings" }
        define_unclosed() { "Unclosed escaped define" }

        // defines
        define_unknown() { "Define not found in this scope" } { "add a define by using the `!define = value` syntax" }
        define_rec_limit() { "Define expansion limit reached" } { "your define might be recursive" }
        define_trailing_chars() { "Trailing characters at the end of define" }

        // ifs
        stray_elseif() { "Stray `elseif`" } { "change `elseif` to `if`" }
        stray_else() { "Stray `else`" } { "remove `else`" }
        stray_endif() { "Stray `endif`" } { "remove `endif` or put an appropriate `if` before it" }
        else_after_while(c: &str) { "`{}` after `while`", c } { "remove `{}`", c }
        if_no_condition(c: &str) { "`{}` with no condition", c } { "add a condition, like `{} 2+2 == 4`", c }

        // labels
        label_no_parent() { "Sublabel has no parent" }

        // macros
        macro_nested_def() { "Nested macro definition" } { "remove the nested macro" }
        macro_def_no_name() { "Macro definition with no name" } { "add a macro name, like `macro test()`" }
        macro_no_name() { "Macro call with no name" } { "add a macro name, like `%test()`" }
        macro_malformed() { "Malformed macro call" } { "macro calls have parentheses around arguments, like `%test(a,b)`" }

        // functions
        function_no_name() { "Function with no name" } { "add a function name, like `function test(a,b) = a+b`" }
        function_no_eq() { "Function requires an equal sign" } { "add an equals sign after the arguments, like `function test(a,b) = a+b`" }
        function_empty() { "Function is empty" } { "add a function body, like `function test(a,b) = a+b`" }

        // argument lists
        arglist_expected() { "Expected an argument list" } { "add a list of arguments" }
        arglist_unterminated() { "Unterminated argument list" } { "add a closing parenthesis to the argument list" }
        arglist_non_ident() { "Unexpected token in argument list" } { "arguments can only be identifiers" }

        // commands
        cmd_no_arg(cmd: &str, arg: &str, example: &str) { "`{}` command with no {}", cmd, arg } { "add an argument, like `{} {}`", cmd, example }
        cmd_unknown_sep() { "Unknown separator" }
        cmd_unknown() { "Unknown command" }

        // rom
        rom_unmapped() { "Address does not map to ROM" }
        rom_bank_crossed() { "This statement crosses a bank boundary" }
        rom_warnpc(addr: u32, warn: u32) { "warnpc fail: {:06X} > {:06X}", addr, warn }

        // expr
        expr_fn_arg_count(need: impl Display, have: usize) { "This function expects {} arguments, found {}", need, have }
        expr_read_file_oob() { "File read out of bounds" }
    }
}
