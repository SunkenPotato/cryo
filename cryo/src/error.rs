use std::{backtrace::Backtrace, panic::PanicHookInfo};

use cryo_lexer::LexicalError;
use cryo_parser::ParseError;

#[derive(Debug)]
#[expect(unused)]
pub enum CompileError {
    LexicalError(LexicalError),
    ParseError(ParseError),
}

pub fn set_panic_hook() {
    std::panic::set_hook(Box::new(panic_hook));
}

fn panic_hook(info: &PanicHookInfo) {
    eprintln!(
        "An internal compiler error (ICE) occurred. This is abnormal and should be reported."
    );

    eprintln!("ICE occurred at: {}", info.location().unwrap());
    eprintln!(
        "Info: {}",
        info.payload()
            .downcast_ref::<&str>()
            .unwrap_or(&"-- not available --")
    );
    eprintln!("Backtrace:\n {}", Backtrace::force_capture());
}
