pub mod expr;

use std::fmt::Display;

use crate::lexer::tokens::Token;

pub trait Parse: Sized {
    fn parse(stream: &[Token]) -> Result<(Self, &[Token]), ParseError>;
}

macro_rules! parse_error {
    ($code:literal, $id:ident) => {
        pub fn $id(tokens: &'t [Token], message: impl Into<String>) -> Self {
            Self {
                code: $code,
                message: message.into(),
                tokens,
            }
        }
    };

    ($code:literal, $id:ident, $message:literal) => {
        pub fn $id(tokens: &'t [Token]) -> Self {
            Self {
                code: $code,
                message: $message.into(),
                tokens,
            }
        }
    };
}

pub struct ParseError<'t> {
    pub code: u16,
    pub tokens: &'t [Token],
    pub message: String,
}

impl Display for ParseError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "error[E{:0>4}]: {}", self.code, self.message)?;
        write!(f, "-->\t")?;
        for token in self.tokens {
            write!(f, "{token} ")?;
        }
        Ok(())
    }
}

impl<'t> ParseError<'t> {
    parse_error!(1, end_of_input, "end of input");
    parse_error!(2, expected_literal, "expected literal");
    parse_error!(3, invalid_type);
    parse_error!(4, numerical_overflow, "number is too large");
    parse_error!(5, invalid_escape);
}

pub struct Parser;

impl Parser {
    pub fn parse(input: &[Token]) {}
}
