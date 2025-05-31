pub mod expr;

use std::fmt::{Debug, Display};

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
        #[must_use]
        pub fn $id(tokens: &'t [Token]) -> Self {
            Self {
                code: $code,
                message: $message.into(),
                tokens,
            }
        }
    };
}

#[derive(Debug)]
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
    parse_error!(6, expected_operation, "expected arithmetic operation");
    parse_error!(7, invalid_token);
}

impl PartialEq for ParseError<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.code == other.code
    }
}

impl Eq for ParseError<'_> {}

pub struct Parser;

impl Parser {
    pub fn parse(mut input: &[Token]) {
        todo!()
    }
}
