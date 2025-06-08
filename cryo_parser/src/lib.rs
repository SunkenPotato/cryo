//! Parser implementation for the cryo language.
//!
//! # What is a parser?
//! A parser turns tokens ("words", so to speak, view [`Token`](cryo_lexer::Token)) from a lexer into an abstract syntax tree (AST).
//!
//! The core of this crate is the [`Parse`] trait and the [`Parser`] struct.

#![warn(clippy::missing_errors_doc, clippy::missing_panics_doc)]
#![deny(missing_docs)]
#![forbid(unsafe_code)]

pub mod expr;

use cryo_lexer::tokens::Token;
use cryo_span::Span;

use std::fmt::{Debug, Display};

pub struct TokenStream(Vec<Token>);

impl TokenStream {
    pub const fn new(t: Vec<Token>) -> Self {
        Self(t)
    }

    pub fn peek(&self) -> Option<&Token> {
        self.0.first()
    }

    pub fn advance(&mut self) -> Option<Token> {
        self.0.pop()
    }

    pub fn peek_require<T: 'static>(&self) -> Option<&T> {
        self.peek().map(Token::require_ref).flatten()
    }

    pub fn advance_require<T: 'static>(&mut self) -> Result<T, Option<Token>> {
        let inter = self.advance();

        match inter {
            Some(v) => v.require().map_err(|_| None),
            None => return Err(None),
        }
    }
}

pub trait Parse: Sized {
    fn parse(stream: &mut TokenStream) -> Result<Self, ParseError>;
}

#[derive(Debug)]
pub struct ParseError<'a> {
    span: Span,
    message: &'a str,
    code: u32,
}

impl<'a> ParseError<'a> {
    pub const fn new(span: Span, message: &'a str, code: u32) -> Self {
        Self {
            span,
            message,
            code,
        }
    }
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "error[E{:0<5}]: {}", self.code, self.message);
        write!(f, "--> {}", self.span);

        Ok(())
    }
}

/// Parses a list of tokens into an AST.
pub struct Parser {
    #[expect(unused)]
    tokens: Vec<Token>,
}

impl Parser {
    /// Create a new parser from a list of tokens.
    pub const fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    /// Consume the tokens passed and generate an abstract syntax tree.
    pub fn parse(self) -> () {
        todo!()
    }
}
