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
