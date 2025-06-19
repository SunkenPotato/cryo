#![warn(elided_lifetimes_in_paths)]
//! Parser implementation for the cryo language.
//!
//! # What is a parser?
//! A parser turns tokens ("words", so to speak, view [`Token`](cryo_lexer::Token)) from a lexer into an abstract syntax tree (AST).
//!
//! The core of this crate is the [`Parse`] trait and the [`Parser`] struct.

#![warn(clippy::missing_errors_doc, clippy::missing_panics_doc)]
#![deny(missing_docs)]

pub mod error;
pub mod expr;

use cryo_lexer::tokens::{Token, TokenGroup};
use cryo_span::Span;

use std::{collections::VecDeque, panic::Location};

use crate::error::{GenericError, ParseError, SpannedGenericError};

/// A newtype wrapper around a [`VecDeque`] for parse operations.
#[derive(Debug, Clone)]
pub struct TokenStream {
    commands: usize,
    container: VecDeque<Token>,
}

/// A reference to a specialized token.
pub struct SpecToken<'a, T> {
    token: &'a T,
    span: Span,
}

impl TokenStream {
    /// Create a new stream from a [`VecDeque`].
    pub const fn new(container: VecDeque<Token>) -> Self {
        Self {
            container,
            commands: 0,
        }
    }

    /// Advance the stream, returning the first element in the stream if it exists.
    ///
    /// # Errors
    /// Returns an error if the stream is empty.
    #[track_caller]
    pub fn advance(&mut self) -> Result<&Token, SpannedGenericError> {
        self.container
            .front()
            .ok_or(SpannedGenericError::new(
                Span::EMPTY,
                GenericError::EndOfInput,
            ))
            .inspect(|_| self.commands += 1)
    }

    /// Advance the `TokenStream` and require the next token to be of `T`.
    ///
    /// Returns `Ok(T)` if the token is of `T`. \
    ///
    /// # Errors
    /// If the token is not present or has the incorrect type, the stream will not be advanced
    /// and return an error.
    #[track_caller]
    pub fn advance_require<T: 'static + TokenGroup>(
        &mut self,
    ) -> Result<SpecToken<'_, T>, SpannedGenericError> {
        let token = self.advance()?;

        let span = token.span;

        match token.require_ref::<T>() {
            Some(token) => Ok(SpecToken { token, span }),
            None => Err(SpannedGenericError::new(
                Span::EMPTY,
                GenericError::IncorrectToken(T::NAME, token.name()),
            )),
        }
    }

    /// Peek at the next token. This method does the same as `advance`,
    /// except when [`TokenStream::peek`] is called, the stream will not advance.
    pub fn peek(&self) -> Result<&Token, SpannedGenericError> {
        self.peek_n(0)
    }

    /// Peek at the n-th token ahead. View [`TokenStream::peek`] for more information.
    pub fn peek_n(&self, n: usize) -> Result<&Token, SpannedGenericError> {
        self.container.get(n).ok_or(SpannedGenericError::new(
            Span::EMPTY,
            GenericError::EndOfInput,
        ))
    }

    /// Apply all pending commands.
    pub fn sync(&mut self) {
        self.container.drain(..self.commands);
        self.commands = 0;
    }
}

/// Trait for parsing operations.
///
/// Every type implementing this should be able to define and create itself from a
/// certain sequence of [`Token`]s.
pub trait Parse: Sized {
    /// Create an instance of `Self` from a certain number of tokens passed into this function.
    ///
    /// # Errors
    /// The error depends on the implementation of the trait.
    fn parse(stream: &mut TokenStream) -> Result<Self, Box<dyn ParseError>>;
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
