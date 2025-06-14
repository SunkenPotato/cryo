//! Parser implementation for the cryo language.
//!
//! # What is a parser?
//! A parser turns tokens ("words", so to speak, view [`Token`](cryo_lexer::Token)) from a lexer into an abstract syntax tree (AST).
//!
//! The core of this crate is the [`Parse`] trait and the [`Parser`] struct.

#![warn(clippy::missing_errors_doc, clippy::missing_panics_doc)]
#![deny(missing_docs)]

pub mod expr;

use cryo_lexer::tokens::{Token, TokenGroup};
use cryo_span::Span;

use std::{
    borrow::Cow,
    collections::VecDeque,
    fmt::{Debug, Display},
};

/// A newtype wrapper around a [`VecDeque`] for parse operations.
pub struct TokenStream(VecDeque<Token>);

impl TokenStream {
    /// Create a new stream from a [`VecDeque`].
    pub const fn new(t: VecDeque<Token>) -> Self {
        Self(t)
    }

    /// Peek into the stream, retrieving the first element as a reference if it exists.
    pub fn peek(&self) -> Option<&Token> {
        self.0.front()
    }

    /// Advance the stream, returning the first element in the stream if it exists.
    pub fn advance(&mut self) -> Result<Token, ParseError> {
        self.0.pop_front().ok_or(ParseError::END_OF_INPUT)
    }

    /// Peek into the stream and check if the next token is of `T`, if some.
    ///
    /// Returns `None` if the token is not of `T` or doesn't exist.
    ///
    /// See also: [`peek`]
    pub fn peek_require<T: 'static + TokenGroup>(&self) -> Option<&T> {
        self.peek().map(Token::require_ref).flatten()
    }

    /// Advance the `TokenStream` and require the next token to be of `T`.
    ///
    /// Returns `Ok(T)` if the token is of `T`. \
    /// If the token is not present or has the incorrect type, the stream will not be advanced
    /// and the function will return `Err(())`.
    pub fn advance_require<T: 'static + TokenGroup>(&mut self) -> Result<(T, Span), ParseError> {
        let reference = self.peek().ok_or(ParseError::END_OF_INPUT)?;

        if !reference.is::<T>() {
            return Err(ParseError::wrong_token::<T>(reference));
        }

        let span = reference.span;

        // SAFETY: it has been checked if the stream contains the next elements
        // and that the token type is `T`.
        unsafe {
            Ok((
                self.advance()
                    .unwrap_unchecked()
                    .require::<T>()
                    .unwrap_unchecked(),
                span,
            ))
        }
    }
}

/// Trait for parsing operations.
///
/// Every type implementing this should be able to define and create itself from a
/// certain sequence of [`Token`]s.
pub trait Parse: Sized {
    /// Create an instance of `Self` from a certain number of tokens passed into this function.
    fn parse(stream: &mut TokenStream) -> Result<Self, ParseError>;
}

/// Represents a parse error.
///
/// Contains a span pointing to where the problematic tokens are in the input,
/// an error message, and a code.
#[derive(Debug)]
pub struct ParseError<'a> {
    span: Span,
    message: Cow<'a, str>,
    code: u32,
}

impl ParseError<'static> {
    /// The parser received an unexpected end of input.
    pub const END_OF_INPUT: Self = Self::new_slice(Span::EMPTY, "unexpected end of input", 0);

    /// The parser received the wrong token.
    pub fn wrong_token<T: TokenGroup>(recv: &Token) -> Self {
        Self::new_owned(
            recv.span,
            format!(
                "invalid token: expected '{}', got '{}'",
                T::NAME,
                recv.name()
            ),
            1,
        )
    }

    /// An invalid type was received (e.g., string literal instead of number literal).
    pub fn wrong_type(span: Span, ex: &'static str, re: &'static str) -> Self {
        let message = format!("invalid type: expected '{}', got '{}'", ex, re);

        Self::new_owned(span, message, 2)
    }

    /// Literal integer overflow.
    pub const fn integer_overflow(span: Span) -> Self {
        Self::new_slice(span, "integer overflow", 3)
    }

    /// An invalid character was escaped / there is no escape character.
    pub const fn invalid_escape(span: Span) -> Self {
        Self::new_slice(span, "invalid/no escape character", 4)
    }
}

impl<'a> ParseError<'a> {
    /// Create a [`ParseError`] with a string slice message.
    pub const fn new_slice(span: Span, message: &'a str, code: u32) -> Self {
        Self {
            span,
            message: Cow::Borrowed(message),
            code,
        }
    }

    /// Create a [`ParseError`] with an owned string message.
    pub fn new_owned(span: Span, message: String, code: u32) -> Self {
        Self {
            span,
            message: Cow::Owned(message),
            code,
        }
    }
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "error[E{:0<5}]: {}", self.code, self.message)?;
        write!(f, "--> {}", self.span)?;

        Ok(())
    }
}

impl PartialEq for ParseError<'_> {
    fn eq(&self, other: &Self) -> bool {
        (self.code == other.code) && (self.span == other.span)
    }
}

impl Eq for ParseError<'_> {}

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
