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

use std::{
    collections::VecDeque,
    ops::{Deref, DerefMut},
};

use crate::error::{GenericError, ParseError, SpannedGenericError};

/// A newtype wrapper around a [`VecDeque`] for parse operations.
#[derive(Debug, Clone, Hash)]
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
    pub fn new(container: impl Into<VecDeque<Token>>) -> Self {
        Self {
            container: container.into(),
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

/// A parser production with a [`Span`].
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Spanned<T>(pub T, pub Span);

impl<T> Spanned<T> {
    /// Map a `Spanned<T>` into a `Spanned<U>` with the given closure.
    pub fn map<F, U>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned(f(self.0), self.1)
    }

    /// Extend the current span with another span.
    ///
    /// View [`Span::extend`] for more information.
    pub fn extend<U>(&mut self, other: Spanned<U>) {
        self.1 = self.1.extend(other.1)
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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
    fn parse(stream: &mut TokenStream) -> Result<Spanned<Self>, Box<dyn ParseError>>;
}

/// Assert a parser production matches the given one, as well as that the stream has been entirely consumed.
#[cfg(test)]
#[track_caller]
pub fn parse_assert<T: Parse + std::fmt::Debug + PartialEq>(stream: &mut TokenStream, expect: T) {
    match T::parse(stream) {
        Ok(Spanned(token, _)) => assert_eq!(expect, token),
        Err(e) => panic!("{e}"),
    }

    assert!(stream.container.is_empty());
}

/// Assert that a given type fails to parse the stream, as well as that the stream has not been consumed.
#[cfg(test)]
#[track_caller]
pub fn parse_assert_err<T, E>(stream: &mut TokenStream, err: E)
where
    T: Parse + std::fmt::Debug,
    E: std::fmt::Debug,
    dyn ParseError: PartialEq<E>,
{
    use std::panic::Location;

    let len = stream.container.len();

    match T::parse(stream) {
        Ok(v) => panic!("test at {} should have failed: {v:#?}", Location::caller()),
        Err(e) => assert!(*e == err),
    };

    let len2 = stream.container.len();

    assert_eq!(len, len2);
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
