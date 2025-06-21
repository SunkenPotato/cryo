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

use cryo_lexer::tokens::TokenGroup;
use cryo_span::Span;
use stream::{TokenStream, TokenStreamGuard};

use std::ops::{Deref, DerefMut};

use crate::error::ParseError;

pub mod stream;

/// A reference to a specialized token.
pub struct SpecToken<'a, T: TokenGroup> {
    /// A reference to the specialized token.
    pub token: &'a T,
    /// The span this token lives at.
    pub span: Span,
}

/// A parser production or error with a [`Span`].
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Spanned<T> {
    /// The actual value.
    pub t: T,
    /// The span.
    pub span: Span,
}

impl<T> Spanned<T> {
    /// Create a new [`Spanned`]
    pub const fn new(t: T, span: Span) -> Self {
        Self { t, span }
    }

    /// Map a `Spanned<T>` into a `Spanned<U>` with the given closure.
    pub fn map<F, U>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned {
            t: f(self.t),
            span: self.span,
        }
    }

    /// Extend the current span with another span.
    ///
    /// View [`Span::extend`] for more information.
    pub fn extend<U>(&mut self, other: Spanned<U>) {
        self.span = self.span.extend(other.span)
    }

    /// Convert `self` to a tuple.
    pub fn as_tuple(self) -> (T, Span) {
        (self.t, self.span)
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.t
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.t
    }
}

/// The output of [`Parse::parse`].
pub type ParseResult<T> = Result<Spanned<T>, Box<dyn ParseError>>;

/// Trait for parsing operations.
///
/// Every type implementing this should be able to define and create itself from a
/// certain sequence of [`Token`]s.
pub trait Parse: Sized {
    /// Create an instance of `Self` from a certain number of tokens passed into this function.
    ///
    /// # Errors
    /// The error depends on the implementation of the trait.
    fn parse<'t>(stream: &mut TokenStreamGuard<'t>) -> ParseResult<Self>;
}

/// Assert a parser production matches the given one, as well as that the stream has been entirely consumed.
#[cfg(test)]
#[track_caller]
pub fn parse_assert<T: Parse + std::fmt::Debug + PartialEq>(stream: &mut TokenStream, expect: T) {
    match parse(stream) {
        Ok(Spanned { t, span: _ }) => assert_eq!(expect, t),
        Err(e) => panic!("{e}"),
    }

    assert!(stream.inner().is_empty());
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

    let len = stream.inner().len();

    match parse::<T>(stream) {
        Ok(v) => panic!("test at {} should have failed: {v:#?}", Location::caller()),
        Err(e) => assert!(*e == err),
    };

    let len2 = stream.inner().len();

    assert_eq!(len, len2);
}

/// Try to parse a [`TokenStream`] as `T`.
pub fn parse<T>(tokens: &mut TokenStream) -> ParseResult<T>
where
    T: Parse,
{
    tokens.with(T::parse)
}
