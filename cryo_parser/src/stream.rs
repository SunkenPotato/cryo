//! Containers for passing tokens to parser functions.

use std::collections::VecDeque;

use cryo_lexer::tokens::{Token, TokenGroup};
use cryo_span::Span;

use crate::{Spanned, SpecToken, error::GenericError};

/// A newtype wrapper around a [`VecDeque`] for parse operations.
#[derive(Debug, Clone, Hash)]
pub struct TokenStream {
    container: VecDeque<Token>,
}

impl TokenStream {
    /// Create a new `TokenStream` from some tokens.
    pub fn new(stream: impl Into<VecDeque<Token>>) -> Self {
        Self {
            container: stream.into(),
        }
    }

    /// Obtain access to the [`TokenStream`] via a closure.
    ///
    /// If the closure returns `Ok`, the changes made by the closure to the stream will be applied. \
    /// Otherwise, the changes will be discarded.
    pub fn with<F, T, E>(&mut self, f: F) -> Result<T, E>
    where
        for<'a> F: FnOnce(&mut TokenStreamGuard<'a>) -> Result<T, E>,
    {
        let mut guard = TokenStreamGuard {
            stream: self,
            cursor: 0,
        };

        let result = f(&mut guard);
        let cursor = guard.cursor;
        drop(guard);

        if result.is_ok() {
            self.container.drain(..cursor);
        }

        result
    }

    /// Obtain a reference to the inner `VecDeque`.
    pub const fn inner(&self) -> &VecDeque<Token> {
        &self.container
    }
}

#[derive(Debug)]
/// Access to a [`TokenStream`].
pub struct TokenStreamGuard<'t> {
    stream: &'t mut TokenStream,
    cursor: usize,
}

impl<'t> TokenStreamGuard<'t> {
    const END_OF_INPUT: Spanned<GenericError> = Spanned {
        t: GenericError::EndOfInput,
        span: Span::EMPTY,
    };

    /// Advance the stream, returning the first element in the stream if it exists.
    ///
    /// # Errors
    /// Returns an error if the stream is empty.
    #[track_caller]
    pub fn advance(&mut self) -> Result<&Token, Spanned<GenericError>> {
        self.stream
            .container
            .get(self.cursor)
            .ok_or(Self::END_OF_INPUT)
            .inspect(|_| self.cursor += 1)
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
    ) -> Result<SpecToken<'_, T>, Spanned<GenericError>> {
        let token = self.advance()?;
        let span = token.span;

        match token.require_ref::<T>() {
            Some(token) => Ok(SpecToken { token, span }),
            None => Err(Self::END_OF_INPUT),
        }
    }

    /// Look at the next token without advancing the stream.
    pub fn peek(&self) -> Result<&Token, Spanned<GenericError>> {
        self.peek_n(0)
    }

    /// Look at the n-th token in the stream without advancing it.
    pub fn peek_n(&self, n: usize) -> Result<&Token, Spanned<GenericError>> {
        self.stream.container.get(n).ok_or(Self::END_OF_INPUT)
    }

    /// Create a checkpoint-state before calling the closure, useful for
    /// parsing components which try to parse another component if the current fails.
    pub fn with<F, T, E>(&mut self, f: F) -> Result<T, E>
    where
        F: FnOnce(&mut Self) -> Result<T, E>,
    {
        let saved_cursor = self.cursor;
        let result = f(self);

        if result.is_err() {
            self.cursor = saved_cursor;
        }

        result
    }
}
