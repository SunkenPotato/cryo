use std::array;

use cryo_span::Span;

use crate::{FromToken, Token};

pub struct TokenStream<'source> {
    inner: Vec<Token<'source>>,
}

impl<'source> TokenStream<'source> {
    pub fn new(s: Vec<Token<'source>>) -> Self {
        Self { inner: s }
    }

    pub fn with<F, T, E>(&mut self, f: F) -> Result<T, E>
    where
        for<'stream> F: FnOnce(&mut TokenStreamGuard<'stream, 'source>) -> Result<T, E>,
    {
        let mut guard = TokenStreamGuard {
            stream: self,
            cursor: 0,
        };

        let result = f(&mut guard);
        let cursor = guard.cursor;

        if result.is_ok() {
            self.inner.drain(..cursor);
        }

        result
    }
}

pub enum TokenStreamError {
    EndOfInput,
    IncorrectToken(&'static str, Span),
}

const EOI: TokenStreamError = TokenStreamError::EndOfInput;

pub struct TokenStreamGuard<'stream, 'source> {
    stream: &'stream mut TokenStream<'source>,
    cursor: usize,
}

impl<'stream, 'source> TokenStreamGuard<'stream, 'source> {
    pub fn advance(&'stream mut self) -> Result<&'stream Token<'source>, TokenStreamError> {
        self.stream
            .inner
            .first()
            .inspect(|_| self.cursor += 1)
            .ok_or(EOI)
    }

    pub fn advance_require<T: FromToken<'source>>(
        &'stream mut self,
    ) -> Result<&'stream T, TokenStreamError> {
        let token = self.advance()?;
        token
            .require()
            .ok_or(TokenStreamError::IncorrectToken(T::NAME, token.span))
    }

    pub fn peek(&'stream self) -> Result<&'stream Token<'source>, TokenStreamError> {
        self.stream.inner.first().ok_or(EOI)
    }

    pub fn peek_require<T: FromToken<'source>>(
        &'stream self,
    ) -> Result<&'stream T, TokenStreamError> {
        let token = self.peek()?;
        token
            .require()
            .ok_or(TokenStreamError::IncorrectToken(T::NAME, token.span))
    }

    pub fn peek_n(&'stream self, idx: usize) -> Result<&'stream Token<'source>, TokenStreamError> {
        self.stream.inner.get(idx).ok_or(EOI)
    }

    pub fn peek_slice_n<const N: usize>(
        &'stream self,
    ) -> Result<[&'stream Token<'source>; N], TokenStreamError> {
        array::from_fn(|idx| self.peek_n(idx)).try_map(core::convert::identity)
    }

    pub fn with<F, T, E>(&mut self, f: F) -> Result<T, E>
    where
        F: FnOnce(&mut TokenStreamGuard) -> Result<T, E>,
    {
        let cursor = self.cursor;
        let result = f(self);

        if result.is_err() {
            self.cursor = cursor;
        }

        result
    }
}
