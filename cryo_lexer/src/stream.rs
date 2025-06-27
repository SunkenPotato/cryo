use std::array;

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

pub struct TokenStreamGuard<'stream, 'source> {
    stream: &'stream mut TokenStream<'source>,
    cursor: usize,
}

impl<'stream, 'source> TokenStreamGuard<'stream, 'source> {
    pub fn advance(&'stream mut self) -> Option<&'stream Token<'source>> {
        self.stream.inner.first().inspect(|_| self.cursor += 1)
    }

    #[expect(private_bounds)]
    pub fn advance_require<T: FromToken<'source>>(&'stream mut self) -> Option<&'stream T> {
        self.advance()?.require()
    }

    pub fn peek(&'stream self) -> Option<&'stream Token<'source>> {
        self.stream.inner.first()
    }

    #[expect(private_bounds)]
    pub fn peek_require<T: FromToken<'source>>(&'stream self) -> Option<&'stream T> {
        self.peek()?.require()
    }

    pub fn peek_n(&'stream self, idx: usize) -> Option<&'stream Token<'source>> {
        self.stream.inner.get(idx)
    }

    pub fn peek_slice_n<const N: usize>(&'stream self) -> Option<[&'stream Token<'source>; N]> {
        array::from_fn(|idx| self.peek_n(idx)).transpose()
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
