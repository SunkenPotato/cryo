use std::array;

use cryo_span::{Span, Spanned};

use crate::{FromToken, Token, TokenExt};

#[derive(Clone, Debug)]
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

#[cfg_attr(test, derive(Debug, PartialEq))] // only for tests, since all externals use dyn ParseError
pub enum TokenStreamError {
    EndOfInput,
    IncorrectToken(&'static str, Span),
}

const EOI: TokenStreamError = TokenStreamError::EndOfInput;

pub struct TokenStreamGuard<'stream, 'source> {
    stream: &'stream mut TokenStream<'source>,
    cursor: usize,
}

impl std::fmt::Debug for TokenStreamGuard<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TokenStreamGuard")
            .field("inner", &&self.stream.inner[self.cursor..])
            .finish()
    }
}

impl<'stream, 'source> TokenStreamGuard<'stream, 'source> {
    pub fn advance<'b>(&'b mut self) -> Result<&'b Token<'source>, TokenStreamError> {
        self.stream
            .inner
            .get(self.cursor)
            .inspect(|_| self.cursor += 1)
            .ok_or(EOI)
    }

    #[track_caller]
    pub fn advance_require<'b, T: FromToken<'source>>(
        &'b mut self,
    ) -> Result<Spanned<&'b T>, TokenStreamError> {
        let token = self.advance()?;
        token
            .require()
            .ok_or(TokenStreamError::IncorrectToken(T::NAME, token.span))
    }

    pub fn peek(&'stream self) -> Result<&'stream Token<'source>, TokenStreamError> {
        self.stream.inner.get(self.cursor).ok_or(EOI)
    }

    pub fn peek_require<T: FromToken<'source>>(
        &'stream self,
    ) -> Result<Spanned<&'stream T>, TokenStreamError> {
        let token = self.peek()?;
        token
            .require()
            .ok_or(TokenStreamError::IncorrectToken(T::NAME, token.span))
    }

    pub fn peek_n(&'stream self, idx: usize) -> Result<&'stream Token<'source>, TokenStreamError> {
        self.stream.inner.get(self.cursor + idx).ok_or(EOI)
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

#[cfg(test)]
mod tests {
    #![allow(unused_must_use)]
    use cryo_span::Span;

    use crate::{
        Token, TokenType,
        atoms::{Assign, Semi},
        stream::{TokenStream, TokenStreamError},
    };

    #[test]
    fn consume_stream() {
        // Span::ZERO since the spans aren't important
        let mut stream = TokenStream::new(vec![
            Token::new(TokenType::Assign(Assign), Span::ZERO),
            Token::new(TokenType::Semi(Semi), Span::ZERO),
            Token::new(TokenType::Keyword(crate::atoms::Keyword::Let), Span::ZERO),
        ]);

        stream.with(|guard| {
            assert_eq!(
                guard.advance(),
                Ok(&Token::new(
                    TokenType::Keyword(crate::atoms::Keyword::Let),
                    Span::ZERO
                ))
            );

            assert_eq!(
                guard.advance(),
                Ok(&Token::new(TokenType::Semi(Semi), Span::ZERO))
            );

            assert_eq!(
                guard.advance(),
                Ok(&Token::new(TokenType::Assign(Assign), Span::ZERO))
            );

            Ok::<(), TokenStreamError>(())
        });
    }
}
