//! Token streams.
//!
//! Token streams are used by parsers to inspect the input they are given and process it.
//!
//! View [`TokenStream`] for more information.

use std::array;

use cryo_span::{Span, Spanned};

use crate::{FromToken, Token, TokenExt};

/// A token stream.
///
/// Token streams cannot be created manually, only through the lexer.
#[derive(Clone, Debug)]
pub struct TokenStream<'source> {
    pub(crate) inner: Vec<Token<'source>>,
}

impl<'source> TokenStream<'source> {
    pub(crate) fn new(s: Vec<Token<'source>>) -> Self {
        Self { inner: s }
    }

    /// Grant access to token stream operations via a [guard](TokenStreamGuard) supplied to the given closure.
    ///
    /// If the closure returns [`Result::Ok`], the operations applied inside the closure will be committed to the stream.
    /// Otherwise, the changes will be discarded.
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

/// A token stream error.
#[cfg_attr(test, derive(Debug, PartialEq))] // only for tests, since all externals use dyn ParseError
pub enum TokenStreamError {
    /// An unexpected end of input was reached.
    EndOfInput,
    /// The token type the caller requested did not match the matching token.
    IncorrectToken(&'static str, Span),
}

const EOI: TokenStreamError = TokenStreamError::EndOfInput;

/// A token stream guard.
///
/// This struct provides basic methods for operations concerning the actual token stream, such as advancing the stream or peeking.
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
    /// Advance the stream.
    ///
    /// # Errors
    /// Returns [`TokenStreamError::EndOfInput`] if there are no tokens left.
    pub fn advance<'b>(&'b mut self) -> Result<&'b Token<'source>, TokenStreamError> {
        self.stream
            .inner
            .get(self.cursor)
            .inspect(|_| self.cursor += 1)
            .ok_or(EOI)
    }

    /// Advance the stream and attempt to convert the token into `T` via `FromToken`.
    ///
    /// # Errors
    /// Returns [`TokenStreamError::IncorrectToken`] if the token was not able to be converted to `T`.
    ///
    /// See also [`TokenStreamGuard::advance`].
    #[track_caller]
    pub fn advance_require<'b, T: FromToken<'source>>(
        &'b mut self,
    ) -> Result<Spanned<&'b T>, TokenStreamError> {
        let token = self.advance()?;
        token
            .require()
            .ok_or(TokenStreamError::IncorrectToken(T::NAME, token.span))
    }

    /// Peek at the next token without advancing the stream.
    ///
    /// # Errors
    /// Returns [`TokenStreamError::EndOfInput`] if there are no tokens left.
    pub fn peek(&'stream self) -> Result<&'stream Token<'source>, TokenStreamError> {
        self.stream.inner.get(self.cursor).ok_or(EOI)
    }

    /// Peek at the next token and attempt to convert the token into `T` via `FromToken`.
    ///
    /// # Errors
    /// Returns [`TokenStreamError::IncorrectToken`] if the token was not able to be converted to `T`.
    ///
    /// See also [`TokenStreamGuard::peek`].
    pub fn peek_require<T: FromToken<'source>>(
        &'stream self,
    ) -> Result<Spanned<&'stream T>, TokenStreamError> {
        let token = self.peek()?;
        token
            .require()
            .ok_or(TokenStreamError::IncorrectToken(T::NAME, token.span))
    }

    /// Peek `n` tokens ahead.
    ///
    /// # Errors
    /// Returns [`TokenStreamError::EndOfInput`] if `n` is out of bounds or if the stream is empty.
    pub fn peek_n(&'stream self, n: usize) -> Result<&'stream Token<'source>, TokenStreamError> {
        self.stream.inner.get(self.cursor + n).ok_or(EOI)
    }

    /// Peek at the next `N` tokens.
    ///
    /// # Errors
    /// Returns [`TokenStreamError::EndOfInput`] if the length of the stream is smaller than `N`.
    pub fn peek_slice_n<const N: usize>(
        &'stream self,
    ) -> Result<[&'stream Token<'source>; N], TokenStreamError> {
        array::from_fn(|idx| self.peek_n(idx)).try_map(core::convert::identity)
    }

    /// Call a closure `f` on a new guard.
    ///
    /// If the closure returns [`Result::Ok`], the changes applied inside the closure are committed to the upstream [`TokenStreamGuard`].
    /// Otherwise, the changes are discarded.
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
                Ok(&Token::new(TokenType::Assign(Assign), Span::ZERO))
            );

            assert_eq!(
                guard.advance(),
                Ok(&Token::new(TokenType::Semi(Semi), Span::ZERO))
            );

            assert_eq!(
                guard.advance(),
                Ok(&Token::new(
                    TokenType::Keyword(crate::atoms::Keyword::Let),
                    Span::ZERO
                ))
            );

            Ok::<(), TokenStreamError>(())
        });
    }
}
