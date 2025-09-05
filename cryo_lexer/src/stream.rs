//! Token streams.
//!
//! Token streams are used by parsers to inspect the input they are given and process it.
//!
//! View [`TokenStream`] for more information.

use std::fmt::Debug;

use crate::{Token, TokenKind};
use cryo_span::{Span, Spanned};
pub use guard::Guard;

/// A token stream.
///
/// Token streams should not be created manually, only through the lexer.
///
/// To interact with a token stream, create a guard with [`StreamLike::with`] or obtain a non-tracking reference with [`TokenStream::non_tracking`].
#[derive(Clone)]
pub struct TokenStream {
    pub(crate) inner: Box<[Token]>,
    pub(crate) cursor: usize,
}

impl TokenStream {
    #[cfg(not(test))]
    #[cfg(false)]
    pub(crate) fn new(s: impl IntoIterator<Item = Token>) -> Self {
        Self {
            inner: s.into_iter().collect(),
            cursor: 0,
        }
    }

    pub(crate) fn new(s: impl IntoIterator<Item = Token>) -> Self {
        Self {
            inner: s.into_iter().collect(),
            cursor: 0,
        }
    }

    /// Create a non-tracking guard for this token stream. Operations applied to this guard will be immediately applied, in contrast to the guard provided by [`StreamLike::with`].
    pub fn non_tracking(&'_ mut self) -> Guard<'_> {
        Guard {
            cursor: &mut self.cursor,
            stream: &self.inner,
        }
    }

    /// Get the rest of the tokens that this stream stores. Consider using a guard instead with the methods they provide.
    pub fn remaining(&self) -> &[Token] {
        &self.inner[self.cursor..]
    }

    /// Get all the tokens that this stream stores. This is often not what you want to use, since this allows inspecting tokens that have already been consumed with [`Guard::advance`].
    pub fn all(&self) -> &[Token] {
        &self.inner
    }

    /// Get the position of the cursor.
    pub fn cursor(&self) -> usize {
        self.cursor
    }
}

impl Debug for TokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TokenStream")
            .field("inner", &&self.inner[self.cursor..])
            .finish()
    }
}

/// A token stream error.
#[derive(Debug, PartialEq, Eq)] // only for tests, since all externals use dyn ParseError
pub enum TokenStreamError {
    /// An unexpected end of input was reached.
    EndOfInput(Span),
    /// The token type the caller requested did not match the matching token.
    IncorrectToken {
        /// The token received.
        got: Token,
        /// The expected token.
        expected: TokenKind,
    },
}

impl TokenStreamError {
    /// Get the span of this error. In the case of [`TokenStreamError::EndOfInput`], this will always be `Span::ZERO`.
    pub const fn span(&self) -> Span {
        match self {
            Self::EndOfInput(s) => *s,
            Self::IncorrectToken { got, .. } => got.span,
        }
    }
}

mod guard {
    use std::{array, fmt::Debug};

    use cryo_span::Spanned;

    use crate::{Symbol, Token, TokenKind, stream::TokenStreamError};

    /// A token stream guard. Provides access to operations for interacting with the underlying token stream, such as [`Guard::advance`].
    pub struct Guard<'stream> {
        pub(crate) cursor: &'stream mut usize,
        pub(crate) stream: &'stream [Token],
    }

    impl<'stream> Guard<'stream> {
        /// Advance the stream.
        ///
        /// # Errors
        /// Returns [`TokenStreamError::EndOfInput`] if there are no tokens left.
        #[track_caller]
        pub fn advance(&mut self) -> Result<&Token, TokenStreamError> {
            match self.stream.get(*self.cursor).inspect(|_| *self.cursor += 1) {
                Some(v) => Ok(v),
                None => Err(TokenStreamError::EndOfInput(
                    self.stream[*self.cursor - 1].span,
                )),
            }
        }

        /// Advance the stream and attempt to convert the token into `T` via `FromToken`.
        ///
        /// # Errors
        /// Returns [`TokenStreamError::IncorrectToken`] if the token was not able to be converted to `T`.
        ///
        /// See also [`TokenStreamGuard::advance`].
        #[cfg_attr(test, track_caller)]
        pub fn advance_require(
            &mut self,
            kind: TokenKind,
        ) -> Result<Spanned<Symbol>, TokenStreamError> {
            self.stream
                .get(*self.cursor)
                .ok_or(TokenStreamError::EndOfInput(
                    self.stream[self.cursor.saturating_sub(1)].span,
                ))
                .and_then(|v| {
                    if kind == v.kind {
                        Ok(Spanned::new(v.lexeme, v.span))
                    } else {
                        Err(TokenStreamError::IncorrectToken {
                            got: *v,
                            expected: kind,
                        })
                    }
                })
                .inspect(|_| *self.cursor += 1)
        }

        /// Peek at the next token in the stream. This function will not advance the stream, so calling it multiple times will result in the same outcome.
        pub fn peek(&self) -> Result<&Token, TokenStreamError> {
            self.peek_nth(0)
        }

        /// Peek at the next token and require it to be of `T`. This function is equivalent to `Guard::peek().and_then(|v| v.require::<T>())`.
        pub fn peek_require(&self, kind: TokenKind) -> Result<Spanned<Symbol>, TokenStreamError> {
            self.peek().and_then(|v| {
                if v.kind == kind {
                    Ok(Spanned::new(v.lexeme, v.span))
                } else {
                    Err(TokenStreamError::IncorrectToken {
                        got: *v,
                        expected: kind,
                    })
                }
            })
        }

        /// Peek at the nth token in this stream.
        pub fn peek_nth(&self, n: usize) -> Result<&Token, TokenStreamError> {
            self.stream
                .get(*self.cursor + n)
                .ok_or(TokenStreamError::EndOfInput(
                    self.stream[self.cursor.saturating_sub(1)].span,
                ))
        }

        /// Peek at the nth token and require it to be of `T`. This function is equivalent to `Guard::peek().and_then(|v| v.require::<T>())`.
        pub fn peek_nth_require(
            &self,
            kind: TokenKind,
            n: usize,
        ) -> Result<Spanned<Symbol>, TokenStreamError> {
            self.peek_nth(n).and_then(|v| {
                if v.kind == kind {
                    Ok(Spanned::new(v.lexeme, v.span))
                } else {
                    Err(TokenStreamError::IncorrectToken {
                        got: *v,
                        expected: kind,
                    })
                }
            })
        }

        /// Peek at the next `N` tokens in this stream.
        pub fn peek_n<const N: usize>(&self) -> Result<[&Token; N], TokenStreamError> {
            array::try_from_fn(|idx| self.peek_nth(idx))
        }
    }

    impl Debug for Guard<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("Guard")
                .field("inner", &&self.stream[*self.cursor..])
                .finish()
        }
    }
}

trait Sealed {}

impl<T, E> Sealed for Result<T, E> {}
impl<T> Sealed for Option<T> {}

/// Provides common behavior for creating a stream guard.
#[expect(private_bounds)]
pub trait StreamLike: Sealed {
    /// Grant access to a [`Guard`], providing a save-discard-like system, where if the closure returns `Result::Ok`, the
    /// changes applied to the guard inside the closure are committed, else discarded.
    fn with<F, T, E>(&mut self, f: F) -> Result<T, E>
    where
        F: FnOnce(&mut Guard) -> Result<T, E>;

    /// Equivalent to `with`, except that this wraps the `Ok` value in a [`Spanned`] which has the span of all the tokens consumed.
    fn spanning<F, T, E>(&mut self, f: F) -> Result<Spanned<T>, E>
    where
        F: FnOnce(&mut Guard) -> Result<T, E>;

    /// Grant access to a non-consuming guard, which will never consume tokens from the underlying stream.
    fn non_consuming<F, T, E>(&mut self, f: F) -> Result<T, E>
    where
        F: FnOnce(&mut Guard) -> Result<T, E>;
}

impl Sealed for TokenStream {}
impl Sealed for Guard<'_> {}

impl StreamLike for TokenStream {
    fn with<F, T, E>(&mut self, f: F) -> Result<T, E>
    where
        F: FnOnce(&mut Guard) -> Result<T, E>,
    {
        let mut cursor_copy = self.cursor;
        let mut guard = Guard {
            cursor: &mut cursor_copy,
            stream: &self.inner,
        };

        let result = f(&mut guard);

        if result.is_ok() {
            self.cursor = cursor_copy;
        }

        result
    }

    fn spanning<F, T, E>(&mut self, f: F) -> Result<Spanned<T>, E>
    where
        F: FnOnce(&mut Guard) -> Result<T, E>,
    {
        let cursor_before = self.cursor;
        let result = self.with(f)?;
        let final_span = self.inner[cursor_before..self.cursor()]
            .iter()
            .fold(self.inner[cursor_before].span, |b, token| {
                b.extend(token.span)
            });

        Ok(Spanned::new(result, final_span))
    }

    fn non_consuming<F, T, E>(&mut self, f: F) -> Result<T, E>
    where
        F: FnOnce(&mut Guard) -> Result<T, E>,
    {
        let mut cursor_before = self.cursor;
        let mut guard = Guard {
            cursor: &mut cursor_before,
            stream: &self.inner,
        };

        f(&mut guard)
    }
}

impl<'stream> StreamLike for Guard<'stream> {
    fn with<F, T, E>(&mut self, f: F) -> Result<T, E>
    where
        F: FnOnce(&mut Guard) -> Result<T, E>,
    {
        let mut cursor_copy = *self.cursor;
        let mut guard = Guard {
            cursor: &mut cursor_copy,
            stream: self.stream,
        };

        let result = f(&mut guard);

        if result.is_ok() {
            *self.cursor = cursor_copy;
        }

        result
    }

    fn spanning<F, T, E>(&mut self, f: F) -> Result<Spanned<T>, E>
    where
        F: FnOnce(&mut Guard) -> Result<T, E>,
    {
        let cursor_before = *self.cursor;
        let result = self.with(f)?;
        let final_span = if cursor_before == *self.cursor {
            let st = if cursor_before > 0 {
                self.stream[cursor_before - 1].span.stop
            } else {
                0
            };

            Span::new(st, st)
        } else {
            self.stream[cursor_before..*self.cursor]
                .iter()
                .fold(self.stream[cursor_before].span, |b, token| {
                    b.extend(token.span)
                })
        };

        Ok(Spanned::new(result, final_span))
    }

    fn non_consuming<F, T, E>(&mut self, f: F) -> Result<T, E>
    where
        F: FnOnce(&mut Guard) -> Result<T, E>,
    {
        let mut cursor_before = *self.cursor;
        let mut guard = Guard {
            cursor: &mut cursor_before,
            stream: self.stream,
        };

        f(&mut guard)
    }
}

#[cfg(test)]
mod tests {
    use cryo_span::Span;

    use crate::{
        Token, TokenKind,
        stream::{StreamLike, TokenStream, TokenStreamError},
    };

    #[test]
    fn consume_stream() {
        // Span::ZERO since the spans aren't important
        let mut stream = TokenStream::new(vec![
            Token::new(TokenKind::Equal, "=".into(), Span::ZERO),
            Token::new(TokenKind::Semi, ";".into(), Span::ZERO),
            Token::new(TokenKind::Identifier, "let".into(), Span::ZERO),
        ]);

        stream
            .with(|guard| {
                assert_eq!(
                    guard.advance(),
                    Ok(&Token::new(TokenKind::Equal, "=".into(), Span::ZERO))
                );

                assert_eq!(
                    guard.advance(),
                    Ok(&Token::new(TokenKind::Semi, ";".into(), Span::ZERO))
                );

                assert_eq!(
                    guard.advance(),
                    Ok(&Token::new(TokenKind::Identifier, "let".into(), Span::ZERO))
                );

                Ok::<(), TokenStreamError>(())
            })
            .unwrap();
    }
}
