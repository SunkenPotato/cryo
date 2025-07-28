//! Token streams.
//!
//! Token streams are used by parsers to inspect the input they are given and process it.
//!
//! View [`TokenStream`] for more information.

use std::fmt::Debug;

use crate::{Token, TokenType};
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

    // so that tests do not have to interact with the lexer.
    //#[cfg(test)]
    #[allow(missing_docs)]
    pub fn new(s: impl IntoIterator<Item = Token>) -> Self {
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
    EndOfInput,
    /// The token type the caller requested did not match the matching token.
    IncorrectToken(TokenType),
}

mod guard {
    use std::{array, fmt::Debug};

    use cryo_span::Spanned;

    use crate::{Token, TokenExt, TokenLike, stream::TokenStreamError};

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
                None => Err(TokenStreamError::EndOfInput),
            }
        }

        /// Advance the stream and attempt to convert the token into `T` via `FromToken`.
        ///
        /// # Errors
        /// Returns [`TokenStreamError::IncorrectToken`] if the token was not able to be converted to `T`.
        ///
        /// See also [`TokenStreamGuard::advance`].
        #[track_caller]
        pub fn advance_require<T: TokenLike>(&mut self) -> Result<Spanned<&T>, TokenStreamError> {
            self.stream
                .get(*self.cursor)
                .ok_or(TokenStreamError::EndOfInput)
                .and_then(|v| {
                    v.require::<T>()
                        .ok_or(TokenStreamError::IncorrectToken(v.t))
                })
                .inspect(|_| *self.cursor += 1)
        }

        /// Peek at the next token in the stream. This function will not advance the stream, so calling it multiple times will result in the same outcome.
        pub fn peek(&self) -> Result<&Token, TokenStreamError> {
            self.peek_nth(0)
        }

        /// Peek at the next token and require it to be of `T`. This function is equivalent to `Guard::peek().and_then(|v| v.require::<T>())`.
        pub fn peek_require<T>(&self) -> Result<Spanned<&T>, TokenStreamError>
        where
            T: TokenLike,
        {
            self.peek()
                .and_then(|v| v.require().ok_or(TokenStreamError::IncorrectToken(v.t)))
        }

        /// Peek at the nth token in this stream.
        pub fn peek_nth(&self, n: usize) -> Result<&Token, TokenStreamError> {
            self.stream
                .get(*self.cursor + n)
                .ok_or(TokenStreamError::EndOfInput)
        }

        /// Peek at the nth token and require it to be of `T`. This function is equivalent to `Guard::peek().and_then(|v| v.require::<T>())`.
        pub fn peek_nth_require<T>(&self, n: usize) -> Result<Spanned<&T>, TokenStreamError>
        where
            T: TokenLike,
        {
            self.peek_nth(n)
                .and_then(|v| v.require().ok_or(TokenStreamError::IncorrectToken(v.t)))
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

#[diagnostic::on_unimplemented(
    message = "it is not known whether this type can represent a success or a failure"
)]
trait Fails: Sealed {
    fn is_fail(&self) -> bool;
}

impl<T, E> Fails for Result<T, E> {
    fn is_fail(&self) -> bool {
        self.is_err()
    }
}

impl<T> Fails for Option<T> {
    fn is_fail(&self) -> bool {
        self.is_none()
    }
}

/// Provides common behavior for creating a stream guard.
#[allow(private_bounds)]
pub trait StreamLike: Sealed {
    /// Grant access to a [`Guard`], providing a save-discard-like system, where if the closure returns `Result::Ok`, the
    /// changes applied to the guard inside the closure are committed, else discarded.
    fn with<F, R>(&mut self, f: F) -> R
    where
        R: Fails,
        F: FnOnce(&mut Guard) -> R;
}

impl Sealed for TokenStream {}
impl Sealed for Guard<'_> {}

impl StreamLike for TokenStream {
    #[allow(private_bounds)]
    fn with<F, R>(&mut self, f: F) -> R
    where
        R: Fails,
        F: FnOnce(&mut Guard) -> R,
    {
        let mut cursor_copy = self.cursor;
        let mut guard = Guard {
            cursor: &mut cursor_copy,
            stream: &self.inner,
        };

        let result = f(&mut guard);

        if !result.is_fail() {
            self.cursor = cursor_copy;
        }

        result
    }
}

impl<'stream> StreamLike for Guard<'stream> {
    #[allow(private_bounds)]
    fn with<F, R>(&mut self, f: F) -> R
    where
        R: Fails,
        F: FnOnce(&mut Guard) -> R,
    {
        let mut cursor_copy = *self.cursor;
        let mut guard = Guard {
            cursor: &mut cursor_copy,
            stream: self.stream,
        };

        let result = f(&mut guard);

        if !result.is_fail() {
            *self.cursor = cursor_copy;
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
        atoms::{Equal, Semi},
        identifier::Identifier,
        stream::{StreamLike, TokenStream, TokenStreamError},
    };

    #[test]
    fn consume_stream() {
        // Span::ZERO since the spans aren't important
        let mut stream = TokenStream::new(vec![
            Token::new(TokenType::Equal(Equal), Span::ZERO),
            Token::new(TokenType::Semi(Semi), Span::ZERO),
            Token::new(TokenType::Identifier(Identifier("let".into())), Span::ZERO),
        ]);

        stream.with(|guard| {
            assert_eq!(
                guard.advance(),
                Ok(&Token::new(TokenType::Equal(Equal), Span::ZERO))
            );

            assert_eq!(
                guard.advance(),
                Ok(&Token::new(TokenType::Semi(Semi), Span::ZERO))
            );

            assert_eq!(
                guard.advance(),
                Ok(&Token::new(
                    TokenType::Identifier(Identifier("let".into())),
                    Span::ZERO
                ))
            );

            Ok::<(), TokenStreamError>(())
        });
    }
}
