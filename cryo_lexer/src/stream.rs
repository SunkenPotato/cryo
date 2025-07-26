//! Token streams.
//!
//! Token streams are used by parsers to inspect the input they are given and process it.
//!
//! View [`TokenStream`] for more information.

use std::fmt::Debug;

use crate::Token;
pub use guard::{CheckpointGuard, Guard};
use smallvec::SmallVec;

/// A token stream.
///
/// Token streams cannot be created manually, only through the lexer.
///
/// To interact with a token stream, create a guard with [`StreamLike::with`] or obtain a non-tracking reference with [`TokenStream::non_tracking`].
#[derive(Clone)]
pub struct TokenStream<'source> {
    pub(crate) inner: Box<[Token<'source>]>,
    pub(crate) cursor: usize,
}

impl<'source> TokenStream<'source> {
    pub(crate) fn new(s: impl IntoIterator<Item = Token<'source>>) -> Self {
        Self {
            inner: s.into_iter().collect(),
            cursor: 0,
        }
    }

    /// Create a non-tracking guard for this token stream. Operations applied to this guard will be immediately applied, in contrast to the guard provided by [`StreamLike::with`].
    pub fn non_tracking(&mut self) -> Guard {
        Guard {
            cursor: &mut self.cursor,
            stream: &self.inner,
        }
    }

    /// Get the rest of the tokens that this stream stores. Consider using a guard instead with the methods they provide.
    pub fn remaining(&self) -> &[Token<'source>] {
        &self.inner[self.cursor..]
    }

    /// Get all the tokens that this stream stores. This is often not what you want to use, since this allows inspecting tokens that have already been consumed with [`Guard::advance`].
    pub fn all(&self) -> &[Token<'source>] {
        &self.inner
    }

    /// Get the position of the cursor.
    pub fn cursor(&self) -> usize {
        self.cursor
    }
}

impl Debug for TokenStream<'_> {
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
    IncorrectToken(&'static str),
}

mod guard {
    use std::{
        array,
        fmt::Debug,
        ops::{Deref, DerefMut},
    };

    use cryo_span::Spanned;
    use smallvec::SmallVec;

    use crate::{FromToken, Token, TokenExt, stream::TokenStreamError};

    /// A token stream guard. Provides access to operations for interacting with the underlying token stream, such as [`Guard::advance`].
    pub struct Guard<'source, 'stream> {
        pub(crate) cursor: &'stream mut usize,
        pub(crate) stream: &'stream [Token<'source>],
    }

    impl<'source, 'stream> Guard<'source, 'stream> {
        /// Advance the stream.
        ///
        /// # Errors
        /// Returns [`TokenStreamError::EndOfInput`] if there are no tokens left.
        pub fn advance<'b>(&'b mut self) -> Result<&'b Token<'source>, TokenStreamError> {
            self.stream
                .get(*self.cursor)
                .inspect(|_| *self.cursor += 1)
                .ok_or(TokenStreamError::EndOfInput)
        }

        /// Advance the stream and attempt to convert the token into `T` via `FromToken`.
        ///
        /// # Errors
        /// Returns [`TokenStreamError::IncorrectToken`] if the token was not able to be converted to `T`.
        ///
        /// See also [`TokenStreamGuard::advance`].
        pub fn advance_require<'b, T: FromToken<'source>>(
            &'b mut self,
        ) -> Result<Spanned<&'b T>, TokenStreamError> {
            self.advance()
                .and_then(|v| v.require().ok_or(TokenStreamError::IncorrectToken(T::NAME)))
        }

        /// Peek at the next token in the stream. This function will not advance the stream, so calling it multiple times will result in the same outcome.
        pub fn peek<'b>(&'b self) -> Result<&'b Token<'source>, TokenStreamError> {
            self.peek_nth(0)
        }

        /// Peek at the next token and require it to be of `T`. This function is equivalent to `Guard::peek().and_then(|v| v.require::<T>())`.
        pub fn peek_require<'b, T>(&'b self) -> Result<Spanned<&'b T>, TokenStreamError>
        where
            T: FromToken<'source>,
        {
            self.peek()
                .and_then(|v| v.require().ok_or(TokenStreamError::IncorrectToken(T::NAME)))
        }

        /// Peek at the nth token in this stream.
        pub fn peek_nth<'b>(&'b self, n: usize) -> Result<&'b Token<'source>, TokenStreamError> {
            self.stream
                .get(*self.cursor + n)
                .ok_or(TokenStreamError::EndOfInput)
        }

        /// Peek at the nth token and require it to be of `T`. This function is equivalent to `Guard::peek().and_then(|v| v.require::<T>())`.
        pub fn peek_nth_require<'b, T>(
            &'b self,
            n: usize,
        ) -> Result<Spanned<&'b T>, TokenStreamError>
        where
            T: FromToken<'source>,
        {
            self.peek_nth(n)
                .and_then(|v| v.require().ok_or(TokenStreamError::IncorrectToken(T::NAME)))
        }

        /// Peek at the next `N` tokens in this stream.
        pub fn peek_n<'b, const N: usize>(
            &'b self,
        ) -> Result<[&'b Token<'source>; N], TokenStreamError> {
            array::try_from_fn(|idx| self.peek_nth(idx))
        }
    }

    impl Debug for Guard<'_, '_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("Guard")
                .field("inner", &&self.stream[*self.cursor..])
                .finish()
        }
    }

    /// A guard with checkpoints.
    ///
    /// Allows for rolling back to when the [`CheckpointGuard::checkpoint`] method was called.
    pub struct CheckpointGuard<'stream, 'source> {
        pub(crate) guard: Guard<'stream, 'source>,
        pub(crate) checkpoints: SmallVec<[usize; 4]>,
    }

    impl<'stream, 'source> CheckpointGuard<'stream, 'source> {
        /// Store the current state of the guard.
        pub fn checkpoint(&mut self) {
            self.checkpoints.push(*self.guard.cursor);
        }

        /// Apply the last checkpoint of the guard.
        pub fn unroll(&mut self) {
            let new_cursor = self.checkpoints.pop().unwrap_or(*self.guard.cursor);
            *self.guard.cursor = new_cursor;
        }
    }

    impl<'source, 'stream> Deref for CheckpointGuard<'source, 'stream> {
        type Target = Guard<'source, 'stream>;

        fn deref(&self) -> &Self::Target {
            &self.guard
        }
    }

    impl DerefMut for CheckpointGuard<'_, '_> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.guard
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

    /// Create a checkpoint guard. This is similar to the guard `with` provides, except that one must rollback changes manually.
    fn checkpointed(&mut self) -> CheckpointGuard;
}

impl Sealed for TokenStream<'_> {}
impl Sealed for Guard<'_, '_> {}

impl<'source> StreamLike for TokenStream<'source> {
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

    fn checkpointed(&mut self) -> CheckpointGuard {
        CheckpointGuard {
            guard: Guard {
                cursor: &mut self.cursor,
                stream: &self.inner,
            },
            checkpoints: SmallVec::new(),
        }
    }
}

impl<'source, 'stream> StreamLike for Guard<'source, 'stream> {
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

    fn checkpointed(&mut self) -> CheckpointGuard {
        CheckpointGuard {
            guard: Guard {
                cursor: self.cursor,
                stream: self.stream,
            },
            checkpoints: SmallVec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    #![allow(unused_must_use)]
    use cryo_span::Span;

    use crate::{
        Token, TokenType,
        atoms::{Assign, Semi},
        stream::{StreamLike, TokenStream, TokenStreamError},
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
