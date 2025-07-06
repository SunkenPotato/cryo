//! Tools for parsing tokens.

use cryo_lexer::{
    FromToken,
    stream::{TokenStream, TokenStreamGuard},
};
use cryo_parser_proc_macro::Parse;
use cryo_span::{Span, Spanned};

use crate::{S, error::ParseError};

pub mod combinators;

/// The result of a parser.
pub type ParseResult<T> = Result<S<T>, Box<dyn ParseError>>;

/// A parser.
///
/// For non-terminals, it is often enough to derive this trait.
pub trait Parse: Sized {
    /// The type that this parser returns (often `Self`).
    type Output;
    /// The actual parser.
    fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self::Output>;
}

impl Parse for () {
    type Output = Self;

    fn parse(_: &mut TokenStreamGuard) -> ParseResult<Self::Output> {
        Ok(Spanned::new((), Span::ZERO))
    }
}

impl<T: Parse> Parse for Box<T> {
    type Output = Box<T::Output>;

    fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self::Output> {
        T::parse(tokens).map(|v| v.map(Box::new))
    }
}

impl<T: Parse> Parse for Option<T> {
    type Output = Option<T::Output>;

    fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self::Output> {
        match tokens.with(T::parse) {
            Ok(v) => Ok(v.map(Option::Some)),
            Err(e) => match (e.code(), e.subcode()) {
                (0, 0 | 1) => Ok(Spanned::new(None, *e.span())),
                _ => Err(e),
            },
        }
    }
}

/// A utility for parsing terminals which require exactly one token.
///
/// The closure supplied must take a type which implements [`cryo_lexer::FromToken`] and effectively return a [`ParseResult`].
pub fn terminal<'source, T, R, E, F>(
    stream: &mut TokenStream<'source>,
    f: F,
) -> Result<Spanned<R>, Box<dyn ParseError>>
where
    T: FromToken<'source>,
    E: ParseError + 'static,
    F: FnOnce(&T) -> Result<Spanned<R>, E>,
{
    stream.with(|guard| {
        let token = guard.advance_require()?;
        Ok(f(token.t)?)
    })
}

/// Wrapper around `tokens.with(T::parse)`.
#[track_caller]
pub fn parser<T>(mut tokens: TokenStream) -> ParseResult<T::Output>
where
    T: Parse,
{
    tokens.with(T::parse)
}
