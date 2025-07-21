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
pub type ParseResult<T> = Result<S<T>, ParseError>;

/// A parser.
///
/// For non-terminals, it is often enough to derive this trait.
#[allow(unused)]
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
        tokens.with(T::parse).map(|v| v.map(Box::new))
    }
}

impl<T: Parse> Parse for Option<T> {
    type Output = Option<T::Output>;

    fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self::Output> {
        match tokens.with(T::parse) {
            Ok(v) => Ok(v.map(Option::Some)),
            Err(e) => match (e.code(), e.subcode()) {
                (0, 0 | 1) => Ok(Spanned::new(None, e.span)),
                _ => Err(e),
            },
        }
    }
}

impl<T: Parse> Parse for Vec<T> {
    type Output = Vec<T::Output>;

    fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self::Output> {
        let mut vec = vec![];
        let mut span = Span::ZERO;

        let current_span = match tokens.peek() {
            Ok(Spanned {
                span: current_span, ..
            }) => *current_span,
            _ => return Ok(Spanned::new(vec, span)),
        };

        while let Ok(v) = tokens.with(T::parse) {
            vec.push(v.t);
            if vec.len() == 1 {
                span = v.span;
            } else {
                span += v.span
            }
        }

        if vec.is_empty() {
            span = current_span;
        }

        Ok(Spanned::new(vec, span))
    }
}

impl<T: Parse, const N: usize> Parse for [T; N] {
    type Output = [T::Output; N];

    fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self::Output> {
        fn transpose<T, const N: usize>(val: [Spanned<T>; N]) -> Spanned<[T; N]> {
            let span = val.iter().fold(Span::ZERO, |a, b| a + b.span);
            let items = val.map(|s| s.t);

            Spanned { t: items, span }
        }

        core::array::try_from_fn(|_| tokens.with(T::parse)).map(transpose)
    }
}

/// Utility for parsing sequences of `T` punctuated by `P`.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Punct<T, P> {
    /// The fields.
    pub inner: Vec<(T, P)>,
    /// The last field, does not require a `P`.
    pub tail: Option<Box<T>>,
}

impl<T, P> Parse for Punct<T, P>
where
    T: Parse,
    P: Parse,
{
    type Output = Punct<T::Output, P::Output>;

    fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self::Output> {
        let mut inner = vec![];
        let mut tail = None;
        let mut span = Span::ZERO;

        while let Ok(t) = tokens.with(T::parse) {
            span += t.span;
            if let Ok(p) = tokens.with(P::parse) {
                span += p.span;
                inner.push((t.t, p.t))
            } else {
                tail = Some(Box::new(t.t));
                break;
            }
        }

        Ok(Spanned::new(Punct { inner, tail }, span))
    }
}

/// A utility for parsing terminals which require exactly one token.
///
/// The closure supplied must take a type which implements [`cryo_lexer::FromToken`] and effectively return a [`ParseResult`].
pub fn terminal<'source, T, R, E, F>(
    stream: &mut TokenStream<'source>,
    f: F,
) -> Result<Spanned<R>, ParseError>
where
    T: FromToken<'source>,
    E: Into<ParseError> + 'static,
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
