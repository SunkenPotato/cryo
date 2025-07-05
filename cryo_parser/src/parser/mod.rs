use cryo_lexer::{
    FromToken,
    stream::{TokenStream, TokenStreamGuard},
};
use cryo_parser_proc_macro::Parse;
use cryo_span::{Span, Spanned};

use crate::{S, error::ParseError};

pub mod combinators;

pub type ParseResult<T> = Result<S<T>, Box<dyn ParseError>>;

pub trait Parse: Sized {
    type Output;
    fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self::Output>;
}

impl Parse for () {
    type Output = Self;

    fn parse(_: &mut TokenStreamGuard) -> ParseResult<Self::Output> {
        Ok(Spanned::new((), Span::ZERO))
    }
}

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

pub fn parser<T>(mut tokens: TokenStream) -> ParseResult<T::Output>
where
    T: Parse,
{
    tokens.with(T::parse)
}
