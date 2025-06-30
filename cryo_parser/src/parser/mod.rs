use cryo_lexer::{FromToken, stream::TokenStream};

use crate::error::ParseError;

mod combinators;

pub fn terminal<'source, T, R, E, F>(
    stream: &mut TokenStream<'source>,
    f: F,
) -> Result<R, Box<dyn ParseError>>
where
    T: FromToken<'source>,
    E: ParseError + 'static,
    F: FnOnce(&T) -> Result<R, E>,
{
    stream.with(|guard| {
        let token = guard.advance_require()?;
        Ok(f(token)?)
    })
}
