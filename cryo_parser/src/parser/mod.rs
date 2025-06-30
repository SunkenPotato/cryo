use cryo_lexer::{FromToken, stream::TokenStream};

use crate::error::ParseError;

mod combinators;

fn terminal<'source, T, O, E, F>(
    stream: &mut TokenStream<'source>,
    f: F,
) -> Result<O, Box<dyn ParseError>>
where
    T: FromToken<'source>,
    E: ParseError + 'static,
    F: FnOnce(&T) -> Result<O, E>,
{
    stream.with(|guard| {
        let token = guard.advance_require::<T>()?;
        let p_result = f(token)?;

        Ok(p_result)
    })
}
