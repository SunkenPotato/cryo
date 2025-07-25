use std::fmt::Debug;

use cryo_lexer::stream::TokenStream;
use cryo_span::Spanned;

use crate::{Parse, Parser};

#[track_caller]
pub fn assert_parse<'a, I, T>(input: I, expect: Spanned<T>)
where
    I: TryInto<TokenStream<'a>>,
    I::Error: Debug,
    T: Parse + Debug + PartialEq,
{
    let stream: TokenStream = input.try_into().unwrap();
    let mut parser = Parser { stream };
    let Spanned { t, span } = parser.spanning::<T>().unwrap();

    assert_eq!(t, expect.t);
    assert_eq!(span, expect.span)
}
