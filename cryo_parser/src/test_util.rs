#![allow(unexpected_cfgs)]

use std::fmt::Debug;

#[cfg(timed)]
use std::time::Instant;

use cryo_lexer::stream::TokenStream;
use cryo_span::Spanned;

use crate::{Parse, Parser};

#[track_caller]
pub fn assert_parse<I, T>(input: I, expect: Spanned<T>)
where
    I: TryInto<TokenStream>,
    I::Error: Debug,
    T: Parse + Debug + PartialEq,
{
    let stream: TokenStream = input.try_into().unwrap();
    let mut parser = Parser { stream };
    #[cfg(timed)]
    let start_time = Instant::now();
    let Spanned { t, span } = parser.spanning::<T>().unwrap();
    #[cfg(timed)]
    {
        let end_time = Instant::now();
        let duration = end_time.duration_since(start_time);
        eprintln!(
            "`{}` took {}μs to parse.",
            ::core::any::type_name::<T>(),
            duration.as_micros(),
        );
    }
    assert_eq!(t, expect.t);
    assert_eq!(span, expect.span);
    assert!(parser.stream.remaining().is_empty());
}
