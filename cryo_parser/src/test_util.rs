#![allow(unexpected_cfgs)]

use std::fmt::Debug;

#[cfg(timed)]
use std::time::Instant;

use cryo_lexer::stream::{StreamLike, TokenStream};
use cryo_span::Spanned;

use crate::Parse;

#[track_caller]
pub fn assert_parse<I, T>(input: I, expect: Spanned<T>)
where
    I: TryInto<TokenStream>,
    I::Error: Debug,
    T: Parse + Debug + PartialEq,
{
    let mut stream: TokenStream = input.try_into().unwrap();
    #[cfg(timed)]
    let start_time = Instant::now();
    let Spanned { t, span } = stream.spanning(T::parse).unwrap();
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
    assert!(stream.remaining().is_empty())
}
