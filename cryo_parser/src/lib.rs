#![allow(private_bounds, private_interfaces)]

pub mod error;
pub mod expr;

use cryo_lexer::stream::{TokenStream, TokenStreamGuard};
use cryo_span::Spanned;

type S<T> = Spanned<T>;
type ParseResult<T> = Result<S<T>, S<Box<dyn ParseError>>>;

use crate::error::ParseError;

trait Parse: Sized {
    fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self>;
}

pub fn parser<T>(mut tokens: TokenStream) -> ParseResult<T>
where
    T: Parse,
{
    tokens.with(T::parse)
}

#[cfg(test)]
mod test_util {
    use cryo_lexer::stream::TokenStream;

    use crate::{Parse, S, error::ParseError};

    #[track_caller]
    pub(crate) fn assert_parse<T>(mut tokens: TokenStream, expect: S<T>)
    where
        T: Parse + std::fmt::Debug + PartialEq,
    {
        let result = tokens.with(T::parse);

        match result {
            Ok(v) => assert_eq!(v, expect),
            Err(e) => {
                panic!("parse failed: {e}")
            }
        }
    }

    #[derive(Debug)]
    pub struct TestError {
        code: u32,
        subcode: u32,
    }

    impl TestError {
        pub const fn new(code: u32, subcode: u32) -> Self {
            Self { code, subcode }
        }
    }

    impl ParseError for TestError {
        fn code(&self) -> u32 {
            self.code
        }

        fn subcode(&self) -> u32 {
            self.subcode
        }

        fn name(&self) -> &'static str {
            unreachable!()
        }

        fn span(&self) -> &cryo_span::Span {
            unreachable!()
        }
    }

    #[track_caller]
    pub(crate) fn assert_parse_fail<T>(mut tokens: TokenStream, expect: TestError)
    where
        T: Parse + std::fmt::Debug,
    {
        let result = tokens.with(T::parse);
        match result {
            Ok(v) => panic!("parsing succeded: {v:?}"),
            Err(e) => assert_eq!(&*e.t, &expect as &dyn ParseError),
        }
    }
}
