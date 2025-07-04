#![allow(private_bounds, private_interfaces)]

pub mod error;
pub mod parser;

use cryo_span::Spanned;

type S<T> = Spanned<T>;

#[cfg(test)]
mod test_util {
    use std::fmt::Debug;

    use cryo_lexer::stream::TokenStream;

    use crate::{S, error::ParseError, parser::Parse};

    #[track_caller]
    pub(crate) fn assert_parse<T>(mut tokens: TokenStream, expect: S<T::Output>)
    where
        T: Parse,
        T::Output: Debug + PartialEq,
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
        T: Parse,
        T::Output: Debug,
    {
        let result = tokens.with(T::parse);
        match result {
            Ok(v) => panic!("parsing succeded: {v:?}"),
            Err(e) => assert_eq!(&*e.t, &expect as &dyn ParseError),
        }
    }
}
