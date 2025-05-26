pub mod integer;

use integer::{Integer, IntegerParseError};

use crate::group;

use super::{Parse, ParseResultInto};

group! {
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub enum LiteralParseError {
        IntegerParseError(IntegerParseError)
    }
}

group! {
    #[derive(Debug, Clone)]
    pub enum Literal {
        Integer(Integer)
    }
}

impl Parse for Literal {
    type Error = LiteralParseError;

    fn parse(input: &str) -> Result<(Self, &str), Self::Error> {
        Integer::parse(input).into2()
    }
}
