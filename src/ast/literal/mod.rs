pub mod integer;
pub mod string;

use integer::{Integer, IntegerParseError};
use string::{StringLiteral, StringParseError};

use crate::group;

use super::{Parse, ParseResultInto, extract_whitespace};

group! {
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub enum LiteralParseError {
        IntegerParseError(IntegerParseError),
        StringParseError(StringParseError),
        Empty
    }
}

group! {
    #[derive(Debug, Clone, PartialEq)]
    pub enum Literal {
        Integer(Integer),
        String(StringLiteral)
    }
}

impl<'a> Parse<'a> for Literal {
    type Error = LiteralParseError;

    fn parse(input: &str) -> Result<(Self, &str), Self::Error> {
        let input2 = extract_whitespace(input);
        let first_char = input2.chars().next().ok_or(LiteralParseError::Empty)?;
        if first_char.is_ascii_digit() || first_char == '-' {
            return Integer::parse(input2).into2();
        } else if first_char == '"' {
            return StringLiteral::parse(input2).into2();
        } else {
            return Err(LiteralParseError::Empty);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        Parse,
        literal::{Literal, integer::Integer, string::StringLiteral},
    };

    #[test]
    fn parse_string() {
        let s = "\"Hello, world\"";

        assert_eq!(
            Ok((Literal::String(StringLiteral("Hello, world".into())), "")),
            Literal::parse(s)
        );
    }

    #[test]
    fn parse_int() {
        let s = "--20402";

        assert_eq!(
            Ok((Literal::Integer(Integer(20402)), "")),
            Literal::parse(s)
        )
    }
}
