use crate::ast::{Parse, extract, strip_whitespace};

pub const NEGATE_OP: char = '-';
pub const INT_SEPARATOR: char = '_';
pub const ZERO_CODE_POINT: i32 = 0x30;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IntegerParseError {
    Empty,
    OverflowError,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Integer(pub i32);

impl Parse for Integer {
    type Error = IntegerParseError;

    fn parse(input: &str) -> Result<(Self, &str), Self::Error> {
        let input = strip_whitespace(input);
        let (digits, rest) = extract(input, |c| {
            c.is_ascii_digit() || *c == NEGATE_OP || *c == INT_SEPARATOR
        });

        if digits.is_empty() {
            return Err(IntegerParseError::Empty);
        }

        let mut negate = 1;
        let mut int: i32 = 0;

        for c in digits.chars() {
            if c == NEGATE_OP {
                negate *= -1;
            } else if c == INT_SEPARATOR {
                continue;
            } else {
                let digit = c as i32 - ZERO_CODE_POINT;

                int = int
                    .checked_mul(10)
                    .ok_or(IntegerParseError::OverflowError)?
                    .checked_add(digit)
                    .ok_or(IntegerParseError::OverflowError)?;
            }
        }

        int *= negate;

        Ok((Self(int), rest))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        Parse,
        literal::integer::{Integer, IntegerParseError},
    };

    #[test]
    fn parse_int() {
        let s = "123456";

        assert_eq!((Integer(123456), ""), Integer::parse(s).unwrap())
    }

    #[test]
    fn parse_negated_int() {
        let s = "-123456";

        assert_eq!((Integer(-123456), ""), Integer::parse(s).unwrap())
    }

    #[test]
    fn parse_multi_negated_int() {
        let s = "-----123456";

        assert_eq!((Integer(-123456), ""), Integer::parse(s).unwrap())
    }

    #[test]
    fn parse_with_overflow() {
        let s = "2147483648";

        assert_eq!(
            IntegerParseError::OverflowError,
            Integer::parse(s).unwrap_err()
        )
    }

    #[test]
    fn parse_empty() {
        let s = "";

        assert_eq!(IntegerParseError::Empty, Integer::parse(s).unwrap_err())
    }
}
