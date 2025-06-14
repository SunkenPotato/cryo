//! Literal expressions.
//!
//! Literal expressions are expressions whose value is known at parse-time, are direct and require no further computation.
//!
//! Literal expressions are *not* any of the following:
//! - binding references
//! - arithmetic expressions

use crate::{Parse, ParseError};
use cryo_lexer::literal::Literal as LToken;

const ASCII_ZERO: u8 = 0x30;
const NEGATIVE_SIGN: u8 = 0x2D;
const SEPARATOR: u8 = 0x5F;

const ESCAPE: char = '\\';
const NEWLINE_ESCAPE: char = 'n';
const TAB_ESCAPE: char = 't';
const ZERO_ESCAPE: char = '0';

/// A literal expression.
///
/// A [`Literal`] can be any of the following:
/// - [`NumberLiteral`]
/// - [`StringLiteral`]
///
/// View the module-level docs for a definition of literal expressions.
pub enum Literal {
    /// The number literal variant.
    NumberLiteral(NumberLiteral),
    /// The string literal variant.
    StringLiteral(StringLiteral),
}

/// An integer literal.
///
/// This literal has the same constraints as an [`i32`], since it is represented by an `i32`.
#[derive(PartialEq, Eq, Debug, Clone, Copy, PartialOrd, Ord, Hash, Default)]
pub struct NumberLiteral(pub i32);

impl Parse for NumberLiteral {
    fn parse(stream: &mut crate::TokenStream) -> Result<Self, crate::ParseError> {
        let (token, span) = stream.advance_require::<LToken>()?;
        let LToken::NumberLiteral(num_lit) = token else {
            return Err(ParseError::wrong_type(span, "number", "string"));
        };

        let mut integer = 0i32;
        let mut sign = 1;

        for c in num_lit.0.bytes() {
            match c {
                NEGATIVE_SIGN => sign *= -1,
                SEPARATOR => continue,
                // this is ok because the lexer has already checked the validity of these strings
                c => {
                    let digit = (c - ASCII_ZERO) as i32;
                    integer = integer
                        .checked_mul(10)
                        .ok_or(ParseError::integer_overflow(span))?
                        .checked_add(digit)
                        .ok_or(ParseError::integer_overflow(span))?;
                }
            }
        }

        Ok(Self(integer * sign))
    }
}

/// An escaped string literal.
///
/// This literal has the same constraints as a [`String`], since it is represented by a `String`.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StringLiteral(pub String);

impl Parse for StringLiteral {
    // TODO: add support for unicode escapes
    fn parse(stream: &mut crate::TokenStream) -> Result<Self, ParseError> {
        let (token, span) = stream.advance_require::<LToken>()?;
        let LToken::StringLiteral(str_lit) = token else {
            return Err(ParseError::wrong_type(span, "string", "number"));
        };

        let mut s = String::new();
        let mut iter = str_lit.0.chars().peekable();

        while let Some(c) = iter.next() {
            if c == ESCAPE {
                let Some(escaped) = iter.peek() else {
                    return Err(ParseError::invalid_escape(span));
                };

                match *escaped {
                    NEWLINE_ESCAPE => s.push('\n'),
                    TAB_ESCAPE => s.push('\t'),
                    ZERO_ESCAPE => s.push('\0'),
                    _ => return Err(ParseError::invalid_escape(span)),
                }

                iter.next();
                continue;
            }

            s.push(c);
        }

        return Ok(Self(s));
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use cryo_lexer::{
        literal::{Literal, NumberLiteral as NL, StringLiteral as SL},
        tokens::{Token, TokenType},
    };
    use cryo_span::Span;

    use crate::{
        Parse, ParseError, TokenStream,
        expr::literal::{NumberLiteral, StringLiteral},
    };

    #[test]
    fn parse_int_lit() {
        let input = "--123_456_789".to_owned();
        let token = Token::new(
            TokenType::Literal(Literal::NumberLiteral(NL(input))),
            Span::EMPTY,
        );
        let mut ts = TokenStream::new(VecDeque::from([token]));

        assert_eq!(NumberLiteral::parse(&mut ts), Ok(NumberLiteral(123456789)))
    }

    #[test]
    fn do_not_parse_overflow() {
        let input = "2147483648".to_owned(); // (i32::MAX + 1)
        let token = Token::new(
            TokenType::Literal(Literal::NumberLiteral(NL(input))),
            Span::EMPTY,
        );

        let mut ts = TokenStream::new(VecDeque::from([token]));

        assert_eq!(
            NumberLiteral::parse(&mut ts),
            Err(ParseError::integer_overflow(Span::EMPTY))
        )
    }

    #[test]
    fn parse_str_lit() {
        str_test("hello, world", "hello, world")
    }

    #[test]
    fn parse_str_lit_with_escapes() {
        str_test("hello, world\\n\\t\\0", "hello, world\n\t\0");
    }

    #[test]
    fn do_not_parse_invalid_escape() {
        str_test_fail("hello, world\\z", ParseError::invalid_escape(Span::EMPTY));
    }

    #[test]
    fn do_not_parse_missing_escape() {
        str_test_fail("hello, world\\", ParseError::invalid_escape(Span::EMPTY));
    }

    fn str_test(ex: &str, re: &str) {
        let token = Token::new(
            TokenType::Literal(Literal::StringLiteral(SL(ex.to_owned()))),
            Span::EMPTY,
        );
        let mut ts = TokenStream::new(VecDeque::from([token]));

        assert_eq!(
            StringLiteral::parse(&mut ts),
            Ok(StringLiteral(re.to_owned()))
        );
        assert!(ts.0.is_empty());
    }

    fn str_test_fail(ex: &str, err: ParseError) {
        let token = Token::new(
            TokenType::Literal(Literal::StringLiteral(SL(ex.to_owned()))),
            Span::EMPTY,
        );
        let mut ts = TokenStream::new(VecDeque::from([token]));

        assert_eq!(StringLiteral::parse(&mut ts), Err(err));
        assert!(ts.0.is_empty());
    }
}
