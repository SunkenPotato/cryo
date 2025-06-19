//! Literal expressions.
//!
//! Literal expressions are expressions whose value is known at parse-time, are direct and require no further computation.
//!
//! Literal expressions are *not* any of the following:
//! - binding references
//! - arithmetic expressions

use crate::{
    Parse, SpecToken, err,
    error::{GenericError, ParseError, SpannedGenericError},
    parse_error,
};
use cryo_lexer::literal::Literal as LToken;

const ASCII_ZERO: u8 = 0x30;
const NEGATIVE_SIGN: u8 = 0x2D;
const SEPARATOR: u8 = 0x5F;

const ESCAPE: char = '\\';
const QUOTE_ESCAPE: char = '"';
const NEWLINE_ESCAPE: char = 'n';
const TAB_ESCAPE: char = 't';
const NULL_ESCAPE: char = '0';

/// A literal expression.
///
/// A [`Literal`] can be any of the following:
/// - [`NumberLiteral`]
/// - [`StringLiteral`]
///
/// View the module-level docs for a definition of literal expressions.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Literal {
    /// The number literal variant.
    NumberLiteral(NumberLiteral),
    /// The string literal variant.
    StringLiteral(StringLiteral),
}

parse_error! {
    /// Errors that can occur during literal parsing.
    #(group)
    pub enum LiteralParseError {
        /// A number literal parse error. View [`NumberLiteralError`] for more info.
        NumberLiteralParseError(SpannedNumberLiteralError),
        /// A string literal parse error. View [`StringLiteralError`] for more info.
        StringLiteralParseError(SpannedStringLiteralError),
    }
}

impl Parse for Literal {
    fn parse(stream: &mut crate::TokenStream) -> Result<Self, Box<dyn ParseError>> {
        NumberLiteral::parse(stream)
            .map(Self::NumberLiteral)
            .or_else(|_| StringLiteral::parse(stream).map(Self::StringLiteral))
    }
}

/// An integer literal.
///
/// This literal has the same constraints as an [`i32`], since it is represented by an `i32`.
#[derive(PartialEq, Eq, Debug, Clone, Copy, PartialOrd, Ord, Hash, Default)]
pub struct NumberLiteral(pub i32);

parse_error! {
    /// A number literal parse error.
    #(concrete, 1)
    pub enum NumberLiteralError {
        /// An overflowing integer was detected at compile time.
        #("compile-time integer overflow", 0)
        Overflow
    }
}

impl Parse for NumberLiteral {
    fn parse(stream: &mut crate::TokenStream) -> Result<Self, Box<dyn ParseError>> {
        let SpecToken { token, span } = stream.advance_require::<LToken>()?;
        let LToken::NumberLiteral(number) = token else {
            return Err(err!(
                SpannedGenericError,
                GenericError::IncorrectToken("number", "string"),
                span
            ));
        };

        let mut integer = 0i32;
        let mut sign = 1;

        for ch in number.0.bytes() {
            match ch {
                NEGATIVE_SIGN => {
                    sign *= -1;
                    continue;
                }
                SEPARATOR => continue,
                digit => {
                    let digit = digit - ASCII_ZERO;
                    integer = integer
                        .checked_mul(10)
                        .ok_or_else(|| {
                            err!(
                                SpannedNumberLiteralError,
                                NumberLiteralError::Overflow,
                                span
                            )
                        })?
                        .checked_add(digit as i32)
                        .ok_or_else(|| {
                            err!(
                                SpannedNumberLiteralError,
                                NumberLiteralError::Overflow,
                                span
                            )
                        })?;
                }
            }
        }

        stream.sync();

        Ok(Self(integer * sign))
    }
}

/// An escaped string literal.
///
/// This literal has the same constraints as a [`String`], since it is represented by a `String`.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StringLiteral(pub String);

parse_error! {
    /// Errors that occur during string literal parsing.
    #(concrete, 2)
    pub enum StringLiteralError {
        /// An invalid escape was encountered.
        #("invalid escape: {}", 0)
        InvalidEscape(esc: char)
    }
}

impl Parse for StringLiteral {
    // TODO: add support for unicode escapes
    fn parse(stream: &mut crate::TokenStream) -> Result<Self, Box<dyn ParseError>> {
        let SpecToken { token, span } = stream.advance_require::<LToken>()?;
        let LToken::StringLiteral(number) = token else {
            return Err(err!(
                SpannedGenericError,
                GenericError::IncorrectToken("string", "number"),
                span
            ));
        };

        let mut iter = number.0.chars();
        let mut buffer = String::new();

        while let Some(ch) = iter.next() {
            if ch == ESCAPE {
                let Some(escaped) = iter.next() else {
                    return Err(err!(
                        SpannedStringLiteralError,
                        StringLiteralError::InvalidEscape('\0'),
                        span
                    ));
                };

                let escaped = match escaped {
                    QUOTE_ESCAPE => QUOTE_ESCAPE,
                    NEWLINE_ESCAPE => '\n',
                    TAB_ESCAPE => '\t',
                    NULL_ESCAPE => '\0',
                    ch => {
                        return Err(err!(
                            SpannedStringLiteralError,
                            StringLiteralError::InvalidEscape(ch),
                            span
                        ));
                    }
                };

                buffer.push(escaped);
            } else {
                buffer.push(ch)
            }
        }

        stream.sync();
        Ok(Self(buffer))
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
        Parse, TokenStream,
        error::{ParseError, test_error::TestError},
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

        assert_eq!(
            NumberLiteral::parse(&mut ts),
            Ok(NumberLiteral(123_456_789))
        );
        assert!(ts.container.is_empty());
    }

    #[test]
    fn do_not_parse_int_overflow() {
        let input = "2147483648".to_owned(); // (i32::MAX + 1)
        let token = Token::new(
            TokenType::Literal(Literal::NumberLiteral(NL(input))),
            Span::EMPTY,
        );

        let mut ts = TokenStream::new(VecDeque::from([token]));

        assert_eq!(
            NumberLiteral::parse(&mut ts),
            Err(Box::new(TestError::new(1, 0)) as Box<dyn ParseError>)
        );
        assert!(!ts.container.is_empty());
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
        str_test_fail("hello, world\\z", TestError::new(2, 0));
    }

    #[test]
    fn do_not_parse_missing_escape() {
        str_test_fail("hello, world\\", TestError::new(2, 0));
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
        assert!(ts.container.is_empty())
    }

    fn str_test_fail(ex: &str, err: TestError) {
        let token = Token::new(
            TokenType::Literal(Literal::StringLiteral(SL(ex.to_owned()))),
            Span::EMPTY,
        );
        let mut ts = TokenStream::new(VecDeque::from([token]));

        assert_eq!(
            StringLiteral::parse(&mut ts),
            Err(Box::new(err) as Box<dyn ParseError>)
        );
        assert!(!ts.container.is_empty())
    }
}
