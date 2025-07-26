//! Literal expressions.
//!
//! Literal expressions are expressions that consist of literal tokens, i.e., they are direct and require no further computation when evaluated.

use cryo_lexer::{
    literal::{IntegerLiteral as IToken, StringLiteral as SToken},
    stream::StreamLike,
};
use itertools::Itertools;

use crate::Parse;

/// A literal expression.
///
/// View the module-level docs for more information.
#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
    /// An integer literal.
    IntegerLiteral(IntegerLiteral),
    /// A string literal.
    StringLiteral(StringLiteral),
}

impl Parse for Literal {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        tokens
            .with(IntegerLiteral::parse)
            .map(Self::IntegerLiteral)
            .or_else(|_| tokens.with(StringLiteral::parse).map(Self::StringLiteral))
    }
}

/// An integer literal.
///
/// This is a wrapper around [`i32`] and therefore has the same properties as `i32`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntegerLiteral {
    Value(i32),
    Overflow,
}

impl Parse for IntegerLiteral {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let token = tokens.advance_require::<IToken>()?;
        let mut int = 0i32;
        let mut sgn = 1;

        for c in token.0.chars() {
            if let '_' = c {
                continue;
            } else if let '-' = c {
                sgn *= -1;
                continue;
            }

            let digit = (c as u32).cast_signed() - 0x30;
            int = match int.checked_mul(10) {
                Some(v) => v,
                _ => return Ok(IntegerLiteral::Overflow),
            };

            int = match int.checked_add(digit) {
                Some(v) => v,
                _ => return Ok(IntegerLiteral::Overflow),
            };
        }

        Ok(Self::Value(int * sgn))
    }
}

/// An unescaped string literal.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StringLiteral {
    Value(Box<str>),
    InvalidEscape(char),
}

impl Parse for StringLiteral {
    // TODO: add support for unicode escapes
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let token = tokens.advance_require::<SToken>()?;
        let mut buffer = String::with_capacity(token.0.len());

        let mut iter = token.0.chars().tuple_windows();

        while let Some((a, b)) = iter.next() {
            if let '\\' = a {
                buffer.push(match b {
                    'n' => '\n',
                    't' => '\t',
                    '0' => '\0',
                    '"' => '"',
                    esc => {
                        return Ok(StringLiteral::InvalidEscape(esc));
                    }
                });

                iter.next();
            } else {
                // a because the last b is guaranteed to be '"'
                buffer.push(a)
            }
        }
        buffer.shrink_to_fit();

        Ok(Self::Value(buffer.into_boxed_str()))
    }
}

#[cfg(test)]
mod tests {
    use cryo_span::{Span, Spanned};

    use crate::{
        expr::{
            BaseExpr, Expr,
            literal::{IntegerLiteral, Literal, StringLiteral},
        },
        test_util::assert_parse,
    };

    #[test]
    fn parse_int_literal() {
        assert_parse(
            "--123_456",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                    IntegerLiteral::Value(123456),
                ))),
                Span::new(0, 9),
            ),
        );
    }

    #[test]
    fn fail_parse_overflow() {
        assert_parse(
            "2147483648",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                    IntegerLiteral::Overflow,
                ))),
                Span::new(0, 10),
            ),
        );
    }

    #[test]
    fn parse_str_lit() {
        assert_parse(
            "\"hello, world!\\n\"",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::Lit(Literal::StringLiteral(StringLiteral::Value(
                    Box::from("hello, world!\n"),
                )))),
                Span::new(0, 17),
            ),
        );
    }

    #[test]
    fn fail_parse_str_lit_invalid_escape() {
        assert_parse(
            "\"hello, world\\x\"",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::Lit(Literal::StringLiteral(
                    StringLiteral::InvalidEscape('x'),
                ))),
                Span::new(0, 16),
            ),
        );
    }
}
