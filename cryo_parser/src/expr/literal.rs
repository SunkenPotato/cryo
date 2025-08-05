//! Literal expressions.
//!
//! Literal expressions are expressions that consist of literal tokens, i.e., they are direct and require no further computation when evaluated.

use cryo_lexer::{
    atoms::Minus,
    literal::{IntegerLiteral as IToken, StringLiteral as SToken},
    stream::StreamLike,
};
use cryo_span::Spanned;

use crate::Parse;

/// A literal expression.
///
/// View the module-level docs for more information.
#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
    /// An integer literal.
    IntegerLiteral(Spanned<IntegerLiteral>),
    /// A string literal.
    StringLiteral(Spanned<StringLiteral>),
}

impl Parse for Literal {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        tokens
            .spanning(IntegerLiteral::parse)
            .map(Self::IntegerLiteral)
            .or_else(|_| {
                tokens
                    .spanning(StringLiteral::parse)
                    .map(Self::StringLiteral)
            })
    }
}

/// An integer literal.
///
/// This is a wrapper around [`i32`] and therefore has the same properties as `i32`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntegerLiteral {
    /// The value of this literal.
    Value(i32),
    /// The literal would have resulted in an overflow.
    Overflow,
}

impl Parse for IntegerLiteral {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let mut sgn = 1;

        while tokens.advance_require::<Minus>().is_ok() {
            sgn *= -1;
        }

        let int_token = tokens.advance_require::<IToken>()?;
        let mut int = 0i32;
        for c in int_token.0.chars() {
            if let '_' = c {
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
    /// The value of this literal.
    Value(Box<str>),
    /// The literal contained an invalid escape.
    InvalidEscape(Option<char>),
}

impl Parse for StringLiteral {
    // TODO: add support for unicode escapes
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let token = tokens.advance_require::<SToken>()?;
        let mut buffer = String::with_capacity(token.0.len());

        let mut iter = token.0.chars().peekable();

        while let Some(a) = iter.next() {
            if let '\\' = a {
                buffer.push(match iter.peek() {
                    Some('n') => '\n',
                    Some('t') => '\t',
                    Some('0') => '\0',
                    esc => {
                        return Ok(StringLiteral::InvalidEscape(esc.cloned()));
                    }
                });

                iter.next();
            } else {
                buffer.push(a)
            }
        }
        buffer.shrink_to_fit();

        Ok(Self::Value(buffer.into_boxed_str()))
    }
}

#[cfg(test)]
mod tests {
    use cryo_lexer::{
        Symbol, Token, TokenType,
        atoms::Minus,
        literal::{IntegerLiteral as IntLitToken, Literal as LitToken},
        stream::TokenStream,
    };
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
            TokenStream::new([
                Token::new(TokenType::Minus(Minus), Span::new(0, 1)),
                Token::new(
                    TokenType::Literal(LitToken::IntegerLiteral(IntLitToken(Symbol::new(
                        "123_456",
                    )))),
                    Span::new(1, 8),
                ),
            ]),
            Spanned::new(
                Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(Spanned::new(
                    IntegerLiteral::Value(-123456),
                    Span::new(0, 8),
                )))),
                Span::new(0, 8),
            ),
        );
    }

    #[test]
    fn fail_parse_overflow() {
        assert_parse(
            "2147483648",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(Spanned::new(
                    IntegerLiteral::Overflow,
                    Span::new(0, 10),
                )))),
                Span::new(0, 10),
            ),
        );
    }

    #[test]
    fn parse_str_lit() {
        assert_parse(
            "\"hello, world!\\n\"",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::Lit(Literal::StringLiteral(Spanned::new(
                    StringLiteral::Value(Box::from("hello, world!\n")),
                    Span::new(0, 17),
                )))),
                Span::new(0, 17),
            ),
        );
    }

    #[test]
    fn parse_usual_str_lit() {
        assert_parse(
            "\"hello, world\"",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::Lit(Literal::StringLiteral(Spanned::new(
                    StringLiteral::Value(Box::from("hello, world")),
                    Span::new(0, 14),
                )))),
                Span::new(0, 14),
            ),
        )
    }

    #[test]
    fn fail_parse_str_lit_invalid_escape() {
        assert_parse(
            "\"hello, world\\x\"",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::Lit(Literal::StringLiteral(Spanned::new(
                    StringLiteral::InvalidEscape(Some('x')),
                    Span::new(0, 16),
                )))),
                Span::new(0, 16),
            ),
        );
    }
}
