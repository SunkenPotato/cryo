//! Expressions.

use cryo_lexer::{TokenKind, stream::StreamLike};
use cryo_parser_proc_macro::IsFail;
use cryo_span::Spanned;

use crate::{
    ExpectedToken, IsFail, Parse, ParseError, ParseErrorKind, expr::literal::Literal, ident::Ident,
};

/// An operator for a binary operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq, IsFail)]
#[fail = false]
pub enum BinaryOp {
    /// The addition operator `+`.
    Add,
    /// The subtraction operator `-`.
    Sub,
    /// The division operator `/`.
    Div,
    /// The multiplication operator `*`.
    Mul,
    /// The remainder operator `%`.
    Rem,
    /// The dot operator `.`.s
    Dot,
    /// The equality operator `==`.
    Eq,
    /// The inequality operator `!=`.
    NotEq,
}

impl BinaryOp {
    const ACCEPTED_TOKENS: &[TokenKind] = &[
        TokenKind::Plus,
        TokenKind::Minus,
        TokenKind::Slash,
        TokenKind::Star,
        TokenKind::Percent,
        TokenKind::Dot,
        TokenKind::Equal,
        TokenKind::Bang,
    ];

    /// The precedence of this operator.
    pub const fn precedence(self) -> u8 {
        match self {
            Self::Eq | Self::NotEq => 1,
            Self::Add | Self::Sub => 2,
            Self::Mul | Self::Div | Self::Rem => 3,
            Self::Dot => 4,
        }
    }
}

/// A binary expression.
#[derive(Clone, Debug, PartialEq, Eq, IsFail)]
#[fail = false]
pub struct BinaryExpr {
    /// The left-hand side of this expression.
    pub lhs: Box<Spanned<Expr>>,
    /// The binary operator.
    pub op: Spanned<BinaryOp>,
    /// The right-hand side of this expression.
    pub rhs: Box<Spanned<Expr>>,
}

/// A base expression. This differs from a normal [`Expr`], in the way that it cannot be a binary expression.
#[derive(Clone, Debug, PartialEq, Eq, IsFail)]
#[fail(bubble)]
pub enum BaseExpr {
    /// A literal expression.
    Literal(Literal),
    /// A binding usage.
    Binding(Ident),
    /// A unary expression.
    UnaryExpr(Unary),
    /// A parenthesized expression.
    Parenthesized(Box<Expr>),
}

/// An expression.
#[derive(Clone, Debug, PartialEq, Eq, IsFail)]
#[fail(bubble)]
pub enum Expr {
    /// A binary expression.
    BinaryExpr(BinaryExpr),
    /// A base expression.
    BaseExpr(BaseExpr),
}

/// A unary expression.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Unary {
    /// The operator.
    pub op: Spanned<UnaryOp>,
    /// The expression.
    pub expr: Box<Spanned<BaseExpr>>,
}

/// A unary operator, used in unary expressions.
#[derive(Clone, Copy, Debug, PartialEq, Eq, IsFail)]
#[fail = false]
pub enum UnaryOp {
    /// The `!` operator.
    Not,
    /// The `-` operator.
    Neg,
}

impl IsFail for Unary {
    fn is_fail(&self) -> bool {
        self.op.is_fail() && self.expr.is_fail()
    }
}

////////////////////////////////////
// Parsers                        //
////////////////////////////////////

impl Parse for BinaryOp {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let token = tokens.advance()?;
        let t = match token.kind {
            TokenKind::Plus => Self::Add,
            TokenKind::Minus => Self::Sub,
            TokenKind::Slash => Self::Div,
            TokenKind::Star => Self::Mul,
            TokenKind::Percent => Self::Rem,
            TokenKind::Dot => Self::Dot,
            TokenKind::Equal => {
                tokens.advance_require(TokenKind::Equal)?;
                Self::Eq
            }
            TokenKind::Bang => {
                tokens.advance_require(TokenKind::Equal)?;
                Self::NotEq
            }
            _ => {
                return Err(ParseError::new(
                    token.span,
                    token.span,
                    ParseErrorKind::IncorrectToken {
                        got: *token,
                        expected: ExpectedToken::Multiple(Self::ACCEPTED_TOKENS),
                    },
                ));
            }
        };

        Ok(t)
    }
}

impl Parse for BaseExpr {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        if let Ok(l_paren) = tokens.advance_require(TokenKind::LParen) {
            let expr = tokens.with(Expr::parse)?;
            match tokens.advance_require(TokenKind::RParen) {
                Ok(_) => Ok(Self::Parenthesized(Box::new(expr))),
                Err(e) => Err(ParseError {
                    span: e.span(),
                    context: l_paren.span.extend(e.span()),
                    kind: ParseErrorKind::UnclosedDelimiter(TokenKind::LParen),
                }),
            }
        } else {
            Self::parse_1(tokens)
        }
    }
}

impl BaseExpr {
    fn parse_1(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        if let Ok(op) = tokens.spanning(UnaryOp::parse) {
            return Ok(Self::UnaryExpr(Unary {
                op,
                expr: Box::new(tokens.spanning(Self::parse)?),
            }));
        }

        tokens
            .with(Literal::parse)
            .map(Self::Literal)
            .or_else(|_| tokens.with(Ident::parse).map(Self::Binding))
    }
}

impl Expr {
    fn parse_1(tokens: &mut cryo_lexer::stream::Guard, min_prec: u8) -> crate::ParseResult<Self> {
        enum SpannedExpr {
            Base(Spanned<BaseExpr>),
            Binary(Spanned<BinaryExpr>),
        }

        #[expect(clippy::from_over_into)]
        impl Into<Spanned<Expr>> for SpannedExpr {
            fn into(self) -> Spanned<Expr> {
                match self {
                    Self::Base(v) => v.map(Expr::BaseExpr),
                    Self::Binary(v) => v.map(Expr::BinaryExpr),
                }
            }
        }

        let mut lhs = tokens.spanning(BaseExpr::parse).map(SpannedExpr::Base)?;

        while let Ok(op) = tokens.non_consuming(BinaryOp::parse) {
            if op.precedence() <= min_prec {
                break;
            }

            let op = tokens
                .spanning(BinaryOp::parse)
                .expect("operator should parse correctly as it has already been parsed in non-consuming mode");

            let rhs = tokens.spanning(|tokens| Expr::parse_1(tokens, op.precedence()))?;
            let rhs_span = rhs.span;

            let spanned_lhs = lhs.into();

            let bin_expr = BinaryExpr {
                lhs: Box::new(spanned_lhs),
                op,
                rhs: Box::new(rhs),
            };
            let lhs_span = bin_expr.lhs.span;

            lhs = SpannedExpr::Binary(Spanned::new(
                bin_expr,
                lhs_span.extend(op.span).extend(rhs_span),
            ));
        }

        Ok(Into::<Spanned<Expr>>::into(lhs).t)
    }
}

impl Parse for Expr {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        Self::parse_1(tokens, 0)
    }
}

impl UnaryOp {
    /// A list of tokens accepted by the [`UnaryOp`] parser.
    pub const ACCEPTED_TOKENS: &[TokenKind] = &[TokenKind::Bang];
}

impl Parse for UnaryOp {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let token = tokens.advance()?;
        match token.kind {
            TokenKind::Bang => Ok(Self::Not),
            TokenKind::Minus => Ok(Self::Neg),
            _ => Err(ParseError::new(
                token.span,
                token.span,
                ParseErrorKind::IncorrectToken {
                    got: *token,
                    expected: ExpectedToken::Multiple(Self::ACCEPTED_TOKENS),
                },
            )),
        }
    }
}

/// Literal parsers and structs.
pub mod literal {
    use crate::{IsFail, Parse};
    use cryo_lexer::{
        Symbol, TokenKind,
        stream::{StreamLike, TokenStreamError},
    };
    use cryo_parser_proc_macro::IsFail;

    /// A literal expression.
    #[derive(Debug, PartialEq, Eq, Clone, IsFail)]
    #[fail(bubble)]
    pub enum Literal {
        /// A string literal.
        StringLiteral(StringLiteral),
        /// An integer literal.
        IntegerLiteral(IntegerLiteral),
    }

    /// A string literal expression.
    #[derive(Debug, PartialEq, Eq, Clone, IsFail)]
    #[fail = false]
    pub enum StringLiteral {
        /// A fully parsed string literal.
        Value(Symbol),
        /// An invalid escape was encountered.
        InvalidEscape(Symbol, char),
    }

    /// An integer literal expression.
    #[derive(Debug, PartialEq, Eq, Clone, IsFail)]
    pub enum IntegerLiteral {
        /// A fully parsed integer literal.
        Value(i32),
        /// A series of `-` was parsed, but no integer literal.
        #[fail]
        Negations,
        /// An overflow would have occurred.
        #[fail]
        Overflow(i32),
    }

    ////////////////////////////////////
    // Parsers                        //
    ////////////////////////////////////

    impl Parse for Literal {
        fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
            tokens
                .with(StringLiteral::parse)
                .map(Self::StringLiteral)
                .or_else(|_| tokens.with(IntegerLiteral::parse).map(Self::IntegerLiteral))
        }
    }

    impl Parse for StringLiteral {
        fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
            let str_token = tokens.advance_require(TokenKind::StringLiteral)?;
            let mut buf = String::with_capacity(str_token.len());

            let mut iter = str_token.chars();

            while let Some(ch) = iter.next() {
                let new_ch = if ch == '\\' {
                    // lexer guarantees there will be a next character
                    let esc = iter.next().unwrap();

                    match esc {
                        '\\' => '\\',
                        'n' => '\n',
                        't' => '\t',
                        '0' => '\0',
                        other => return Ok(Self::InvalidEscape(buf.as_str().into(), other)),
                    }
                } else {
                    ch
                };

                buf.push(new_ch)
            }

            Ok(Self::Value(buf.as_str().into()))
        }
    }

    impl Parse for IntegerLiteral {
        fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
            let mut neg = 1;

            while tokens.advance_require(TokenKind::Minus).is_ok() {
                neg *= -1;
            }

            let int_token = match tokens.advance_require(TokenKind::IntegerLiteral) {
                Ok(v) => v,
                Err(TokenStreamError::EndOfInput { .. }) => return Ok(Self::Negations),
                Err(e) => return Err(e.into()),
            };

            let mut int = 0i32;

            for digit in int_token.chars() {
                if let '0'..='9' = digit {
                    let snapshot = int;
                    int = match int.checked_mul(10) {
                        Some(v) => v,
                        None => return Ok(Self::Overflow(snapshot)),
                    };

                    int = match int.checked_add(digit.to_digit(10).unwrap() as i32) {
                        Some(v) => v,
                        None => return Ok(Self::Overflow(snapshot)),
                    };
                }
            }

            int *= neg;

            Ok(Self::Value(int))
        }
    }

    ////////////////////////////////////
    // Tests                          //
    ////////////////////////////////////

    #[cfg(test)]
    mod tests {
        use super::*;
        use cryo_span::{Span, Spanned};

        use crate::test_util::assert_parse;

        #[test]
        fn parse_int() {
            assert_parse(
                "--123_456",
                Spanned::new(IntegerLiteral::Value(123_456), Span::new(0, 9)),
            );
        }

        #[test]
        fn parse_int_only_neg_sign() {
            assert_parse(
                "--",
                Spanned::new(IntegerLiteral::Negations, Span::new(0, 2)),
            )
        }

        #[test]
        fn parse_int_overflow() {
            assert_parse(
                "2147483648",
                Spanned::new(IntegerLiteral::Overflow(214748364), Span::new(0, 10)),
            );
        }

        #[test]
        fn parse_str() {
            assert_parse(
                "\"Hello, world!\\n\"",
                Spanned::new(
                    StringLiteral::Value("Hello, world!\n".into()),
                    Span::new(0, 17),
                ),
            );
        }

        #[test]
        fn parse_str_inv_escape() {
            assert_parse(
                "\"Hello, world!\\f\"",
                Spanned::new(
                    StringLiteral::InvalidEscape("Hello, world!".into(), 'f'),
                    Span::new(0, 17),
                ),
            )
        }
    }
}

////////////////////////////////////
// Tests                          //
////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;
    use cryo_span::{Span, Spanned};

    use crate::{expr::literal::IntegerLiteral, test_util::assert_parse};

    #[test]
    fn parse_bin_expr() {
        assert_parse(
            "2 + 2",
            Spanned::new(
                Expr::BinaryExpr(BinaryExpr {
                    lhs: Box::new(Spanned::new(
                        Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                            IntegerLiteral::Value(2),
                        ))),
                        Span::new(0, 1),
                    )),
                    op: Spanned::new(BinaryOp::Add, Span::new(2, 3)),
                    rhs: Box::new(Spanned::new(
                        Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                            IntegerLiteral::Value(2),
                        ))),
                        Span::new(4, 5),
                    )),
                }),
                Span::new(0, 5),
            ),
        );
    }

    #[test]
    fn parse_bin_expr_nested() {
        // [(4 * 3) - 2] == (2 * 5)
        assert_parse(
            "4 * 3 - 2 == 2 * 5",
            Spanned::new(
                Expr::BinaryExpr(BinaryExpr {
                    // 4 * 3 - 2
                    lhs: Box::new(Spanned::new(
                        Expr::BinaryExpr(BinaryExpr {
                            // 4 * 3
                            lhs: Box::new(Spanned::new(
                                Expr::BinaryExpr(BinaryExpr {
                                    // 4
                                    lhs: Box::new(Spanned::new(
                                        Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                                            IntegerLiteral::Value(4),
                                        ))),
                                        Span::new(0, 1),
                                    )),
                                    // *
                                    op: Spanned::new(BinaryOp::Mul, Span::new(2, 3)),
                                    // 3
                                    rhs: Box::new(Spanned::new(
                                        Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                                            IntegerLiteral::Value(3),
                                        ))),
                                        Span::new(4, 5),
                                    )),
                                }),
                                Span::new(0, 5),
                            )),
                            // -
                            op: Spanned::new(BinaryOp::Sub, Span::new(6, 7)),
                            // 2
                            rhs: Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                                    IntegerLiteral::Value(2),
                                ))),
                                Span::new(8, 9),
                            )),
                        }),
                        Span::new(0, 9),
                    )),
                    // ==
                    op: Spanned::new(BinaryOp::Eq, Span::new(10, 12)),
                    // 2 * 5
                    rhs: Box::new(Spanned::new(
                        Expr::BinaryExpr(BinaryExpr {
                            // 2
                            lhs: Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                                    IntegerLiteral::Value(2),
                                ))),
                                Span::new(13, 14),
                            )),
                            // *
                            op: Spanned::new(BinaryOp::Mul, Span::new(15, 16)),
                            // 5
                            rhs: Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                                    IntegerLiteral::Value(5),
                                ))),
                                Span::new(17, 18),
                            )),
                        }),
                        Span::new(13, 18),
                    )),
                }),
                Span::new(0, 18),
            ),
        )
    }

    #[test]
    fn parse_unary_ops() {
        assert_parse(
            "!(0==0)",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::UnaryExpr(Unary {
                    op: Spanned::new(UnaryOp::Not, Span::new(0, 1)),
                    expr: Box::new(Spanned::new(
                        BaseExpr::Parenthesized(Box::new(Expr::BinaryExpr(BinaryExpr {
                            lhs: Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                                    IntegerLiteral::Value(0),
                                ))),
                                Span::new(2, 3),
                            )),
                            op: Spanned::new(BinaryOp::Eq, Span::new(3, 5)),
                            rhs: Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                                    IntegerLiteral::Value(0),
                                ))),
                                Span::new(5, 6),
                            )),
                        }))),
                        Span::new(1, 7),
                    )),
                })),
                Span::new(0, 7),
            ),
        )
    }

    #[test]
    fn parse_binary_with_unary() {
        assert_parse(
            "5 - -5",
            Spanned::new(
                Expr::BinaryExpr(BinaryExpr {
                    lhs: Box::new(Spanned::new(
                        Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                            IntegerLiteral::Value(5),
                        ))),
                        Span::new(0, 1),
                    )),
                    op: Spanned::new(BinaryOp::Sub, Span::new(2, 3)),
                    rhs: Box::new(Spanned::new(
                        Expr::BaseExpr(BaseExpr::UnaryExpr(Unary {
                            op: Spanned::new(UnaryOp::Neg, Span::new(4, 5)),
                            expr: Box::new(Spanned::new(
                                BaseExpr::Literal(Literal::IntegerLiteral(IntegerLiteral::Value(
                                    5,
                                ))),
                                Span::new(5, 6),
                            )),
                        })),
                        Span::new(4, 6),
                    )),
                }),
                Span::new(0, 6),
            ),
        );
    }
}
