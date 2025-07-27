//! Expressions.
//!
//! Expressions are components, that, when evaluated, produce a value.

use std::fmt::Debug;

use cryo_lexer::{
    TokenType,
    atoms::Equal,
    stream::{Guard, StreamLike},
};

use crate::{Parse, expr::literal::Literal, ident::Ident};

pub mod literal;

/// Binary operators.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Operator {
    /// The addition operator.
    Add,
    /// The subtraction operator.
    Sub,
    /// The multiplication operator.
    Mul,
    /// The division operator.
    Div,
    /// The remainder operator (`%`).
    Rem,
    /// The equality comparison operator.
    Eq,
    /// The inequality comparison operator.
    NotEq,
}

impl Operator {
    fn parse_1<const CONSUME: bool>(tokens: &mut Guard) -> crate::ParseResult<Self> {
        let op = match tokens.peek()?.t {
            TokenType::Plus(_) => Operator::Add,
            TokenType::Minus(_) => Operator::Sub,
            TokenType::Star(_) => Operator::Mul,
            TokenType::Slash(_) => Operator::Div,
            TokenType::Percent(_) => Operator::Rem,
            TokenType::Equal(_) => {
                tokens.peek_require::<Equal>()?;
                if CONSUME {
                    tokens
                        .advance()
                        .expect("stream should not be empty since it has been checked");
                }
                Operator::Eq
            }
            TokenType::Bang(_) => {
                tokens.peek_require::<Equal>()?;
                if CONSUME {
                    tokens
                        .advance()
                        .expect("stream should not be empty since it has been checked");
                }
                Operator::NotEq
            }
            t => {
                return Err(crate::ParseError::TokenStreamError(
                    cryo_lexer::stream::TokenStreamError::IncorrectToken(t),
                ));
            }
        };
        if CONSUME {
            tokens
                .advance()
                .expect("stream should not be empty since it has been checked");
        }
        Ok(op)
    }
}

impl Parse for Operator {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        Self::parse_1::<true>(tokens)
    }
}

impl Operator {
    /// Return the precedence of this operator.
    pub const fn precedence(&self) -> u8 {
        match self {
            Self::NotEq | Self::Eq => 1,
            Self::Add | Self::Sub => 2,
            Self::Mul | Self::Div | Self::Rem => 3,
        }
    }
}

/// A binary expression.
#[derive(Debug, PartialEq, Eq)]
pub struct BinaryExpr {
    /// The left-hand side of this expression.
    pub lhs: Box<Expr>,
    /// The operator.
    pub op: Operator,
    /// The right-hand side of this expression.
    pub rhs: Box<Expr>,
}

/// An expression.
#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    /// A simple expression.
    BaseExpr(BaseExpr),
    /// A binary expression.
    BinaryExpr(BinaryExpr),
}

impl Expr {
    fn parse_1(tokens: &mut Guard, min_prec: u8) -> crate::ParseResult<Self> {
        let mut lhs = tokens.with(BaseExpr::parse).map(Self::BaseExpr)?;

        while let Ok(op) = tokens.with(Operator::parse_1::<false>) {
            if op.precedence() <= min_prec {
                break;
            }

            tokens.with(Operator::parse).expect(
                "operator should parse correctly since operator token has already been consumed",
            );

            let rhs = tokens.with(|tokens| Expr::parse_1(tokens, op.precedence()))?;

            lhs = Self::BinaryExpr(BinaryExpr {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            })
        }

        Ok(lhs)
    }
}

impl Parse for Expr {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        Self::parse_1(tokens, 0)
    }
}

/// A base, or simple expression.
#[derive(Debug, PartialEq, Eq)]
pub enum BaseExpr {
    /// A literal value.
    Lit(Literal),
    /// A binding usage.
    BindingUsage(Ident),
}

impl Parse for BaseExpr {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        tokens
            .with(Literal::parse)
            .map(Self::Lit)
            .or_else(|_| tokens.with(Ident::parse).map(Self::BindingUsage))
    }
}

#[cfg(test)]
mod tests {
    #![allow(missing_docs)]
    use cryo_span::{Span, Spanned};

    use crate::{
        expr::{
            BaseExpr, BinaryExpr, Expr, Operator,
            literal::{IntegerLiteral, Literal},
        },
        test_util::assert_parse,
    };

    #[test]
    fn parse_add() {
        assert_parse(
            "5 + 6",
            Spanned::new(
                Expr::BinaryExpr(BinaryExpr {
                    lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                        IntegerLiteral::Value(5),
                    )))),
                    op: super::Operator::Add,
                    rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                        IntegerLiteral::Value(6),
                    )))),
                }),
                Span::new(0, 5),
            ),
        );
    }

    #[test]
    fn parse_add_mul() {
        assert_parse(
            // (5) + (6 * 2)
            "5 + 6 * 2",
            Spanned::new(
                Expr::BinaryExpr(BinaryExpr {
                    lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                        IntegerLiteral::Value(5),
                    )))),
                    op: Operator::Add,
                    rhs: Box::new(Expr::BinaryExpr(BinaryExpr {
                        lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(6),
                        )))),
                        op: Operator::Mul,
                        rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(2),
                        )))),
                    })),
                }),
                Span::new(0, 9),
            ),
        );
    }

    #[test]
    fn parse_mul_add() {
        assert_parse(
            //((6 * 2)+(5))
            //((6) * (2 + 5))
            "6 * 2 + 5",
            Spanned::new(
                Expr::BinaryExpr(BinaryExpr {
                    lhs: Box::new(Expr::BinaryExpr(BinaryExpr {
                        lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(6),
                        )))),
                        op: super::Operator::Mul,
                        rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(2),
                        )))),
                    })),
                    op: Operator::Add,
                    rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                        IntegerLiteral::Value(5),
                    )))),
                }),
                Span::new(0, 9),
            ),
        );
    }

    #[test]
    fn parse_complex_binary_expr() {
        assert_parse(
            "2 + 3 * 4 == 6 / 2 - 2",
            Spanned::new(
                Expr::BinaryExpr(BinaryExpr {
                    lhs: Box::new(Expr::BinaryExpr(BinaryExpr {
                        lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(2),
                        )))),
                        op: Operator::Add,
                        rhs: Box::new(Expr::BinaryExpr(BinaryExpr {
                            lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                IntegerLiteral::Value(3),
                            )))),
                            op: Operator::Mul,
                            rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                IntegerLiteral::Value(4),
                            )))),
                        })),
                    })),
                    op: Operator::Eq,
                    rhs: Box::new(Expr::BinaryExpr(BinaryExpr {
                        lhs: Box::new(Expr::BinaryExpr(BinaryExpr {
                            lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                IntegerLiteral::Value(6),
                            )))),
                            op: Operator::Div,
                            rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                IntegerLiteral::Value(2),
                            )))),
                        })),
                        op: Operator::Sub,
                        rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(2),
                        )))),
                    })),
                }),
                Span::new(0, 22),
            ),
        );
    }
}
