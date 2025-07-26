use std::fmt::Debug;

use cryo_lexer::stream::{Guard, StreamLike};

use crate::{Parse, expr::literal::Literal};
use cryo_lexer::atoms::Operator as OpToken;

pub mod literal;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    NotEq,
}

impl Parse for Operator {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        tokens
            .advance_require::<OpToken>()
            .map(|v| (*v.t).into())
            .map_err(Into::into)
    }
}

impl Operator {
    pub const fn precedence(&self) -> u8 {
        match self {
            Self::NotEq | Self::Eq => 1,
            Self::Add | Self::Sub => 2,
            Self::Mul | Self::Div | Self::Rem => 3,
        }
    }
}

impl From<OpToken> for Operator {
    fn from(value: OpToken) -> Self {
        match value {
            OpToken::Add => Self::Add,
            OpToken::Sub => Self::Sub,
            OpToken::Mul => Self::Mul,
            OpToken::Div => Self::Div,
            OpToken::Rem => Self::Rem,
            OpToken::Eq => Self::Eq,
            OpToken::NotEq => Self::NotEq,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub op: Operator,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    BaseExpr(BaseExpr),
    BinaryExpr(BinaryExpr),
}

impl Expr {
    fn parse_1(tokens: &mut Guard, min_prec: u8) -> crate::ParseResult<Self> {
        let mut lhs = tokens.with(BaseExpr::parse).map(Self::BaseExpr)?;

        while let Ok(op) = tokens.peek_require::<OpToken>() {
            let op: Operator = (*op.t).into();

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

#[derive(Debug, PartialEq, Eq)]
pub enum BaseExpr {
    Lit(Literal),
}

impl Parse for BaseExpr {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        tokens.with(Literal::parse).map(Self::Lit)
    }
}

#[cfg(test)]
mod tests {
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
    #[ignore = "integer parsing cannot differentiate between subtraction and negation operator"]
    fn parse_complex_binary_expr() {
        assert_parse(
            // (2 + (3 * 4)) == ((6 * 2) + 2)
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
