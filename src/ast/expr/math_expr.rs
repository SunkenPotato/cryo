use crate::{
    ast::{Parse, extract_whitespace, literal::Literal},
    group,
};

use super::{Expr, ExprParseError};

group! {
    #[derive(Debug, PartialEq)]
    pub enum MathExprParseError {
        ExprParseError(ExprParseError),
        OpParseError(OpParseError)
    }
}

#[derive(Debug, PartialEq)]
pub struct MathExpr {
    pub lhs: Expr,
    pub op: Op,
    pub rhs: Expr,
}

impl Parse for MathExpr {
    type Error = MathExprParseError;

    fn parse(input: &str) -> Result<(Self, &str), Self::Error> {
        let (lhs, rest) = match Literal::parse(input) {
            Ok(v) => v,
            Err(e) => {
                return Err(MathExprParseError::ExprParseError(
                    ExprParseError::LiteralParseError(e),
                ));
            }
        };
        let (op, rest) = Op::parse(rest)?;
        let (rhs, rest) = Expr::parse(rest)?;
        let lhs = Expr::Literal(lhs);

        let s = Self { lhs, op, rhs };

        Ok((s, rest))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Op {
    Add,
    Sub,
    Div,
    Mul,
    Mod,
}

#[derive(Debug, PartialEq)]
pub enum OpParseError {
    UnexpectedToken(char),
    Empty,
}

impl Parse for Op {
    type Error = OpParseError;

    fn parse(input: &str) -> Result<(Self, &str), Self::Error> {
        let input = extract_whitespace(input);
        let next = input.chars().next().ok_or(OpParseError::Empty)?;

        let op = match next {
            '+' => Op::Add,
            '-' => Op::Sub,
            '*' => Op::Mul,
            '/' => Op::Div,
            '%' => Op::Mod,
            c => return Err(OpParseError::UnexpectedToken(c)),
        };

        Ok((op, &input[1..]))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::literal::{Literal, integer::Integer};

    use super::*;

    #[test]
    fn parse_add() {
        assert_eq!(Op::parse("+"), Ok((Op::Add, "")));
        assert_eq!(Op::parse("+ "), Ok((Op::Add, " ")));
        assert_eq!(Op::parse(" +"), Ok((Op::Add, "")));
    }

    #[test]
    fn parse_sub() {
        assert_eq!(Op::parse("-"), Ok((Op::Sub, "")));
        assert_eq!(Op::parse("- "), Ok((Op::Sub, " ")));
        assert_eq!(Op::parse(" -"), Ok((Op::Sub, "")));
    }

    #[test]
    fn parse_mul() {
        assert_eq!(Op::parse("*"), Ok((Op::Mul, "")));
        assert_eq!(Op::parse("* "), Ok((Op::Mul, " ")));
        assert_eq!(Op::parse(" *"), Ok((Op::Mul, "")));
    }

    #[test]
    fn parse_div() {
        assert_eq!(Op::parse("/"), Ok((Op::Div, "")));
        assert_eq!(Op::parse("/ "), Ok((Op::Div, " ")));
        assert_eq!(Op::parse(" /"), Ok((Op::Div, "")));
    }

    #[test]
    fn parse_mod() {
        assert_eq!(Op::parse("%"), Ok((Op::Mod, "")));
        assert_eq!(Op::parse("% "), Ok((Op::Mod, " ")));
        assert_eq!(Op::parse(" %"), Ok((Op::Mod, "")));
    }

    #[test]
    fn parse_errors() {
        assert_eq!(Op::parse(""), Err(OpParseError::Empty));
        assert_eq!(Op::parse("@"), Err(OpParseError::UnexpectedToken('@')));
        assert_eq!(Op::parse("a"), Err(OpParseError::UnexpectedToken('a')));
    }

    #[test]
    fn parse_math_expr() {
        let expr = "5 + 7 / 2";

        assert_eq!(
            MathExpr::parse(expr),
            Ok((
                MathExpr {
                    lhs: Expr::Literal(Literal::Integer(Integer(5))),
                    op: Op::Add,
                    rhs: Expr::MathExpr(
                        MathExpr {
                            lhs: Expr::Literal(Literal::Integer(Integer(7))),
                            op: Op::Div,
                            rhs: Expr::Literal(Literal::Integer(Integer(2)))
                        }
                        .into()
                    )
                },
                ""
            ))
        )
    }
}
