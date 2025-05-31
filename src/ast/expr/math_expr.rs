use crate::{
    ast::{Parse, ParseError},
    lexer::tokens::{Operation as OpToken, Token},
};

use super::{Expr, ReducedExpr};

#[derive(Debug, PartialEq)]
pub struct MathExpr {
    pub lhs: Expr,
    pub op: Operation,
    pub rhs: Expr,
}

impl Parse for MathExpr {
    fn parse(stream: &[Token]) -> Result<(Self, &[Token]), ParseError> {
        let (lhs, rest) = ReducedExpr::parse(stream)?;
        let (op, rest) = Operation::parse(rest)?;
        let (rhs, rest) = Expr::parse(rest)?;

        let s = Self {
            lhs: lhs.into(),
            op,
            rhs,
        };

        Ok((s, rest))
    }
}

#[derive(Debug, PartialEq)]
pub enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl From<OpToken> for Operation {
    fn from(value: OpToken) -> Self {
        match value {
            OpToken::Add => Self::Add,
            OpToken::Sub => Self::Sub,
            OpToken::Mul => Self::Mul,
            OpToken::Div => Self::Div,
            OpToken::Mod => Self::Mod,
        }
    }
}

impl Parse for Operation {
    fn parse(stream: &[Token]) -> Result<(Self, &[Token]), ParseError> {
        let token = stream.first().ok_or(ParseError::end_of_input(stream))?;
        let op: OpToken = *token
            .as_any()
            .require_err(ParseError::expected_operation(&stream[0..1]))?;

        Ok((op.into(), &stream[1..]))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Parse;
    use crate::ast::expr::Expr;
    use crate::ast::expr::literal::{Literal, NumberLiteral};
    use crate::ast::expr::math_expr::{MathExpr, Operation};
    use crate::token;

    #[test]
    fn parse_simple_math_expr() {
        let s = &[token![ln 5], token![op+], token![ln 3]]; // 5 + 3

        assert_eq!(
            Ok((
                MathExpr {
                    lhs: Expr::Literal(Literal::NumberLiteral(NumberLiteral(5))),
                    op: Operation::Add,
                    rhs: Expr::Literal(Literal::NumberLiteral(NumberLiteral(3)))
                },
                &[][..]
            )),
            MathExpr::parse(s)
        );
    }

    #[test]
    fn parse_complex_math_expr() {
        let s = &[
            token![ln 5],
            token![op+],
            token![ln 3],
            token![op/],
            token![ln 4],
        ]; // 5 + 3 / 4

        assert_eq!(
            Ok((
                MathExpr {
                    lhs: Expr::Literal(Literal::NumberLiteral(NumberLiteral(5))),
                    op: Operation::Add,
                    rhs: Expr::MathExpr(Box::new(MathExpr {
                        lhs: Expr::Literal(Literal::NumberLiteral(NumberLiteral(3))),
                        op: Operation::Div,
                        rhs: Expr::Literal(Literal::NumberLiteral(NumberLiteral(4)))
                    }))
                },
                &[][..]
            )),
            MathExpr::parse(s)
        );
    }
}
