//! Arithmetic expressions.
//!
//! These are defined as consisting of an [`Expr`] evaluating to an integer (LHS), an [`Operator`], and another `Expr` evaluating to an integer (RHS).

use cryo_lexer::operation::Operation as OpToken;

use crate::{
    Parse,
    error::ParseError,
    expr::{Expr, ReducedExpr},
};

/// An arithmetic expression.
///
/// An arithmetic expression must consist of a left hand expression, an operator, and a right hand expression.
///
/// These can be nested and act as a tree.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct MathExpr {
    /// The left hand side of the expression.
    pub lhs: Expr,
    /// The operator.
    pub op: Operator,
    /// The right hand side of the expression.
    pub rhs: Expr,
}

impl Parse for MathExpr {
    #[track_caller]
    fn parse(stream: &mut crate::TokenStream) -> Result<Self, Box<dyn ParseError>> {
        println!("d");
        let lhs = match ReducedExpr::parse(stream) {
            Ok(v) => Expr::ReducedExpr(v),
            Err(e) => return Err(e),
        };
        println!("c");

        let op = Operator::parse(stream)?;
        println!("b");
        let rhs = Expr::parse(stream)?;

        println!("a");

        Ok(Self { lhs, op, rhs })
    }
}

/// Arithmetic operators.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Operator {
    /// The addition operator (`+`).
    Add,
    /// The subtraction operator (`-`).
    Sub,
    /// The multiplication operator (`*`).
    Mul,
    /// The division operator (`/`).
    Div,
    /// The remainder operator (`%`).
    Rem,
}

impl From<OpToken> for Operator {
    fn from(value: OpToken) -> Self {
        match value {
            OpToken::Add => Self::Add,
            OpToken::Sub => Self::Sub,
            OpToken::Mul => Self::Mul,
            OpToken::Div => Self::Div,
            OpToken::Rem => Self::Rem,
        }
    }
}

impl Parse for Operator {
    fn parse(stream: &mut crate::TokenStream) -> Result<Self, Box<dyn ParseError>> {
        let token = stream.advance_require::<OpToken>()?;
        let inner = *token.token;

        stream.sync();

        Ok((inner).into())
    }
}

#[cfg(test)]
mod tests {
    use cryo_lexer::t;

    use crate::{
        Parse, TokenStream,
        expr::{
            Expr, ReducedExpr,
            literal::{Literal, NumberLiteral},
            math_expr::{MathExpr, Operator},
        },
    };

    #[test]
    fn parse_op() {
        let mut ts = TokenStream::new([t![op+]]);

        assert_eq!(Operator::parse(&mut ts), Ok(Operator::Add));
        assert!(ts.container.is_empty())
    }

    #[test]
    fn parse_math_expr() {
        let mut ts = TokenStream::new([t![nl 5], t![op+], t![nl 5]]);

        assert_eq!(
            MathExpr::parse(&mut ts),
            Ok(MathExpr {
                lhs: Expr::ReducedExpr(ReducedExpr::Literal(Literal::NumberLiteral(
                    NumberLiteral(5)
                ))),
                op: Operator::Add,
                rhs: Expr::ReducedExpr(ReducedExpr::Literal(Literal::NumberLiteral(
                    NumberLiteral(5)
                )))
            })
        )
    }
}
