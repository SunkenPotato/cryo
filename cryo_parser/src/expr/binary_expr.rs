//! Arithmetic expressions.

use crate::{
    expr::{BaseExpr, Expr},
    parser::Parse,
};
use cryo_lexer::atoms::Operators as OToken;

/// A binary expression.
///
/// This consists of a left-hand [`BaseExpr`], an [`Operator`], and a right-hand side [`Expr`].
// this type should not implement Parse
#[derive(Debug, PartialEq)]
pub struct BinaryExpr {
    /// The left-hand side of this expression.
    pub lhs: BaseExpr,
    /// The operator.
    pub op: Operator,
    /// The right-hand side.
    pub rhs: Box<Expr>,
}

/// Operators used for binary expressions.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
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
    /// The equality operator (`==`).
    Eq,
    /// The inequality operator (`!=`).
    NotEq,
}

impl Operator {
    /// Returns the precedence level for this operator.
    pub fn precedence(&self) -> u8 {
        match self {
            Operator::Mul | Operator::Div | Operator::Rem => 60,
            Operator::Add | Operator::Sub => 50,
            Operator::NotEq | Operator::Eq => 40,
        }
    }
}

impl Parse for Operator {
    type Output = Self;

    #[track_caller]
    fn parse(
        tokens: &mut cryo_lexer::stream::TokenStreamGuard,
    ) -> crate::parser::ParseResult<Self::Output> {
        Ok(tokens.advance_require::<OToken>()?.map(|v| match *v {
            OToken::Add => Self::Add,
            OToken::Sub => Self::Sub,
            OToken::Mul => Self::Mul,
            OToken::Div => Self::Div,
            OToken::Rem => Self::Rem,
            OToken::Eq => Self::Eq,
            OToken::NotEq => Self::NotEq,
        }))
    }
}
