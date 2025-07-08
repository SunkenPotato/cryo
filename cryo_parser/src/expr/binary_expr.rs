//! Arithmetic expressions.

use crate::{
    expr::{Expr, ReducedExpr},
    parser::Parse,
};
use cryo_lexer::atoms::Operators as OToken;
use cryo_parser_proc_macro::Parse;

/// A binary expression.
///
/// This consists of a left-hand [`ReducedExpr`], an [`Operator`], and a right-hand side [`Expr`].
#[derive(Parse, Debug, PartialEq)]
pub struct BinaryExpr {
    /// The left-hand side of this expression.
    pub lhs: ReducedExpr,
    /// The operator.
    pub op: Operator,
    /// The right-hand side.
    pub rhs: Expr,
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
