//! Expressions.
//!
//! Expressions are defined as statements of code that return a value, be it `null`, `()`, or any other.
//!
//! For different types of expressions, view the [`Expr`] enum.

use math_expr::Operator;

use crate::{
    Parse,
    error::ParseError,
    expr::{literal::Literal, math_expr::MathExpr},
};

pub mod literal;
pub mod math_expr;

/// A single expression.
///
/// Expressions can have two different types, namely:
/// - [`MathExpr`]
/// - [`ReducedExpr`]
///
/// Due to [`MathExpr`] consisting of possibly many different chained `Expr`s,
/// `Expr` is split up into `MathExpr` and `ReducedExpr`, which contains all [`Expr`] variants except for [`MathExpr`].
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    /// All expression types, except for a [`MathExpr`].
    ReducedExpr(ReducedExpr),
    /// A [`MathExpr`].
    MathExpr(Box<MathExpr>),
}

impl Parse for Expr {
    fn parse(stream: &mut crate::TokenStream) -> Result<Self, Box<dyn ParseError>> {
        let possible_op_token = stream.peek_n(1);

        if let Ok(v) = possible_op_token {
            if v.is::<Operator>() {
                return Ok(Expr::MathExpr(Box::new(MathExpr::parse(stream)?)));
            }
        }
        return Ok(Expr::ReducedExpr(ReducedExpr::parse(stream)?));
    }
}

impl From<ReducedExpr> for Expr {
    fn from(value: ReducedExpr) -> Self {
        Self::ReducedExpr(value)
    }
}

/// An expression with all variants, except for [`MathExpr`].
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ReducedExpr {
    /// A literal expression.
    Literal(Literal),
}

impl Parse for ReducedExpr {
    fn parse(stream: &mut crate::TokenStream) -> Result<Self, Box<dyn ParseError>> {
        Literal::parse(stream).map(Self::Literal)
    }
}
