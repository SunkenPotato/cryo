//! Expressions.
//!
//! Expressions are components of programming languages that, when evaluated, return a value.

pub mod binding_ref;
pub mod block;
pub mod cond_expr;
pub mod literal;
pub mod math_expr;

#[cfg(test)]
mod tests;

use cryo_parser_proc_macro::Parse;
use cryo_span::Spanned;

use crate::{
    expr::{
        binding_ref::BindingRef,
        block::Block,
        cond_expr::IfExpr,
        literal::Literal,
        math_expr::{MathExpr, Operator},
    },
    parser::Parse,
};

/// An expression.
///
/// `Expr` is split up into `MathExpr` and `ReducedExpr` to ease `MathExpr` parsing.
///
/// For the other expression types, view [`ReducedExpr`].
// derive(Parse) cannot be applied to this to prevent double ReducedExpr parsing
#[derive(Debug, PartialEq)]
pub enum Expr {
    /// An arithmetic expression.
    MathExpr(Box<MathExpr>),
    /// Any other expression type.
    ReducedExpr(ReducedExpr),
}

impl Parse for Expr {
    type Output = Self;
    fn parse(
        tokens: &mut cryo_lexer::stream::TokenStreamGuard,
    ) -> crate::parser::ParseResult<Self::Output> {
        let lhs = tokens.with(ReducedExpr::parse)?;
        if let Ok(Spanned {
            t: op,
            span: op_span,
        }) = tokens.with(Operator::parse)
        {
            let (rhs, rhs_span) = tokens.with(Self::parse)?.tuple();
            let span = lhs.span + op_span + rhs_span;
            Ok(Spanned::new(
                Self::MathExpr(Box::new(MathExpr {
                    lhs: lhs.t,
                    op,
                    rhs,
                })),
                span,
            ))
        } else {
            Ok(lhs.map(Self::ReducedExpr))
        }
    }
}

/// Expressions excluding `MathExpr`.
#[derive(Parse, Debug, PartialEq)]
pub enum ReducedExpr {
    /// A literal expression.
    Literal(Literal),
    /// A binding reference.
    BindingRef(BindingRef),
    /// A block expression.
    Block(Block),
    /// A conditional expression.
    IfExpr(IfExpr),
}
