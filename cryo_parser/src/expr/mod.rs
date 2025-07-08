//! Expressions.
//!
//! Expressions are components of programming languages that, when evaluated, return a value.

pub mod binary_expr;
pub mod block;
pub mod cond_expr;
pub mod fn_call;
pub mod literal;
pub mod struct_expr;

#[cfg(test)]
mod tests;

use cryo_parser_proc_macro::Parse;
use cryo_span::Spanned;

use crate::{
    expr::{
        binary_expr::{BinaryExpr, Operator},
        block::Block,
        cond_expr::IfExpr,
        fn_call::FnCall,
        literal::Literal,
        struct_expr::StructExpr,
    },
    ident::Ident,
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
    BinaryExpr(Box<BinaryExpr>),
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
                Self::BinaryExpr(Box::new(BinaryExpr {
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
    BindingRef(Ident),
    /// A block expression.
    Block(Block),
    /// A conditional expression.
    IfExpr(IfExpr),
    /// A struct constructor expression.
    StructExpr(StructExpr),
    /// A function call.
    FnCall(FnCall),
}
