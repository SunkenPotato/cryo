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

use crate::{
    expr::{
        binary_expr::BinaryExpr, block::Block, cond_expr::IfExpr, literal::Literal,
        struct_expr::StructExpr,
    },
    ident::Ident,
    parser::Parse,
};

/// An expression.
// derive(Parse) cannot be applied to this to prevent double ReducedExpr parsing
#[derive(Debug, PartialEq)]
pub enum Expr {
    /// A base expression.
    BaseExpr(BaseExpr),
    /// A binary expression.
    BinaryExpr(BinaryExpr),
}

impl Parse for Expr {
    type Output = Self;
    #[expect(unused)]
    fn parse(
        tokens: &mut cryo_lexer::stream::TokenStreamGuard,
    ) -> crate::parser::ParseResult<Self::Output> {
        todo!()
    }
}

/// Expressions excluding `MathExpr`.
#[derive(Parse, Debug, PartialEq)]
pub enum BaseExpr {
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
}
