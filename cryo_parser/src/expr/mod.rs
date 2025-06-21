//! Expressions.
//!
//! Expressions are defined as statements of code that return a value, be it `null`, `()`, or any other.
//!
//! For different types of expressions, view the [`Expr`] enum.

use binding_ref::BindingRef;
use math_expr::Operator;

use crate::{
    Parse, Spanned,
    error::ParseError,
    expr::{literal::Literal, math_expr::MathExpr},
    stream::TokenStreamGuard,
};

pub mod binding_ref;
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
    fn parse<'t>(stream: &mut TokenStreamGuard<'t>) -> Result<Spanned<Self>, Box<dyn ParseError>> {
        let possible_op_token = stream.peek_n(1);

        if let Ok(v) = possible_op_token {
            if v.is::<Operator>() {
                let me = MathExpr::parse(stream)?;
                let me = Spanned::new(Box::new(me.t), me.span);

                return Ok(me.map(Self::MathExpr));
            }
        }
        return Ok(ReducedExpr::parse(stream)?.map(Self::ReducedExpr));
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
    /// A reference to a binding.
    BindingRef(BindingRef),
}

impl Parse for ReducedExpr {
    fn parse<'t>(
        stream: &mut crate::TokenStreamGuard<'t>,
    ) -> Result<Spanned<Self>, Box<dyn ParseError>> {
        Literal::parse(stream)
            .map(|v| v.map(ReducedExpr::Literal))
            .or_else(|_| BindingRef::parse(dbg!(stream)).map(|v| v.map(Self::BindingRef)))
    }
}

#[cfg(test)]
mod tests {
    use cryo_lexer::{identifier::Identifier, t};

    use crate::{parse_assert, stream::TokenStream};

    use super::{
        ReducedExpr,
        binding_ref::BindingRef,
        literal::{Literal, NumberLiteral},
    };

    #[test]
    fn parse_lit_reduced_expr() {
        let mut ts = TokenStream::new([t![nl 5]]);

        parse_assert(
            &mut ts,
            ReducedExpr::Literal(Literal::NumberLiteral(NumberLiteral(5))),
        );
    }

    #[test]
    fn parse_bind_ref_reduced_expr() {
        let mut ts = TokenStream::new([t![id "x"]]);

        parse_assert(
            &mut ts,
            ReducedExpr::BindingRef(BindingRef(Identifier::new("x"))),
        );
    }
}
