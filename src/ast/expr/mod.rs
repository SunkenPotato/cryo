pub mod binding_ref;
pub mod math_expr;

use binding_ref::BindingRef;
use math_expr::{MathExpr, MathExprParseError};

use crate::group;

use super::{
    IdentifierParseError, Parse, ParseResultInto,
    literal::{Literal, LiteralParseError},
};

group! {
    #[derive(Debug, PartialEq)]
    pub enum ExprParseError {
        LiteralParseError(LiteralParseError),
        MathExprParseError(Box<MathExprParseError>),
        IdentifierParseError(IdentifierParseError)
    }
}

group! {
    #[derive(Debug, PartialEq)]
    pub enum Expr {
        Literal(Literal),
        MathExpr(Box<MathExpr>),
        BindingRef(BindingRef)
    }
}

impl Parse for Expr {
    type Error = ExprParseError;

    fn parse(input: &str) -> Result<(Self, &str), Self::Error> {
        MathExpr::parse(input)
            .map(|(r, s)| (Expr::MathExpr(Box::new(r)), s))
            .or_else(|_| Literal::parse(input).into2())
            .or_else(|_: ExprParseError| BindingRef::parse(input).into2())
    }
}
