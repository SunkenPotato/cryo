use math_expr::{MathExpr, MathExprParseError};

use crate::group;

use super::{
    Parse, ParseResultInto,
    literal::{Literal, LiteralParseError},
};

pub mod math_expr;

group! {
    #[derive(Debug, PartialEq)]
    pub enum ExprParseError {
        LiteralParseError(LiteralParseError),
        MathExprParseError(Box<MathExprParseError>)
    }
}

group! {
    #[derive(Debug, PartialEq)]
    pub enum Expr {
        Literal(Literal),
        MathExpr(Box<MathExpr>)
    }
}

impl Parse for Expr {
    type Error = ExprParseError;

    fn parse(input: &str) -> Result<(Self, &str), Self::Error> {
        // try parsing mathexpression
        // if fails:
        // try parsing literal

        // mathexpression:
        // try parsing
        //  lit op mathexpr
        //
        // if fails try
        //  go to start
        MathExpr::parse(input)
            .map(|(r, s)| (Expr::MathExpr(Box::new(r)), s))
            .or_else(|_| Literal::parse(input).into2())
    }
}
