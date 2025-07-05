pub mod literal;
pub mod math_expr;
#[cfg(test)]
mod tests;

use cryo_parser_proc_macro::Parse;
use cryo_span::Spanned;

use crate::{
    expr::{
        literal::Literal,
        math_expr::{MathExpr, Operator},
    },
    parser::Parse,
};

// derive(Parse) cannot be applied to this to prevent double ReducedExpr parsing
#[derive(Debug, PartialEq)]
pub enum Expr {
    MathExpr(Box<MathExpr>),
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

#[derive(Parse, Debug, PartialEq)]
pub enum ReducedExpr {
    Literal(Literal),
}
