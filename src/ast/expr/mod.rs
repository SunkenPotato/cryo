use literal::Literal;
use math_expr::MathExpr;

use crate::lexer::tokens::Token;

use super::Parse;

pub mod literal;
pub mod math_expr;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    MathExpr(Box<MathExpr>),
}

impl From<ReducedExpr> for Expr {
    fn from(value: ReducedExpr) -> Self {
        match value {
            ReducedExpr::Literal(v) => Self::Literal(v),
        }
    }
}

impl Parse for Expr {
    fn parse(stream: &[Token]) -> Result<(Self, &[Token]), super::ParseError> {
        if let Some(v) = stream.get(1)
            && v.is(Token::OPERATION_TOKEN)
        {
            return MathExpr::parse(stream).map(|(r, t)| (Expr::MathExpr(Box::new(r)), t));
        }

        ReducedExpr::parse(stream).map(|(r, t)| (r.into(), t))
    }
}

pub enum ReducedExpr {
    Literal(Literal),
}

impl Parse for ReducedExpr {
    fn parse(stream: &[Token]) -> Result<(Self, &[Token]), super::ParseError> {
        Literal::parse(stream).map(|(l, t)| (ReducedExpr::Literal(l), t))
    }
}
