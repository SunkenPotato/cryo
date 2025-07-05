use crate::{
    expr::{Expr, ReducedExpr},
    parser::Parse,
};
use cryo_lexer::atoms::Operator as OToken;
use cryo_parser_proc_macro::Parse;

#[derive(Parse, Debug, PartialEq)]
pub struct MathExpr {
    pub lhs: ReducedExpr,
    pub op: Operator,
    pub rhs: Expr,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
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
        }))
    }
}
