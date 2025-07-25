use cryo_lexer::stream::StreamLike;

use crate::{Parse, expr::literal::Literal};

pub mod literal;

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Lit(Literal),
}

impl Parse for Expr {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        tokens.with(Literal::parse).map(Self::Lit)
    }
}
