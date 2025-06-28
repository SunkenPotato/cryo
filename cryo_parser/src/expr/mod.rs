use crate::{Parse, ParseResult, expr::literal::Literal};

pub mod literal;

pub enum Expr {
    ReducedExpr(ReducedExpr),
}

pub enum ReducedExpr {
    Literal(Literal),
}

impl Parse for ReducedExpr {
    fn parse(tokens: &mut cryo_lexer::stream::TokenStreamGuard) -> ParseResult<Self> {
        tokens.with(Literal::parse).map(|v| v.map(Self::Literal))
    }
}
