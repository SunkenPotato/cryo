use crate::{Parse, ParseResult, S, parse_error};

pub enum Literal {
    StringLiteral(StringLiteral),
    IntegerLiteral(IntegerLiteral),
}

parse_error! {
    #(group)
    pub enum LiteralError {
        StringLitError(StringLitError),
    }
}

impl Parse for Literal {
    fn parse(tokens: &mut cryo_lexer::stream::TokenStreamGuard) -> ParseResult<Self> {
        tokens
            .with(IntegerLiteral::parse)
            .map(|v| v.map(Self::IntegerLiteral))
            .or_else(|_| {
                tokens
                    .with(StringLiteral::parse)
                    .map(|v| v.map(Self::StringLiteral))
            })
    }
}

pub struct StringLiteral(pub String);

parse_error! {
    #(concrete, "string literal parse error", 0)
    pub enum StringLitError {
        #(0, "error",)
        Error,
    }
}

impl Parse for StringLiteral {
    fn parse(tokens: &mut cryo_lexer::stream::TokenStreamGuard) -> ParseResult<Self> {
        todo!()
    }
}

pub struct IntegerLiteral(pub i32);

parse_error! {
    #(concrete, "integer literal parse error", 0)
    pub enum IntegerLitError {
        #(0, "todo",)
        A,
    }
}

impl Parse for IntegerLiteral {
    fn parse(tokens: &mut cryo_lexer::stream::TokenStreamGuard) -> ParseResult<Self> {
        todo!()
    }
}
