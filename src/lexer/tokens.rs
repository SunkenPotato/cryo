use crate::span::Span;

use super::{
    Lex,
    identifier::Identifier,
    keyword::Keyword,
    literal::Literal,
    operation::Operation,
    single::{Assign, Semicolon},
};

pub type LexFunction = fn(&str) -> Result<(Token, &str), Span>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    Keyword(Keyword),
    Identifier(Identifier),
    Literal(Literal),
    Assign(Assign),
    Operation(Operation),
    Semicolon(Semicolon),
}

impl TokenType {
    pub const LEX_FUNCTIONS: &'static [LexFunction] = &[
        Keyword::lex,
        Identifier::lex,
        Literal::lex,
        Assign::lex,
        Operation::lex,
        Semicolon::lex,
    ];
}

impl Lex for TokenType {
    fn lex(input: &str) -> Result<(Token, &str), Span> {
        let mut err = None;

        for f in Self::LEX_FUNCTIONS {
            match f(input) {
                Err(e) => {
                    err = Some(e);
                }
                Ok(v) => return Ok(v),
            }
        }

        Err(err.unwrap_or(Span::EMPTY))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub token: TokenType,
    pub span: Span,
}

impl Token {
    #[must_use]
    pub const fn new(token: TokenType, span: Span) -> Self {
        Self { token, span }
    }
}
