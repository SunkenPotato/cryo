//! Tokens created by the lexer.

use cryo_span::{Span, impl_get_span};

use super::{
    Lex,
    identifier::Identifier,
    keyword::Keyword,
    literal::Literal,
    operation::Operation,
    single::{Assign, Semicolon},
};

type LexFunction = fn(&str) -> Result<(Token, &str), Span>;

/// The different types a token can be.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    /// The [`Keyword`] token.
    Keyword(Keyword),
    /// The [`Identifier`] token.
    Identifier(Identifier),
    /// The [`Literal`] token.
    Literal(Literal),
    /// The [`Assign`] token.
    Assign(Assign),
    /// The arithmetic operator ([`Operation`]) token.
    Operation(Operation),
    /// The semicolon token.
    Semicolon(Semicolon),
}

impl TokenType {
    const LEX_FUNCTIONS: &'static [LexFunction] = &[
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

/// A token, as lexed by the lexer.
///
/// Contains the actual token ([`Token::token`]) and the [`Span`] where this token lies in the input.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    /// The actual token.
    pub token: TokenType,
    /// The span pointing to where this token originates in the input.
    pub span: Span,
}

impl_get_span!(Token, span);

impl Token {
    /// Create a new token with from a [`TokenType`] and a [`Span`].
    #[must_use]
    pub const fn new(token: TokenType, span: Span) -> Self {
        Self { token, span }
    }
}
