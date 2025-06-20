//! The [`Identifier`] token.
//!
//! View [`Identifier`] for more info.

use cryo_span::Span;
use internment::Intern;

use super::{INITIAL_FILE, Lex, extract, tokens::Token};

/// Represents the identifier in code. An identifier must match the following regex to be considered valid:
/// ```compile_fail
/// [a-zA-Z_][0-9a-zA-Z_]*
/// ```
/// An identifier is not encapsulated in `""`.
///
/// [`Identifier`]s are stored as interned [`String`]s, making them `Copy` and cheap to compare and hash.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Identifier(pub Intern<String>);

impl Identifier {
    /// Construct a new [`Identifier`] from a string.
    pub fn new(s: impl Into<String>) -> Self {
        Self(Intern::new(s.into()))
    }
}

impl Lex for Identifier {
    fn lex(input: &str) -> Result<(Token, &str), Span> {
        let (id, rest) = extract(input, |c| !c.is_ascii_alphanumeric());

        if id.is_empty() {
            return Err(Span::EMPTY);
        }

        // is OK because we checked if the slice is empty.
        if id.chars().next().unwrap().is_ascii_digit() {
            return Err(Span::ONE);
        }

        let id = Intern::new(id.to_string());
        let span = Span::new(INITIAL_FILE, 0, id.len());
        let token = Token::new(super::tokens::TokenType::Identifier(Self(id)), span);

        Ok((token, rest))
    }
}
