use internment::Intern;

use crate::span::Span;

use super::{INITIAL_FILE, Lex, extract, tokens::Token};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Identifier(pub Intern<String>);

impl Identifier {
    pub fn new(s: impl Into<String>) -> Self {
        Self(Intern::new(s.into()))
    }
}

impl Lex for Identifier {
    fn lex(input: &str) -> Result<(super::tokens::Token, &str), crate::span::Span> {
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
