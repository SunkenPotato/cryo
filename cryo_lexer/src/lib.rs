use cryo_span::{LexResultExt, SourceFile, SourceFileError, SourceMap, Span};
use thiserror::Error;
use tokens::Token;

use crate::tokens::TokenType;

pub mod identifier;
pub mod keyword;
pub mod literal;
pub mod operation;
pub mod single;
pub mod tokens;

/// The file index to use with spans, since there multiple files are not supported yet.
pub const INITIAL_FILE: usize = 0;

pub trait Lex: Sized {
    /// Parse a string into a token.
    ///
    /// Assumes whitespace has already been stripped.
    /// # Errors
    /// The `Span` returned is the erroneous section.
    fn lex(input: &str) -> Result<(Token, &str), Span>;
}

#[must_use]
pub fn tag<'s>(pat: &str, s: &'s str) -> Option<&'s str> {
    s.strip_prefix(pat)
}

pub fn extract(s: &str, f: fn(char) -> bool) -> (&str, &str) {
    let end = s
        .char_indices()
        .find_map(|(idx, c)| if f(c) { Some(idx) } else { None })
        .unwrap_or(s.len());

    (&s[..end], &s[end..])
}

#[must_use]
pub fn extract_whitespace(s: &str) -> &str {
    extract(s, |b| !b.is_ascii_whitespace()).1
}

/// Represents the lexer mode.
///
/// The lexer can either read a direct `String` or the contents of a `File`.
pub enum LexerMode {
    DirectInput(String),
    File(String),
}

pub struct Lexer {
    mode: LexerMode,
}

#[derive(Error, Debug)]
pub enum LexicalError {
    SourceFileError(#[from] SourceFileError),
    ProblematicSpan(#[from] Span),
}

impl std::fmt::Display for LexicalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SourceFileError(v) => {
                write!(f, "error: Could not read file: {v}")?;
            }

            Self::ProblematicSpan(span) => {
                writeln!(f, "error: failed to parse input")?;
                write!(f, "{span}")?;
            }
        }

        Ok(())
    }
}

impl Lexer {
    #[must_use]
    pub const fn new(mode: LexerMode) -> Self {
        Self { mode }
    }

    #[allow(clippy::missing_panics_doc)]
    pub fn lex(self) -> Result<Vec<Token>, LexicalError> {
        match self.mode {
            LexerMode::DirectInput(v) => SourceMap::push(SourceFile::from_string(v)?),
            LexerMode::File(path) => SourceMap::push(SourceFile::new(path)?),
        }

        let source_map = SourceMap::instance();

        let mut tokens = vec![];
        let mut input = source_map.get(0).unwrap().contents().trim();
        let mut offset = 0;

        while !input.is_empty() {
            let unmapped = TokenType::lex(input);
            let mut token_len = 0;

            #[allow(unused_must_use)]
            unmapped.as_ref().inspect(|(_, s)| {
                token_len = input.len() - s.len();
            });

            let (token, rest) = unmapped.map_span(offset)?;
            let rest = extract_whitespace(rest);
            offset += input.len() - rest.len(); // advance offset
            input = rest;
            tokens.push(token);
        }

        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use cryo_span::Span;

    use crate::{
        INITIAL_FILE, Lexer,
        identifier::Identifier,
        keyword::Keyword,
        literal::{Literal, NumberLiteral},
        single::{Assign, Semicolon},
        tokens::{Token, TokenType},
    };

    #[test]
    fn lex() {
        let x = "let x = 5;".to_owned();
        let result = match Lexer::new(super::LexerMode::DirectInput(x)).lex() {
            Ok(v) => v,
            Err(e) => {
                panic!("{e}");
            }
        };

        let expected_tokens = &[
            Token::new(
                TokenType::Keyword(Keyword::Let),
                Span::new(INITIAL_FILE, 0, 3),
            ),
            Token::new(
                TokenType::Identifier(Identifier::new("x")),
                Span::new(INITIAL_FILE, 4, 5),
            ),
            Token::new(TokenType::Assign(Assign), Span::new(INITIAL_FILE, 6, 7)),
            Token::new(
                TokenType::Literal(Literal::NumberLiteral(NumberLiteral("5".into()))),
                Span::new(INITIAL_FILE, 8, 9),
            ),
            Token::new(
                TokenType::Semicolon(Semicolon),
                Span::new(INITIAL_FILE, 9, 10),
            ),
        ];

        assert_eq!(&result, expected_tokens);
    }
}
