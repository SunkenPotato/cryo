//! This crate provides the lexer and tokens for the cryo language.
//!
//! In particular, this crate works primarily with the [`Lexer`] and [`Token`] structs to parse inputs into a computer-understandable form.
//!
//! Core components: [`Lexer`], [`Token`].

#![deny(missing_docs)]

use cryo_span::{INITIAL_FILE, LexResultExt, SourceFile, SourceFileError, SourceMap, Span};
use thiserror::Error;
use tokens::Token;

use crate::tokens::TokenType;

pub mod identifier;
pub mod keyword;
pub mod literal;
pub mod operation;
pub mod single;
pub mod tokens;

static WHITESPACE: &[char] = &['\u{20}', '\u{0A}', '\u{0D}'];

/// Parse a string slice into a [`Token`].
///
/// This trait is by no means necessary yet, however, to avoid breaking changes in libraries that depend on a such trait and cohesiveness, this trait defines
/// the method a certain `Token` should be parsed.
pub trait Lex: Sized {
    /// Parse a string into a token. This function *should* consume the string it has read and lexed, and return a string slice that does not contain the aforementioned.
    ///
    /// Assumes whitespace has already been stripped.
    /// # Errors
    /// The `Span` returned is the erroneous section in `input`.
    fn lex(input: &str) -> Result<(Token, &str), Span>;
}

/// "tag", or remove a prefix from a string slice.
///
/// # Examples
/// ```rust
/// let pat: &str = "H";
/// let input: &str = "Hello, world";
/// assert_eq!(cryo_lexer::tag(pat, input), Some("ello, world"))
/// ```
#[must_use]
pub fn tag<'s>(pat: &str, s: &'s str) -> Option<&'s str> {
    s.strip_prefix(pat)
}

/// Extract a slice from another string slice while a condition does not match.
///
/// # Examples
/// ```rust
/// use cryo_lexer::extract;
///
/// let input = "Hello, world";
/// let (extracted, rest) = extract(input, |c| c.is_ascii_whitespace());
/// assert_eq!(extracted, "Hello,");
/// assert_eq!(rest, " world");
/// ```
pub fn extract(s: &str, f: fn(char) -> bool) -> (&str, &str) {
    let end = s
        .char_indices()
        .find_map(|(idx, c)| if f(c) { Some(idx) } else { None })
        .unwrap_or(s.len());

    (&s[..end], &s[end..])
}

/// Convenience method for extracting the whitespace from a string.
///
/// View [`extract`] for more details.
#[must_use]
pub fn extract_whitespace(s: &str) -> &str {
    extract(s, |b| !WHITESPACE.contains(&b)).1
}

/// Represents the lexer mode.
///
/// The lexer can either read a direct `String` or the contents of a `File`.
pub enum LexerMode {
    /// Read directly from a [`String`]. The file used will be called "<unnamed>"
    DirectInput(String),
    /// Read from a file. The inner string is the path where this file lies.
    File(String),
}

/// The lexer struct.
///
/// This struct is responsible for turning a string input into a stream of tokens, and can be created using the [`Lexer::new`] method.
///
/// View [`Lexer::lex`] for more information on the lexing process.
pub struct Lexer {
    mode: LexerMode,
}

/// The different errors a [`Lexer`] can encounter while trying to lex the provided input.
#[derive(Error, Debug)]
pub enum LexicalError {
    /// A problem occurred while trying to read the input.
    SourceFileError(#[from] SourceFileError),
    /// The provided input is invalid and therefore cannot be lexed. The [`Span`] points to the problematic code.
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
    /// Create a new [`Lexer`] with the given [`LexerMode`]
    #[must_use]
    pub const fn new(mode: LexerMode) -> Self {
        Self { mode }
    }

    /// The whole point.
    ///
    /// This method will try to parse the input it was given when it was constructed.
    pub fn lex(self) -> Result<Vec<Token>, LexicalError> {
        match self.mode {
            LexerMode::DirectInput(v) => SourceMap::push(SourceFile::from_string(v, None)?),
            LexerMode::File(path) => SourceMap::push(SourceFile::new(path)?),
        }

        let source_map = SourceMap::instance();

        let mut tokens = vec![];
        #[allow(clippy::missing_panics_doc)]
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
            offset += input.len() - rest.len();
            dbg!(rest);
            input = rest;
            tokens.push(token);
        }

        Ok(tokens)
    }
}

#[macro_export]
#[allow(missing_docs)]
macro_rules! t {
    (id $id:literal) => {{
        use $crate::{
            identifier::Identifier,
            tokens::{Token, TokenType},
        };

        use cryo_span::Span;

        Token::new(TokenType::Identifier(Identifier::new($id)), Span::EMPTY)
    }};

    (keyword $kw:literal) => {{
        use $crate::{
            keyword::Keyword,
            tokens::{Token, TokenType},
        };

        use cryo_span::Span;

        Token::new(
            TokenType::Keyword(Keyword::from_str($kw).unwrap()),
            Span::EMPTY,
        )
    }};

    (nl $nl:literal) => {{
        use $crate::{
            literal::{Literal, NumberLiteral},
            tokens::{Token, TokenType},
        };

        use cryo_span::Span;

        Token::new(
            TokenType::Literal(Literal::NumberLiteral(NumberLiteral(
                stringify!($nl).to_owned(),
            ))),
            Span::EMPTY,
        )
    }};

    (sl $sl:literal) => {{
        use $crate::{
            literal::{Literal, NumberLiteral},
            tokens::{Token, TokenType},
        };

        use cryo_span::Span;

        Token::new(
            TokenType::Literal(Literal::StringLiteral(StringLiteral($nl.to_owned()))),
            Span::EMPTY,
        )
    }};

    (op $op:tt) => {{
        use std::str::FromStr;
        use $crate::{
            operation::Operation,
            tokens::{Token, TokenType},
        };

        use cryo_span::Span;

        Token::new(
            TokenType::Operation(Operation::from_str(stringify!($op)).unwrap()),
            Span::EMPTY,
        )
    }};

    (=) => {{
        use $crate::{
            single::Assign,
            tokens::{Token, TokenType},
        };

        use cryo_span::Span;

        Token::new(TokenType::Assign(Assign), Span::EMPTY)
    }};

    (;) => {{
        use $crate::{
            single::Semicolon,
            token::{Token, TokenType},
        };

        use cryo_span::Span;

        Token::new(TokenType::Semicolon(Semicolon), Span::EMPTY)
    }};
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
