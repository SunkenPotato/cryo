//! The lexer implementation for the `cryo` language.
//!
//! A lexer will take an input and split the input into [`Token`]s comprehensible by the parser without modifying the inputs.

#![feature(array_try_from_fn)]

pub mod atoms;
pub mod identifier;
pub mod literal;
pub mod stream;

use std::fmt::Display;

use cryo_diagnostic::SourceFile;
use cryo_span::{HasSpan, Span, Spanned};
use internment::Intern;

use crate::{
    atoms::{
        Bang, Colon, Comma, Dot, Equal, LCurly, LParen, Minus, Percent, Plus, RCurly, RParen, Semi,
        Slash, Star,
    },
    identifier::Identifier,
    literal::Literal,
    stream::TokenStream,
};

/// A token.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Token {
    /// The token kind.
    pub kind: TokenKind,
    /// The lexeme, or the string associated with it.
    pub lexeme: Symbol,
    /// The span.
    pub span: Span,
}

impl Token {
    /// Create a new token.
    pub const fn new(kind: TokenKind, lexeme: Symbol, span: Span) -> Self {
        Self { kind, lexeme, span }
    }

    /// Require this to be of the given [`TokenKind`].
    pub fn require(&self, kind: TokenKind) -> Option<Symbol> {
        if kind == self.kind {
            return Some(self.lexeme);
        }

        None
    }
}

impl HasSpan for Token {
    fn span(&mut self) -> &mut Span {
        &mut self.span
    }
}

type LexFn = fn(&str) -> Result<(Token, &str), LexicalError>;

/// An error returned by the lexer.
pub type LexicalError = Spanned<LexicalErrorKind>;

/// A symbol.
///
/// A symbol represents an interned slice of input.
pub type Symbol = Intern<str>;

/// Split an input string while the supplied function returns `false`.
pub fn extract(s: &str, f: impl Fn(char) -> bool) -> (&str, &str) {
    let end = s
        .char_indices()
        .find_map(|(idx, c)| if f(c) { Some(idx) } else { None })
        .unwrap_or(s.len());

    (&s[..end], &s[end..])
}

pub(crate) fn find_token_end(s: &str) -> (&str, &str) {
    if let Some(stripped) = s.strip_prefix('"') {
        for (c0, (c1_idx, c1)) in stripped.chars().zip(stripped.char_indices().skip(1)) {
            if c1 == '"' && c0 != '\\' {
                return (&s[..c1_idx + 2], &s[c1_idx + 2..]);
            }
        }

        return (s, "");
    }

    let idx = s.find(|c: char| c.is_whitespace()).unwrap_or(s.len());

    (&s[..idx], &s[idx..])
}

trait Lex: Sized {
    fn lex(s: &str) -> Result<(Token, &str), LexicalError>;
}

/// A token kind.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[expect(missing_docs)]
pub enum TokenKind {
    Identifier,
    IntegerLiteral,
    StringLiteral,
    Semi,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equal,
    Bang,
    RCurly,
    LCurly,
    RParen,
    LParen,
    Comma,
    Colon,
    Dot,
}

trait Sealed {}

/// Trait for attempting to convert `Token`s into concrete types.
#[expect(private_bounds)]
pub trait TokenLike: Sealed + Sized {
    /// Attempt to convert a given token into `Self`.
    fn from_token(token: &Token) -> Option<Spanned<Self>>;
}

impl TokenKind {
    // the order of these is important
    const LEX_FUNCTIONS: &[LexFn] = &[
        Identifier::lex,
        Literal::lex,
        Plus::lex,
        Minus::lex,
        Star::lex,
        Slash::lex,
        Percent::lex,
        Bang::lex,
        Equal::lex,
        Semi::lex,
        RCurly::lex,
        LCurly::lex,
        Comma::lex,
        LParen::lex,
        RParen::lex,
        Colon::lex,
        Dot::lex,
    ];
}

impl Lex for TokenKind {
    fn lex(s: &str) -> Result<(Token, &str), LexicalError> {
        for f in Self::LEX_FUNCTIONS {
            if let Ok(v) = f(s) {
                return Ok(v);
            }
        }
        Err(LexicalError::new(
            LexicalErrorKind::NoMatch,
            Span::new(0, s.len() as u32),
        ))
    }
}

/// Errors that may occur during lexing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexicalErrorKind {
    /// An expected sequence was not found.
    SequenceNotFound(&'static str),
    /// An invalid sequence was not found.
    InvalidSequence,
    /// An unexpected end of input was reached.
    EndOfInput,
    /// The lexer could not convert the supplied input into any tokens.
    NoMatch,
    /// No progress was made by the lexer.
    NoProgress,
}

impl Display for LexicalErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let e_str = match self {
            Self::SequenceNotFound(s) => format!("sequence not found: '{s}'"),
            Self::EndOfInput => "unexpected end of input".to_owned(),
            Self::NoMatch => "could not parse the given stream".to_owned(),
            Self::InvalidSequence => "invalid sequence".to_owned(),
            Self::NoProgress => "the lexer could not make any progress".to_owned(),
        };
        write!(f, "lexical error: {e_str}")?;

        Ok(())
    }
}

/// An error returned by the lexer. This differs from [`LexicalError`], as it may also be an I/O error.
#[derive(Debug)]
pub enum LexerError {
    /// A lexical error.
    LexicalError(LexicalError),
    /// An I/O error.
    Io(std::io::Error),
}

/// The lexer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lexer {
    /// The source from which it should pull contents from.
    pub source: SourceFile,
}

impl Lexer {
    /// Create a new lexer with the given source.
    pub const fn new(source: SourceFile) -> Self {
        Self { source }
    }

    /// Consume the content supplied by the inner [`SourceFile`] and attempt to create a [`TokenStream`].
    pub fn lex(self) -> Result<(SourceFile, TokenStream), LexerError> {
        let source = match self.source.clone().contents() {
            Ok(v) => v,
            Err(e) => return Err(LexerError::Io(e)),
        };

        let stream = match lexer(&source) {
            Ok(v) => v,
            Err(e) => return Err(LexerError::LexicalError(e)),
        };

        Ok((self.source, stream))
    }
}

/// The whole point.
///
/// The function will attempt to convert the supplied input into tokens and return a [`TokenStream`] which can be used to inspect the tokens generated. \
/// If it fails, the function will return a spanned [`LexicalError`] pointing to where the erroneous input is.
fn lexer(input: &str) -> Result<TokenStream, LexicalError> {
    let mut tokens = vec![];
    let mut loop_input = input.trim();
    let mut cursor = input.len() - loop_input.len();

    while !loop_input.is_empty() {
        let (mut token, rest) = TokenKind::lex(loop_input)?;

        if loop_input.len() == rest.len() {
            return Err(LexicalError::zero(LexicalErrorKind::NoProgress));
        }

        let token_len = loop_input.len() - rest.len();
        token.offset(cursor as u32);

        tokens.push(token);
        cursor += token_len;

        let trimmed_rest = rest.trim_start();
        cursor += rest.len() - trimmed_rest.len();
        loop_input = trimmed_rest;
    }

    Ok(TokenStream::new(tokens))
}

impl TryInto<TokenStream> for &str {
    type Error = LexicalError;

    fn try_into(self) -> Result<TokenStream, Self::Error> {
        lexer(self)
    }
}

#[cfg(test)]
mod tests {
    use cryo_span::Span;

    use crate::{Token, TokenKind, lexer};

    #[test]
    fn lex() {
        let input = "let input = 20 + \"hello\";";

        let expected = [
            Token::new(TokenKind::Identifier, "let".into(), Span::new(0, 3)),
            Token::new(TokenKind::Identifier, "input".into(), Span::new(4, 9)),
            Token::new(TokenKind::Equal, "=".into(), Span::new(10, 11)),
            Token::new(TokenKind::IntegerLiteral, "20".into(), Span::new(12, 14)),
            Token::new(TokenKind::Plus, "+".into(), Span::new(15, 16)),
            Token::new(TokenKind::StringLiteral, "hello".into(), Span::new(17, 24)),
            Token::new(TokenKind::Semi, ";".into(), Span::new(24, 25)),
        ];

        assert_eq!(&*lexer(input).unwrap().inner, expected)
    }
}
