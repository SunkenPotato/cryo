//! The lexer implementation for the `cryo` language.
//!
//! A lexer will take an input and split the input into [`Token`]s comprehensible by the parser without modifying the inputs.

#![feature(array_try_from_fn)]

macro_rules! token_marker {
    (
        $type:ident
    ) => {
        impl $crate::Sealed for $type {}

        impl $crate::TokenLike for $type {
            const NAME: &'static str = stringify!($type);
            fn from_token(token: &$crate::Token) -> Option<::cryo_span::Spanned<&Self>> {
                match token.t {
                    $crate::TokenType::$type(ref v) => {
                        Some(::cryo_span::Spanned::new(v, token.span))
                    }
                    _ => None,
                }
            }
        }
    };
}

pub mod atoms;
pub mod identifier;
pub mod literal;
pub mod stream;

use std::{fmt::Display, ops::Deref};

use cryo_span::{Span, Spanned};
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

/// A token. Contains a [`Span`] and a [`TokenType`].
pub type Token = Spanned<TokenType>;

/// Extension trait for `Spanned<TokenType>` (a.k.a., [`Token`]).
pub trait TokenExt {
    /// Attempt to reinterpret `self` as `T` using [`FromToken`].
    fn require<T: TokenLike>(&self) -> Option<Spanned<&T>>;
    /// Checks whether `self` can be reinterpreted as `T`.
    fn is<T: TokenLike>(&self) -> bool {
        self.require::<T>().is_some()
    }
}

impl TokenExt for Token {
    fn require<T: TokenLike>(&self) -> Option<Spanned<&T>> {
        T::from_token(self)
    }
}

type LexFn = fn(&str) -> Result<(Token, &str), Error>;
type Error = Spanned<LexicalError>;

/// A symbol.
///
/// A symbol represents an interned slice of input.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Symbol(pub Intern<str>);

impl Deref for Symbol {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<&str> for Symbol {
    fn from(value: &str) -> Self {
        Self(Intern::from(value))
    }
}

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
    fn lex(s: &str) -> Result<(Token, &str), Error>;
}

/// The possible types a token may be. `'source` refers to the lifetime of the input given to the parser.
// TODO: overflow subtypes into this to avoid nesting
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    /// An identifier.
    Identifier(Identifier),

    /// A literal.
    Literal(Literal),

    /// A semicolon (`;`).
    Semi(Semi),

    /// A plus (`+`).
    Plus(Plus),

    /// A minus (`-`).
    Minus(Minus),

    /// A star (`*`).
    Star(Star),

    /// A slash (`/`).
    Slash(Slash),

    /// A percent sign (`%`).
    Percent(Percent),

    /// An equals sign (`=`).
    Equal(Equal),

    /// A bang (`!`).
    Bang(Bang),

    /// The right curly brace ('{').
    RCurly(RCurly),

    /// The left curly brace ('}')
    LCurly(LCurly),

    /// The left parenthesis (`(`).
    LParen(LParen),

    /// The right parenthesis (`)`).
    RParen(RParen),

    /// A comma.
    Comma(Comma),

    /// A colon.
    Colon(Colon),

    /// A dot.
    Dot(Dot),
}

trait Sealed {}

/// Trait for attempting to convert `Token`s into concrete types.
#[allow(private_bounds)]
pub trait TokenLike: Sealed {
    /// The name of the token.
    const NAME: &'static str;
    /// Attempt to convert a given token into `Self`.
    fn from_token(token: &Token) -> Option<Spanned<&Self>>;
}

impl TokenType {
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

impl Lex for TokenType {
    fn lex(s: &str) -> Result<(Token, &str), Error> {
        for f in Self::LEX_FUNCTIONS {
            if let Ok(v) = f(s) {
                return Ok(v);
            }
        }
        Err(Error::new(
            LexicalError::NoMatch,
            Span::new(0, s.len() as u32),
        ))
    }
}

/// Errors that may occur during lexing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexicalError {
    /// An expected sequence was not found.
    SequenceNotFound(&'static str),
    /// An invalid sequence was not found.
    InvalidSequence,
    /// An unexpected end of input was reached.
    EndOfInput,
    /// The lexer could not convert the supplied input into any tokens.
    NoMatch,
}

impl Display for LexicalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let e_str = match self {
            Self::SequenceNotFound(s) => format!("sequence not found: '{s}'"),
            Self::EndOfInput => "unexpected end of input".to_owned(),
            Self::NoMatch => "could not parse the given stream".to_owned(),
            Self::InvalidSequence => "invalid sequence".to_owned(),
        };
        write!(f, "lexical error: {e_str}")?;

        Ok(())
    }
}

/// The whole point.
///
/// The function will attempt to convert the supplied input into tokens and return a [`TokenStream`] which can be used to inspect the tokens generated. \
/// If it fails, the function will return a spanned [`LexicalError`] pointing to where the erroneous input is.
pub fn lexer(input: &str) -> Result<TokenStream, Error> {
    let mut tokens = vec![];
    let mut loop_input = input.trim();
    let mut cursor = input.len() - loop_input.len();

    while !loop_input.is_empty() {
        let (mut token, rest) = TokenType::lex(loop_input)?;

        let token_len = loop_input.len() - rest.len();
        token = token.offset(cursor as u32);

        tokens.push(token);
        cursor += token_len;

        let trimmed_rest = rest.trim_start();
        cursor += rest.len() - trimmed_rest.len();
        loop_input = trimmed_rest;
    }

    Ok(TokenStream::new(tokens))
}

impl TryInto<TokenStream> for &'_ str {
    type Error = Error;

    fn try_into(self) -> Result<TokenStream, Self::Error> {
        lexer(self)
    }
}

#[cfg(test)]
mod tests {
    use cryo_span::Span;

    use crate::{
        Token, TokenType,
        atoms::{Equal, Semi},
        identifier::Identifier,
        lexer,
        literal::{IntegerLiteral, Literal, StringLiteral},
    };

    #[test]
    fn lex() {
        let input = "let input = 20 + \"hello\";";

        let expected = [
            Token::new(
                TokenType::Identifier(Identifier("let".into())),
                Span::new(0, 3),
            ),
            Token::new(
                TokenType::Identifier(Identifier("input".into())),
                Span::new(4, 9),
            ),
            Token::new(TokenType::Equal(Equal), Span::new(10, 11)),
            Token::new(
                TokenType::Literal(Literal::IntegerLiteral(IntegerLiteral("20".into()))),
                Span::new(12, 14),
            ),
            Token::new(TokenType::Plus(crate::atoms::Plus), Span::new(15, 16)),
            Token::new(
                TokenType::Literal(Literal::StringLiteral(StringLiteral("hello".into()))),
                Span::new(17, 24),
            ),
            Token::new(TokenType::Semi(Semi), Span::new(24, 25)),
        ];

        assert_eq!(&*lexer(input).unwrap().inner, expected)
    }
}
