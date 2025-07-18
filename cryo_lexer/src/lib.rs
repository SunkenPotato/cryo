//! The lexer implementation for the `cryo` language.
//!
//! A lexer will take an input and split the input into [`Token`]s comprehensible by the parser without modifying the inputs.

#![feature(array_try_map)]

macro_rules! token_marker {
    (
        $type:ident $(<$lt:tt>)?
    ) => {
        impl<'source> $crate::Sealed for $type $(<$lt>)? {}

        impl<'source> $crate::FromToken<'source> for $type $(<$lt>)? {
            const NAME: &'static str = stringify!($type);
            fn from_token<'borrow>(
                token: &'borrow $crate::Token<'source>,
            ) -> Option<::cryo_span::Spanned<&'borrow Self>> {
                match token.t {
                    $crate::TokenType::$type(ref v) => Some(::cryo_span::Spanned::new(v, token.span)),
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

use std::fmt::Display;

use cryo_span::{Span, Spanned};

use crate::{
    atoms::{Assign, Colon, Comma, Dot, Keyword, LCurly, LParen, Operators, RCurly, RParen, Semi},
    identifier::Identifier,
    literal::Literal,
    stream::TokenStream,
};

/// A token. Contains a [`Span`] and a [`TokenType`].
pub type Token<'source> = Spanned<TokenType<'source>>;

/// Extension trait for `Spanned<TokenType>` (a.k.a., [`Token`]).
pub trait TokenExt<'source> {
    /// Attempt to reinterpret `self` as `T` using [`FromToken`].
    fn require<T: FromToken<'source>>(&self) -> Option<Spanned<&T>>;
    /// Checks whether `self` can be reinterpreted as `T`.
    fn is<T: FromToken<'source>>(&self) -> bool {
        self.require::<T>().is_some()
    }
}

impl<'source> TokenExt<'source> for Token<'source> {
    fn require<T: FromToken<'source>>(&self) -> Option<Spanned<&T>> {
        T::from_token(self)
    }
}

type LexFn = fn(&str) -> Result<(Token, &str), Error>;
type Error = Spanned<LexicalError>;

#[macro_export]
#[doc(hidden)]
macro_rules! atom {
    (
        $(#[$attr:meta])*
        $identifier:ident, $atom:expr
    ) => {
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        $(#[$attr])*
        pub struct $identifier;

        impl $crate::Lex for $identifier {
            fn lex(s: &str) -> Result<($crate::Token, &str), $crate::Error> {
                let rest = s
                    .strip_prefix($atom)
                    .ok_or($crate::Error::new($crate::LexicalError::SequenceNotFound($atom), cryo_span::Span::new(0, $atom.len())))?;
                Ok(($crate::Token::new($crate::TokenType::$identifier(Self), cryo_span::Span::new(0, $atom.len())), rest))
            }
        }

        token_marker!($identifier);

        #[cfg(test)]
        ::paste::paste! {
            #[test]
            #[allow(non_snake_case)]
            fn [<lex_ $identifier>]() {
                use $crate::Lex;

                assert_eq!(
                    $identifier::lex($atom),
                    Ok((
                        $crate::Token::new(
                            $crate::TokenType::$identifier($identifier),
                            cryo_span::Span::new(0, $atom.len())
                        ),
                        ""
                    ))
                )
            }
        }
    };


    (
        $(#[$attr:meta])*
        $visibility:vis enum $identifier:ident {
            $(
                $(#[$variant_attr:meta])*
                #($atom:expr)
                $variant:ident
            ),*
        } with $constructor:path
    ) => {
        $(#[$attr])*
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        $visibility enum $identifier {
            $(
                $(#[$variant_attr])*
                $variant,
            )*
        }

        impl $identifier {
            #[doc = concat!("The variants ", stringify!($identifier), " may have")]
            $visibility const VARIANTS: &[Self] = &[$(
                Self::$variant,
            )*];

            #[doc = concat!("Return the string this variant would be able to be parsed from")]
            $visibility const fn as_str(&self) -> &str {
                match self {
                    $(
                        Self::$variant => $atom,
                    )*
                }
            }
        }

        impl $crate::Lex for $identifier {
            fn lex(s: &str) -> Result<($crate::Token, &str), $crate::Error> {
                for variant in Self::VARIANTS {
                    let atom = variant.as_str();
                    if let Some(v) = s.strip_prefix(variant.as_str()) {
                        return Ok(($crate::Token::new($constructor(*variant), cryo_span::Span::new(0, atom.len())), v))
                    }
                }

                let whitespace_pos = s.find(|c: char| c.is_whitespace()).unwrap_or(s.len());
                Err($crate::Error::new($crate::LexicalError::SequenceNotFound(stringify!($identifier)), cryo_span::Span::new(0, whitespace_pos)))
            }
        }

        token_marker!($identifier);

        #[cfg(test)]
        ::paste::paste! {
            #[allow(non_snake_case)]
            mod [<$identifier _tests>] {
                use $crate::Lex;
                $(
                    #[test]
                    #[allow(non_snake_case)]
                    fn [<lex_ $identifier _ $variant>]() {
                        assert_eq!(
                            super::$identifier::lex(super::$identifier::$variant.as_str()),
                            Ok((
                                $crate::Token::new(
                                    $constructor(
                                        super::$identifier::$variant
                                    ),
                                    cryo_span::Span::new(0, super::$identifier::$variant.as_str().len())
                                ),
                                ""
                            ))
                        );
                    }
                )*
            }
        }
    };

    (
        $(#[$attr:meta])*
        $visibility:vis enum $identifier:ident {
            $(
                $(#[$variant_attr:meta])*
                #($atom:expr)
                $variant:ident
            ),*
        }
    ) => {
        $crate::atom!(
            $(#[$attr])*
            $visibility enum $identifier {
                $(
                    $(#[$variant_attr])*
                    #($atom)
                    $variant
                ),*
            } with $crate::TokenType::$identifier
        );
    };
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType<'source> {
    /// A keyword.
    Keyword(Keyword),
    /// An identifier.
    Identifier(Identifier<'source>),
    /// A literal.
    Literal(Literal<'source>),
    /// An arithmetic operator.
    Operators(Operators),
    /// An assign token (`=`).
    Assign(Assign),
    /// A semicolon (`;`).
    Semi(Semi),
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
pub trait FromToken<'source>: Sealed {
    /// The name of the token.
    const NAME: &'static str;
    /// Attempt to convert a given token into `Self`.
    fn from_token<'borrow>(token: &'borrow Token<'source>) -> Option<Spanned<&'borrow Self>>;
}

impl<'s> TokenType<'s> {
    // the order of these is important
    const LEX_FUNCTIONS: &'static [LexFn] = &[
        Keyword::lex,
        Identifier::lex,
        Literal::lex,
        Operators::lex,
        Assign::lex,
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

impl Lex for TokenType<'_> {
    fn lex(s: &str) -> Result<(Token, &str), Error> {
        for f in Self::LEX_FUNCTIONS {
            if let Ok(v) = f(s) {
                return Ok(v);
            }
        }
        Err(Error::new(LexicalError::NoMatch, Span::new(0, s.len())))
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
        token = token.offset(cursor);

        tokens.push(token);
        cursor += token_len;

        let trimmed_rest = rest.trim_start();
        cursor += rest.len() - trimmed_rest.len();
        loop_input = trimmed_rest;
    }

    Ok(TokenStream::new(tokens))
}

#[cfg(test)]
mod tests {
    use cryo_span::Span;

    use crate::{
        Token, TokenType,
        atoms::{Assign, Keyword, Operators, Semi},
        identifier::Identifier,
        lexer,
        literal::{IntegerLiteral, Literal, StringLiteral},
    };

    #[test]
    fn lex() {
        let input = "let input = 20 + \"hello\";";

        let expected = [
            Token::new(TokenType::Keyword(Keyword::Let), Span::new(0, 3)),
            Token::new(TokenType::Identifier(Identifier("input")), Span::new(4, 9)),
            Token::new(TokenType::Assign(Assign), Span::new(10, 11)),
            Token::new(
                TokenType::Literal(Literal::IntegerLiteral(IntegerLiteral("20"))),
                Span::new(12, 14),
            ),
            Token::new(TokenType::Operators(Operators::Add), Span::new(15, 16)),
            Token::new(
                TokenType::Literal(Literal::StringLiteral(StringLiteral("hello"))),
                Span::new(17, 24),
            ),
            Token::new(TokenType::Semi(Semi), Span::new(24, 25)),
        ];

        assert_eq!(lexer(input).unwrap().inner, expected)
    }
}
