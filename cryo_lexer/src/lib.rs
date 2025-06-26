pub mod atoms;
pub mod identifier;
pub mod literal;

use std::fmt::Display;

use cryo_span::{Span, Spanned};

use crate::{
    atoms::{Assign, Keyword, Operator, Semi},
    identifier::Identifier,
    literal::Literal,
};

pub type Token<'source> = Spanned<TokenType<'source>>;
type LexFn = fn(&str) -> Result<(Token, &str), Error>;
type Error = Spanned<LexicalError>;

#[macro_export]
#[doc(hidden)]
macro_rules! atom {
    (
        $identifier:ident, $atom:expr
    ) => {
        impl $crate::Lex for $identifier {
            fn lex(s: &str) -> Result<($crate::Token, &str), $crate::Error> {
                let rest = s
                    .strip_prefix($atom)
                    .ok_or($crate::Error::new($crate::LexicalError::SequenceNotFound($atom), cryo_span::Span::new(0, $atom.len())))?;
                Ok(($crate::Token::new($crate::TokenType::$identifier(Self), cryo_span::Span::new(0, $atom.len())), rest))
            }
        }

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
        }
    ) => {
        $(#[$attr])*
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        $visibility enum $identifier {
            $(
                $($variant_attr)*
                $variant,
            )*
        }

        impl $identifier {
            $visibility const VARIANTS: &[Self] = &[$(
                Self::$variant,
            )*];

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
                        return Ok(($crate::Token::new($crate::TokenType::$identifier(*variant), cryo_span::Span::new(0, atom.len())), v))
                    }
                }

                let whitespace_pos = s.find(|c: char| c.is_whitespace()).unwrap_or(s.len());
                Err($crate::Error::new($crate::LexicalError::SequenceNotFound(stringify!($identifier)), cryo_span::Span::new(0, whitespace_pos)))
            }
        }

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
                                    $crate::TokenType::$identifier(
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
    }
}

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType<'source> {
    Keyword(Keyword),
    Identifier(Identifier<'source>),
    Literal(Literal<'source>),
    Operator(Operator),
    Assign(Assign),
    Semi(Semi),
}

impl TokenType<'_> {
    // the order of these is important
    const LEX_FUNCTIONS: &'static [LexFn] = &[
        Keyword::lex,
        Identifier::lex,
        Literal::lex,
        Operator::lex,
        Assign::lex,
        Semi::lex,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexicalError {
    SequenceNotFound(&'static str),
    InvalidSequence,
    EndOfInput,
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

pub fn lexer(input: &str) -> Result<Vec<Token>, Error> {
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

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use cryo_span::Span;

    use crate::{
        Token, TokenType,
        atoms::{Assign, Keyword, Operator, Semi},
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
            Token::new(TokenType::Operator(Operator::Add), Span::new(15, 16)),
            Token::new(
                TokenType::Literal(Literal::StringLiteral(StringLiteral("hello"))),
                Span::new(17, 24),
            ),
            Token::new(TokenType::Semi(Semi), Span::new(24, 25)),
        ];

        assert_eq!(lexer(input).unwrap(), expected)
    }
}
