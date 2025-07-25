//! A literal.
//!
//! Literals are tokens which do not require any further computation and are direct, primitive values.

use cryo_span::{Span, Spanned};

use crate::{
    Error, Lex, LexicalError, Sealed, Symbol, Token, TokenLike, TokenType, extract, find_token_end,
};

/// A literal. View the module-level docs for more information.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Literal {
    /// A string literal.
    StringLiteral(StringLiteral),
    /// An integer literal.
    IntegerLiteral(IntegerLiteral),
    /// A boolean literal.
    BooleanLiteral(BooleanLiteral),
}

token_marker!(Literal);

impl Lex for Literal {
    fn lex(s: &str) -> Result<(crate::Token, &str), Error> {
        let first = s.chars().next();
        if let Some('"') = first {
            return StringLiteral::lex(s);
        }

        if let Some('0'..='9' | '-') = first {
            return IntegerLiteral::lex(s);
        }

        let (start, _) = find_token_end(s);
        Err(Error::new(
            LexicalError::NoMatch,
            Span::new(0, start.len() as u32),
        ))
    }
}

/// A string literal such as `"hello, world"`.
///
/// String literals support escapes such as `\"` and the null escape `\0`. They are however, unescaped by the parser.
///
/// String literals are delimited by the token `"`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StringLiteral(pub Symbol);

impl TokenLike for StringLiteral {
    const NAME: &'static str = "StringLiteral";
    fn from_token(token: &Token) -> Option<Spanned<&Self>> {
        let lit = Literal::from_token(token)?;
        match lit.t {
            Literal::StringLiteral(s) => Some(Spanned::new(s, lit.span)),
            _ => None,
        }
    }
}

impl Sealed for StringLiteral {}

impl Lex for StringLiteral {
    fn lex(s: &str) -> Result<(crate::Token, &str), crate::Error> {
        let (token, rest) = find_token_end(s);
        let span = Span::new(0, token.len() as u32);
        let unquoted = match token.chars().next() {
            Some('"') => &s[1..],
            Some(_) => return Err(Error::new(LexicalError::SequenceNotFound("\""), span)),
            None => return Err(Error::new(LexicalError::EndOfInput, Span::new(0, 0))),
        };

        let mut cursor = 0;
        let mut iter = unquoted.chars();
        let mut closed = false;

        while let Some(c) = iter.next() {
            match c {
                '"' => {
                    closed = true;
                    break;
                }
                '\\' => {
                    cursor += 1;
                    iter.next();
                }
                _ => (),
            }

            cursor += 1;
        }

        if !closed {
            return Err(Error::new(LexicalError::SequenceNotFound("\""), span));
        }

        Ok((
            Token::new(
                TokenType::Literal(Literal::StringLiteral(StringLiteral(
                    unquoted[..cursor].into(),
                ))),
                span,
            ),
            rest,
        ))
    }
}

/// An integer literal, i.e., a number, e.g. `1` or `-3`.
///
/// Integer literals support separators in the form of underscores (`_`) and negation signs in the form of a hyphen (`-`).
///
/// Integer literals may not begin or end with a separator and may not contain negation signs anywhere but in the front.
/// A variable number of both negation signs and separators are supported where they are valid.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct IntegerLiteral(pub Symbol);

impl TokenLike for IntegerLiteral {
    const NAME: &'static str = "IntegerLiteral";
    fn from_token(token: &Token) -> Option<Spanned<&Self>> {
        let lit = Literal::from_token(token)?;
        match lit.t {
            Literal::IntegerLiteral(s) => Some(Spanned::new(s, lit.span)),
            _ => None,
        }
    }
}

impl Sealed for IntegerLiteral {}

fn is_invalid_integer_char(c: char) -> bool {
    !matches!(c, '0'..='9' | '_')
}

impl Lex for IntegerLiteral {
    fn lex(s: &str) -> Result<(Token, &str), Error> {
        let (int, rest) = extract(s, is_invalid_integer_char);

        if int.starts_with('_') || int.ends_with('_') {
            return Err(Error::new(
                LexicalError::InvalidSequence,
                Span::new(0, int.len() as u32),
            ));
        }

        Ok((
            Token::new(
                TokenType::Literal(Literal::IntegerLiteral(IntegerLiteral(
                    s[..int.len()].into(),
                ))),
                Span::new(0, int.len() as u32),
            ),
            rest,
        ))
    }
}

/// A boolean literal. Can be either `True` or `False`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BooleanLiteral {
    /// The `true` variant.
    True,
    /// The `false` variant.
    False,
}

impl Lex for BooleanLiteral {
    fn lex(s: &str) -> Result<(Token, &str), Error> {
        let (token, rest) = find_token_end(s);
        if token == "true" {
            Ok((
                Token::new(
                    TokenType::Literal(Literal::BooleanLiteral(BooleanLiteral::True)),
                    Span::new(0, 4),
                ),
                rest,
            ))
        } else if token == "false" {
            Ok((
                Token::new(
                    TokenType::Literal(Literal::BooleanLiteral(BooleanLiteral::False)),
                    Span::new(0, 5),
                ),
                rest,
            ))
        } else {
            Err(Error::new(
                LexicalError::SequenceNotFound("true | false"),
                Span::new(0, token.len() as u32),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use cryo_span::Span;

    use crate::{
        Error, Lex, Token, TokenType,
        literal::{BooleanLiteral, IntegerLiteral, Literal, StringLiteral},
    };

    #[test]
    fn lex_str_lit() {
        let input = "\"hello\"";

        assert_eq!(
            StringLiteral::lex(input),
            Ok((
                Token::new(
                    TokenType::Literal(Literal::StringLiteral(StringLiteral("hello".into()))),
                    Span::new(0, 7)
                ),
                ""
            ))
        )
    }

    #[test]
    fn lex_str_lit_with_escapes() {
        let input = "\"hello, world\\n\"";
        assert_eq!(
            StringLiteral::lex(input),
            Ok((
                Token::new(
                    TokenType::Literal(Literal::StringLiteral(StringLiteral(
                        "hello, world\\n".into()
                    ))),
                    Span::new(0, 16)
                ),
                ""
            ))
        )
    }

    #[test]
    fn do_not_parse_unclosed_str_lit() {
        let input = "\"hello, world";

        assert_eq!(
            StringLiteral::lex(input),
            Err(Error::new(
                crate::LexicalError::SequenceNotFound("\""),
                Span::new(0, 13)
            ))
        )
    }

    #[test]
    fn parse_int_lit() {
        let input = "123456";
        assert_eq!(
            IntegerLiteral::lex(input),
            Ok((
                Token::new(
                    TokenType::Literal(Literal::IntegerLiteral(IntegerLiteral(input.into()))),
                    Span::new(0, 6)
                ),
                ""
            ))
        )
    }

    #[test]
    fn parse_int_lit_with_separator() {
        let input = "123_456";
        assert_eq!(
            IntegerLiteral::lex(input),
            Ok((
                Token::new(
                    TokenType::Literal(Literal::IntegerLiteral(IntegerLiteral(input.into()))),
                    Span::new(0, 7)
                ),
                ""
            ))
        )
    }

    #[test]
    fn do_not_parse_with_starting_separator() {
        let input = "_123";
        assert_eq!(
            IntegerLiteral::lex(input),
            Err(Error::new(
                crate::LexicalError::InvalidSequence,
                Span::new(0, 4)
            ))
        )
    }

    #[test]
    fn do_not_parse_with_ending_separator() {
        let input = "123_";
        assert_eq!(
            IntegerLiteral::lex(input),
            Err(Error::new(
                crate::LexicalError::InvalidSequence,
                Span::new(0, 4)
            ))
        )
    }

    #[test]
    fn parse_bool_lit() {
        assert_eq!(
            BooleanLiteral::lex("true"),
            Ok((
                Token::new(
                    TokenType::Literal(Literal::BooleanLiteral(BooleanLiteral::True)),
                    Span::new(0, 4)
                ),
                ""
            ))
        );

        assert_eq!(
            BooleanLiteral::lex("false"),
            Ok((
                Token::new(
                    TokenType::Literal(Literal::BooleanLiteral(BooleanLiteral::False)),
                    Span::new(0, 5)
                ),
                ""
            ))
        )
    }
}
