//! A literal.
//!
//! Literals are tokens which do not require any further computation and are direct, primitive values.

use cryo_span::Span;

use crate::{
    Lex, LexicalError, LexicalErrorKind, Symbol, Token, TokenKind, extract, find_token_end,
};

/// A literal. View the module-level docs for more information.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Literal {
    /// A string literal.
    StringLiteral(StringLiteral),
    /// An integer literal.
    IntegerLiteral(IntegerLiteral),
}

impl Lex for Literal {
    fn lex(s: &str) -> Result<(crate::Token, &str), LexicalError> {
        let first = s.chars().next();
        if let Some('"') = first {
            return StringLiteral::lex(s);
        }

        if let Some('0'..='9') = first {
            return IntegerLiteral::lex(s);
        }

        let (start, _) = find_token_end(s);
        Err(LexicalError::new(
            LexicalErrorKind::NoMatch,
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

impl Lex for StringLiteral {
    fn lex(s: &str) -> Result<(crate::Token, &str), crate::LexicalError> {
        let (token, rest) = find_token_end(s);
        let span = Span::new(0, token.len() as u32);
        let unquoted = match token.chars().next() {
            Some('"') => &s[1..],
            Some(_) => {
                return Err(LexicalError::new(
                    LexicalErrorKind::SequenceNotFound("\""),
                    span,
                ));
            }
            None => {
                return Err(LexicalError::new(
                    LexicalErrorKind::EndOfInput,
                    Span::new(0, 0),
                ));
            }
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
            return Err(LexicalError::new(
                LexicalErrorKind::SequenceNotFound("\""),
                span,
            ));
        }

        Ok((
            Token::new(TokenKind::StringLiteral, unquoted[..cursor].into(), span),
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

fn is_invalid_integer_char(c: char) -> bool {
    !matches!(c, '0'..='9' | '_')
}

impl Lex for IntegerLiteral {
    fn lex(s: &str) -> Result<(Token, &str), LexicalError> {
        let (int, rest) = extract(s, is_invalid_integer_char);

        if int.starts_with('_') || int.ends_with('_') {
            return Err(LexicalError::new(
                LexicalErrorKind::InvalidSequence,
                Span::new(0, int.len() as u32),
            ));
        }

        Ok((
            Token::new(
                TokenKind::IntegerLiteral,
                s[..int.len()].into(),
                Span::new(0, int.len() as u32),
            ),
            rest,
        ))
    }
}

#[cfg(test)]
mod tests {
    use cryo_span::Span;

    use crate::{
        Lex, LexicalError, Token, TokenKind,
        literal::{IntegerLiteral, StringLiteral},
    };

    #[test]
    fn lex_str_lit() {
        let input = "\"hello\"";

        assert_eq!(
            StringLiteral::lex(input),
            Ok((
                Token::new(TokenKind::StringLiteral, "hello".into(), Span::new(0, 7)),
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
                    TokenKind::StringLiteral,
                    "hello, world\\n".into(),
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
            Err(LexicalError::new(
                crate::LexicalErrorKind::SequenceNotFound("\""),
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
                Token::new(TokenKind::IntegerLiteral, input.into(), Span::new(0, 6)),
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
                Token::new(TokenKind::IntegerLiteral, input.into(), Span::new(0, 7)),
                ""
            ))
        )
    }

    #[test]
    fn do_not_parse_with_starting_separator() {
        let input = "_123";
        assert_eq!(
            IntegerLiteral::lex(input),
            Err(LexicalError::new(
                crate::LexicalErrorKind::InvalidSequence,
                Span::new(0, 4)
            ))
        )
    }

    #[test]
    fn do_not_parse_with_ending_separator() {
        let input = "123_";
        assert_eq!(
            IntegerLiteral::lex(input),
            Err(LexicalError::new(
                crate::LexicalErrorKind::InvalidSequence,
                Span::new(0, 4)
            ))
        )
    }
}
