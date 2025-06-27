use cryo_span::Span;

use crate::{
    Error, FromToken, Lex, LexicalError, Token, TokenType, extract, find_token_end, token_marker,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Literal<'source> {
    StringLiteral(StringLiteral<'source>),
    IntegerLiteral(IntegerLiteral<'source>),
}

token_marker!(Literal<'source>);

impl<'source> Lex for Literal<'source> {
    fn lex(s: &str) -> Result<(crate::Token, &str), Error> {
        let first = s.chars().next();
        if let Some('"') = first {
            return StringLiteral::lex(s);
        }

        if let Some('0'..='9' | '-') = first {
            return IntegerLiteral::lex(s);
        }

        let whitespace_pos = s.find(|c: char| c.is_whitespace()).unwrap_or(s.len());
        Err(Error::new(
            LexicalError::NoMatch,
            Span::new(0, whitespace_pos),
        ))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StringLiteral<'source>(pub &'source str);

impl<'s> FromToken<'s> for StringLiteral<'s> {
    fn from_token<'borrow>(token: &'borrow TokenType<'s>) -> Option<&'borrow Self> {
        match Literal::from_token(token)? {
            Literal::StringLiteral(s) => Some(s),
            _ => None,
        }
    }
}

impl Lex for StringLiteral<'_> {
    fn lex(s: &str) -> Result<(crate::Token, &str), crate::Error> {
        let (token, rest) = find_token_end(s);
        let span = Span::new(0, token.len());
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
                TokenType::Literal(Literal::StringLiteral(StringLiteral(&unquoted[..cursor]))),
                span,
            ),
            rest,
        ))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct IntegerLiteral<'source>(pub &'source str);

impl<'s> FromToken<'s> for IntegerLiteral<'s> {
    fn from_token<'borrow>(token: &'borrow TokenType<'s>) -> Option<&'borrow Self> {
        match Literal::from_token(token)? {
            Literal::IntegerLiteral(v) => Some(v),
            _ => None,
        }
    }
}

fn is_invalid_integer_char(c: char) -> bool {
    !matches!(c, '0'..='9' | '_')
}

impl Lex for IntegerLiteral<'_> {
    fn lex(s: &str) -> Result<(Token, &str), Error> {
        let (neg, rest) = extract(s, |c| c != '-');
        let (int, rest) = extract(rest, is_invalid_integer_char);
        let total_len = neg.len() + int.len();

        if int.starts_with('_') || int.ends_with('_') {
            return Err(Error::new(
                LexicalError::InvalidSequence,
                Span::new(0, total_len),
            ));
        }

        Ok((
            Token::new(
                TokenType::Literal(Literal::IntegerLiteral(IntegerLiteral(&s[..total_len]))),
                Span::new(0, total_len),
            ),
            rest,
        ))
    }
}

#[cfg(test)]
mod tests {
    use cryo_span::Span;

    use crate::{
        Error, Lex, Token, TokenType,
        literal::{IntegerLiteral, Literal, StringLiteral},
    };

    #[test]
    fn lex_str_lit() {
        let input = "\"hello\"";

        assert_eq!(
            StringLiteral::lex(input),
            Ok((
                Token::new(
                    TokenType::Literal(Literal::StringLiteral(StringLiteral("hello"))),
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
                    TokenType::Literal(Literal::StringLiteral(StringLiteral("hello, world\\n"))),
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
                    TokenType::Literal(Literal::IntegerLiteral(IntegerLiteral(input))),
                    Span::new(0, 6)
                ),
                ""
            ))
        )
    }

    #[test]
    fn parse_int_lit_with_neg_sign() {
        let input = "--123456";
        assert_eq!(
            IntegerLiteral::lex(input),
            Ok((
                Token::new(
                    TokenType::Literal(Literal::IntegerLiteral(IntegerLiteral(input))),
                    Span::new(0, 8)
                ),
                ""
            ))
        )
    }

    #[test]
    fn parse_int_lit_with_separator() {
        let input = "--123_456";
        assert_eq!(
            IntegerLiteral::lex(input),
            Ok((
                Token::new(
                    TokenType::Literal(Literal::IntegerLiteral(IntegerLiteral(input))),
                    Span::new(0, 9)
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
}
