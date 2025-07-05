//! Identifiers.
//!
//! View [`Identifier`] for more information.

use cryo_span::{Span, Spanned};

use crate::{Error, FromToken, Lex, Sealed, Token, TokenType, find_token_end};

/// An identifier.
///
/// Identifiers are tokens used to identify bindings, functions, or structures. One may also view them as names.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Identifier<'source>(pub &'source str);

fn is_invalid_ident_char(c: char) -> bool {
    !matches!(c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_')
}

impl Lex for Identifier<'_> {
    fn lex(s: &str) -> Result<(crate::Token, &str), crate::Error> {
        let (token, rest) = find_token_end(s);

        let span = Span::new(0, token.len());
        let ('a'..='z' | 'A'..='Z' | '_') = token
            .chars()
            .next()
            .ok_or(Error::new(crate::LexicalError::EndOfInput, Span::new(0, 0)))?
        else {
            return Err(Error::new(crate::LexicalError::InvalidSequence, span));
        };

        if token.contains(is_invalid_ident_char) {
            return Err(Error::new(crate::LexicalError::InvalidSequence, span));
        }

        Ok((
            Token::new(TokenType::Identifier(Identifier(token)), span),
            rest,
        ))
    }
}

impl<'source> FromToken<'source> for Identifier<'source> {
    const NAME: &'static str = "Identifier";
    fn from_token<'borrow>(token: &'borrow Token<'source>) -> Option<Spanned<&'borrow Self>> {
        match token.t {
            TokenType::Identifier(ref id) => Some(Spanned::new(id, token.span)),
            _ => None,
        }
    }
}

impl Sealed for Identifier<'_> {}

#[cfg(test)]
mod tests {
    use cryo_span::Span;

    use crate::{Error, Lex, Token, TokenType, identifier::Identifier};

    #[test]
    fn parse_ident() {
        assert_eq!(
            Identifier::lex("twenty_1"),
            Ok((
                Token::new(
                    TokenType::Identifier(Identifier("twenty_1")),
                    Span::new(0, 8)
                ),
                ""
            ))
        )
    }

    #[test]
    fn do_not_parse_ident_starting_with_digit() {
        assert_eq!(
            Identifier::lex("20_one"),
            Err(Error::new(
                crate::LexicalError::InvalidSequence,
                Span::new(0, 6)
            ))
        );
    }

    #[test]
    fn do_not_parse_ident_invalid_characters() {
        assert_eq!(
            Identifier::lex("a_$"),
            Err(Error::new(
                crate::LexicalError::InvalidSequence,
                Span::new(0, 3)
            ))
        )
    }

    #[test]
    fn do_not_parse_empty_identifier() {
        assert_eq!(
            Identifier::lex(""),
            Err(Error::new(crate::LexicalError::EndOfInput, Span::new(0, 0)))
        );
    }
}
