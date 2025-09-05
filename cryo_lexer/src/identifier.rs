//! Identifiers.
//!
//! View [`Identifier`] for more information.

use cryo_span::Span;

use crate::{Lex, LexicalError, Symbol, Token, TokenKind, extract};

/// An identifier.
///
/// Identifiers are tokens used to identify bindings, functions, or structures. One may also view them as names.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Identifier(pub Symbol);

fn split_at_ident_end(s: &str) -> (&str, &str) {
    extract(s, |c| {
        matches!(
            c,
            ',' | ';'
                | '('
                | ')'
                | '{'
                | '}'
                | ':'
                | '.'
                | '-'
                | '+'
                | '*'
                | '/'
                | '%'
                | '='
                | '!'
                | ' '
        )
    })
}

fn is_invalid_ident_char(c: char) -> bool {
    !matches!(c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_')
}

impl Lex for Identifier {
    fn lex(s: &str) -> Result<(crate::Token, &str), crate::LexicalError> {
        let (token, rest) = split_at_ident_end(s);

        let span = Span::new(0, token.len() as u32);
        let ('a'..='z' | 'A'..='Z' | '_') = token.chars().next().ok_or(LexicalError::new(
            crate::LexicalErrorKind::EndOfInput,
            Span::new(0, 0),
        ))?
        else {
            return Err(LexicalError::new(
                crate::LexicalErrorKind::InvalidSequence,
                span,
            ));
        };

        if token.contains(is_invalid_ident_char) {
            return Err(LexicalError::new(
                crate::LexicalErrorKind::InvalidSequence,
                span,
            ));
        }

        Ok((Token::new(TokenKind::Identifier, token.into(), span), rest))
    }
}

#[cfg(test)]
mod tests {
    use cryo_span::Span;

    use crate::{Lex, LexicalError, Token, TokenKind, identifier::Identifier};

    #[test]
    fn parse_ident() {
        assert_eq!(
            Identifier::lex("twenty_1"),
            Ok((
                Token::new(TokenKind::Identifier, "twenty_1".into(), Span::new(0, 8)),
                ""
            ))
        )
    }

    #[test]
    fn do_not_parse_ident_starting_with_digit() {
        assert_eq!(
            Identifier::lex("20_one"),
            Err(LexicalError::new(
                crate::LexicalErrorKind::InvalidSequence,
                Span::new(0, 6)
            ))
        );
    }

    #[test]
    fn do_not_parse_ident_invalid_characters() {
        assert_eq!(
            Identifier::lex("a_$"),
            Err(LexicalError::new(
                crate::LexicalErrorKind::InvalidSequence,
                Span::new(0, 3)
            ))
        )
    }

    #[test]
    fn do_not_parse_empty_identifier() {
        assert_eq!(
            Identifier::lex(""),
            Err(LexicalError::new(
                crate::LexicalErrorKind::EndOfInput,
                Span::new(0, 0)
            ))
        );
    }
}
