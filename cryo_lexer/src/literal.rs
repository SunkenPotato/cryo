use cryo_span::Span;

use super::{
    INITIAL_FILE, Lex, extract, tag,
    tokens::{Token, TokenType},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    StringLiteral(StringLiteral),
    NumberLiteral(NumberLiteral),
}

impl Lex for Literal {
    fn lex(input: &str) -> Result<(Token, &str), Span> {
        let first = input.chars().next().ok_or(Span::EMPTY)?;

        if first == '"' {
            StringLiteral::lex(input)
        } else if first.is_ascii_digit() {
            NumberLiteral::lex(input)
        } else {
            Err(Span::ONE)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLiteral(pub String);

impl Lex for StringLiteral {
    fn lex(input: &str) -> Result<(super::tokens::Token, &str), Span> {
        let Some(rest) = tag("\"", input) else {
            return Err(Span::ONE);
        };

        let mut end = 0usize;
        let mut closed = false;
        let mut iter = rest.chars().peekable();

        while let Some(c) = iter.next() {
            if c == '\\' {
                let Some(_) = iter.peek() else {
                    return Err(Span::new(INITIAL_FILE, end.saturating_sub(1), end));
                };

                iter.next();
                end += 1;
            } else if c == '"' {
                closed = true;
                break;
            }

            end += 1;
        }

        if !closed {
            return Err(Span::new(INITIAL_FILE, 0, end));
        }

        let token = Token::new(
            TokenType::Literal(Literal::StringLiteral(StringLiteral(
                rest[..end].to_owned(),
            ))),
            Span::new(INITIAL_FILE, 0, end),
        );

        // we add one here to remove the trailing '"'
        Ok((token, &rest[end + 1..]))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NumberLiteral(pub String);

impl Lex for NumberLiteral {
    fn lex(input: &str) -> Result<(Token, &str), Span> {
        let (num, rest) = extract(input, |c| !(c.is_ascii_digit() || c == '_'));

        if num.is_empty() || num.starts_with('_') {
            return Err(Span::new(INITIAL_FILE, 0, input.find(' ').unwrap_or(0)));
        }

        let token = Token::new(
            TokenType::Literal(Literal::NumberLiteral(NumberLiteral(num.to_owned()))),
            Span::new(INITIAL_FILE, 0, num.len()),
        );

        Ok((token, rest))
    }
}

#[cfg(test)]
mod tests {
    use crate::{Lex, literal::Literal, tokens::TokenType};

    use super::StringLiteral;

    #[test]
    fn parse_str() {
        let s = "\"Hello, world\"";
        let parsed = StringLiteral::lex(s).map(|(t, s)| (t.token, s));

        assert_eq!(
            Ok((
                TokenType::Literal(Literal::StringLiteral(StringLiteral("Hello, world".into()))),
                ""
            )),
            parsed
        );
    }
}
