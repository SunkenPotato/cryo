use std::fmt::Display;

use tokens::Token;

pub mod tokens;

#[must_use]
pub fn tag<'s>(pat: &str, s: &'s str) -> Option<&'s str> {
    s.strip_prefix(pat)
}

pub fn extract(s: &str, f: fn(u8) -> bool) -> (&str, &str) {
    let end = s
        .bytes()
        .enumerate()
        .find_map(|(idx, c)| if f(c) { Some(idx) } else { None })
        .unwrap_or(s.len());

    (&s[..end], &s[end..])
}

#[must_use]
pub fn extract_whitespace(s: &str) -> &str {
    extract(s, |b| !b.is_ascii_whitespace()).1
}

#[derive(Debug, PartialEq, Eq)]
pub struct LexicalError<'s> {
    code: u8,
    invalid_section: &'s str,
    message: String,
}

impl<'s> LexicalError<'s> {
    pub fn new(code: u8, invalid_section: &'s str, message: impl Into<String>) -> Self {
        Self {
            code,
            invalid_section,
            message: message.into(),
        }
    }
}

impl Display for LexicalError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "error[{}]: {}", self.code, self.message)?;

        for line in self.invalid_section.lines() {
            write!(f, "|\t{line}")?;
        }

        Ok(())
    }
}

pub struct Lexer;

impl Lexer {
    #[allow(clippy::missing_panics_doc)]
    pub fn lex(string: &str) -> Result<Vec<Token>, LexicalError> {
        let mut next = string.trim();
        let mut stream = vec![];

        while !next.is_empty() {
            let mut possible_token = None;
            let mut possible_error = None;

            'parse: for f in Token::PARSE_FUNCTIONS {
                match f(next) {
                    Ok((token, rest)) => {
                        next = extract_whitespace(rest);
                        dbg!(&token, next, rest);
                        possible_token = Some(token);
                        break 'parse;
                    }

                    Err(e) => possible_error = Some(e),
                }
            }

            match possible_token {
                Some(t) => {
                    stream.push(t);
                }
                None => return Err(possible_error.unwrap()),
            }
        }

        Ok(stream)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[macro_export]
    macro_rules! token {
        (id $id:tt) => {
            $crate::lexer::tokens::Token::Identifier($crate::lexer::tokens::Identifier::new(
                stringify!($id),
            ))
        };

        (ls $ls:expr) => {
            $crate::lexer::tokens::Token::Literal($crate::lexer::tokens::Literal::StringLiteral(
                $ls.into(),
            ))
        };

        (ln $ln:expr) => {
            $crate::lexer::tokens::Token::Literal($crate::lexer::tokens::Literal::NumberLiteral(
                stringify!($ln).to_owned(),
            ))
        };

        (=) => {
            $crate::lexer::tokens::Token::Assign($crate::lexer::tokens::Assign)
        };

        (;) => {
            $crate::lexer::tokens::Token::Semicolon($crate::lexer::tokens::Semicolon)
        };

        (op $op:tt) => {
            $crate::lexer::tokens::Token::Operation($crate::lexer::tokens::Operation::from_char(
                stringify!($op).chars().next().unwrap(),
            ))
        };

        (let) => {
            $crate::lexer::tokens::Token::Keyword($crate::lexer::tokens::Keyword::Let)
        };
    }

    #[test]
    fn lex() {
        let s = "let f = 5 + 7;";

        assert_eq!(
            Ok(vec![
                token![let],
                token![id f],
                token![=],
                token![ln 5],
                token![op+],
                token![ln 7],
                token![;]
            ]),
            Lexer::lex(s)
        );
    }
}
