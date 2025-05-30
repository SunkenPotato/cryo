use crate::{
    ast::{Parse, ParseError},
    lexer::tokens::{Literal as LiteralToken, Token},
};

pub enum Literal {
    StringLiteral(StringLiteral),
    NumberLiteral(NumberLiteral),
}

#[derive(Debug, PartialEq)]
pub struct StringLiteral(pub String);

impl Parse for StringLiteral {
    fn parse(stream: &[Token]) -> Result<(Self, &[Token]), ParseError> {
        let token = stream.first().ok_or(ParseError::end_of_input(stream))?;
        let literal = token
            .as_any()
            .require_err(ParseError::expected_literal(&stream[0..1]))?;

        if let LiteralToken::StringLiteral(l) = literal {
            let mut buf = String::new();
            let mut peekable = l.bytes().peekable();

            while let Some(v) = peekable.next() {
                if v == b'\\' {
                    // Is ok because string literals are checked for proper syntax in the lexer.
                    let next = *peekable.peek().unwrap() as char;

                    match next {
                        '"' => (),
                        'n' => buf.push('\n'),
                        't' => buf.push('\t'),
                        _ => {
                            return Err(ParseError::invalid_escape(
                                &stream[0..1],
                                format!("'{next}' is not a valid escape character"),
                            ));
                        }
                    }

                    peekable.next();
                } else if v == b'"' {
                    break;
                } else {
                    buf.push(v as char);
                }
            }

            // no need to check if string was closed, lexer already did that

            Ok((Self(buf), &stream[1..]))
        } else {
            Err(ParseError::invalid_type(&stream[0..1], "expected a string"))
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct NumberLiteral(pub i32);

impl Parse for NumberLiteral {
    fn parse(stream: &[Token]) -> Result<(Self, &[Token]), ParseError> {
        let token = stream.first().ok_or(ParseError::end_of_input(stream))?;
        let literal = token
            .as_any()
            .require_err(ParseError::expected_literal(&stream[0..1]))?;

        if let LiteralToken::NumberLiteral(l) = literal {
            let mut num = 0i32;
            let mut neg = 1;

            for byte in l.bytes() {
                if byte == b'-' {
                    neg *= -1;
                }

                num = num
                    .checked_mul(10)
                    .ok_or(ParseError::numerical_overflow(&stream[0..1]))?
                    .checked_add(byte as i32 - 0x30)
                    .ok_or(ParseError::numerical_overflow(&stream[0..1]))?;
            }

            num *= neg;

            Ok((Self(num), &stream[1..]))
        } else {
            Err(ParseError::invalid_type(&stream[0..1], "expected a number"))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Parse;
    use crate::lexer::tokens::*;
    use crate::token;

    use super::{NumberLiteral, StringLiteral};

    #[test]
    fn parse_num_lit() {
        let s = &[token![ln 345]];

        let (out, _) = match NumberLiteral::parse(s) {
            Ok(v) => v,
            Err(e) => panic!("{e}"),
        };

        assert_eq!(NumberLiteral(345), out);
    }

    #[test]
    fn parse_str_lit() {
        let input = "\"Hello, world\\n\"";
        let token = match Literal::lex(input) {
            Ok((v, _)) => v,
            Err(e) => panic!("{e}"),
        };

        let lit = match StringLiteral::parse(&[token]) {
            Ok((v, _)) => v,
            Err(e) => panic!("{e}"),
        };

        assert_eq!(lit, StringLiteral("Hello, world\n".into()));
    }
}
