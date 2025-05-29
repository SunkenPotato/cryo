use crate::ast::{Parse, tag};

pub const STRING_DELIMITER: &str = "\"";

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum StringParseError {
    DelimiterNotFound,
    InvalidEscape,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringLiteral(pub String);

impl<'a> Parse<'a> for StringLiteral {
    type Error = StringParseError;

    fn parse(input: &str) -> Result<(Self, &str), Self::Error> {
        let input = tag(STRING_DELIMITER, input).ok_or(StringParseError::DelimiterNotFound)?;

        let mut iter = input.bytes();
        let mut str = vec![];
        let mut end = 0;

        println!("{input}");

        while let Some(byte) = iter.next() {
            end += 1;
            if byte == b'\\' {
                let Some(next) = iter.next() else {
                    return Err(StringParseError::InvalidEscape);
                };
                end += 1;

                let next_c = match next {
                    b'\"' => b'"',
                    b'n' => b'\n',
                    b'u' => {
                        unimplemented!()
                    }
                    _ => return Err(StringParseError::InvalidEscape),
                };

                str.push(next_c);
                continue;
            } else if byte == b'"' {
                break;
            }

            str.push(byte)
        }
        let str = String::from_utf8(str).unwrap();
        Ok((Self(str), &input[end..]))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Parse, literal::string::StringLiteral};

    #[test]
    fn parse_string() {
        let s = "\"hello, world\"";

        assert_eq!(
            Ok((StringLiteral("hello, world".into()), "")),
            StringLiteral::parse(s)
        );
    }

    #[test]
    fn parse_string_with_escape_character() {
        let s = "\"ferris says: \\\"hello, world\\\"\"";

        assert_eq!(
            Ok((StringLiteral("ferris says: \"hello, world\"".into()), "")),
            StringLiteral::parse(s)
        );
    }
}
