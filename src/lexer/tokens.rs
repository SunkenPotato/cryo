use std::{any::Any, fmt::Display, str::FromStr};

use internment::Intern;

use super::{LexicalError, extract, tag};

pub type LexFunction = fn(&str) -> Result<(Token, &str), LexicalError>;

pub trait Validate: Sized {
    /// Note: expects input to have whitespace stripped
    fn lex(s: &str) -> Result<(Token, &str), LexicalError>;
}

#[repr(u8)]
#[derive(PartialEq, Eq, Debug)]
pub enum Token {
    Keyword(Keyword) = Self::KEYWORD_TOKEN,
    Identifier(Identifier) = Self::IDENTIFIER_TOKEN,
    Literal(Literal) = Self::LITERAL_TOKEN,
    Assign(Assign) = Self::ASSIGN_TOKEN,
    Operation(Operation) = Self::OPERATION_TOKEN,
    Semicolon(Semicolon) = Self::SEMICOLON_TOKEN,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_display())
    }
}

pub struct AnyToken<'a>(&'a dyn Any);

impl<'a> AnyToken<'a> {
    #[must_use]
    pub fn require<T: 'static>(self) -> Option<&'a T> {
        self.0.downcast_ref::<T>()
    }

    pub fn require_err<T: 'static, E>(self, e: E) -> Result<&'a T, E> {
        self.0.downcast_ref::<T>().ok_or(e)
    }
}

impl Token {
    pub const PARSE_FUNCTIONS: &'static [LexFunction] = &[
        Keyword::lex,
        Identifier::lex,
        Literal::lex,
        Assign::lex,
        Operation::lex,
        Semicolon::lex,
    ];

    #[must_use]
    pub fn as_any(&self) -> AnyToken<'_> {
        AnyToken(match self {
            Self::Keyword(v) => v,
            Self::Identifier(v) => v,
            Self::Literal(v) => v,
            Self::Assign(v) => v,
            Self::Operation(v) => v,
            Self::Semicolon(v) => v,
        })
    }

    #[must_use]
    pub fn as_display(&self) -> &dyn Display {
        match self {
            Self::Keyword(v) => v,
            Self::Identifier(v) => v,
            Self::Literal(v) => v,
            Self::Assign(v) => v,
            Self::Operation(v) => v,
            Self::Semicolon(v) => v,
        }
    }

    pub const KEYWORD_TOKEN: u8 = 0;
    pub const IDENTIFIER_TOKEN: u8 = 1;
    pub const LITERAL_TOKEN: u8 = 2;
    pub const ASSIGN_TOKEN: u8 = 3;
    pub const OPERATION_TOKEN: u8 = 4;
    pub const SEMICOLON_TOKEN: u8 = 5;

    /// Check whether the current token matches the supplied discriminant.
    ///
    /// Use the associated constants of this struct with this function.
    #[must_use]
    pub fn is(&self, d: u8) -> bool {
        unsafe { *<*const _>::from(self).cast::<u8>() == d }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Keyword {
    Let,
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "let" => Self::Let,
            _ => return Err(()),
        })
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl Keyword {
    pub const VARIANTS: &[Self] = &[Self::Let];

    #[must_use]
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Let => "let",
        }
    }
}

impl Validate for Keyword {
    fn lex(s: &str) -> Result<(Token, &str), LexicalError> {
        for variant in Self::VARIANTS {
            let Some(rest) = tag(variant.as_str(), s) else {
                continue;
            };

            return Ok((Token::Keyword(*variant), rest));
        }

        Err(LexicalError::new(
            1,
            s.split_whitespace().next().unwrap_or(s),
            "expected keyword",
        ))
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Identifier(Intern<String>);

impl Identifier {
    pub fn new(s: impl Into<String>) -> Self {
        Self(Intern::new(s.into()))
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Validate for Identifier {
    fn lex(s: &str) -> Result<(Token, &str), LexicalError> {
        let (id, r) = extract(s, |c| c.is_ascii_whitespace());

        let first_byte = id
            .bytes()
            .next()
            .ok_or_else(|| dbg!(LexicalError::new(2, "", "expected identifier")))?;

        if first_byte.is_ascii_digit() {
            return Err(dbg!(LexicalError::new(
                3,
                id,
                "identifiers may not begin with a number",
            )));
        }

        for byte in id.bytes() {
            if !byte.is_ascii_alphanumeric() && byte != b'_' {
                return Err(dbg!(LexicalError::new(
                    4,
                    id,
                    format!(
                        "identifiers may not contain the character '{}'",
                        byte as char
                    ),
                )));
            }
        }

        Ok((Token::Identifier(Self::new(id)), r))
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Literal {
    StringLiteral(String),
    NumberLiteral(String),
}

impl Literal {
    #[must_use]
    pub fn as_display(&self) -> &dyn Display {
        match self {
            Self::StringLiteral(v) | Self::NumberLiteral(v) => v,
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_display())
    }
}

impl Validate for Literal {
    fn lex(s: &str) -> Result<(Token, &str), LexicalError> {
        // Try parsing a string
        //
        // If s starts with a '"', continue
        if let Some(s) = tag("\"", s) {
            let mut peekable = s.chars().peekable();
            let mut end = 0;
            let mut closed = false;

            while let Some(byte) = peekable.next() {
                end += 1;
                // if we encounter an escape character, check the next character
                // if it's valid, advance
                // else fail
                if byte == '\\' {
                    end += 1;
                    peekable.next();
                } else if byte == '"' {
                    closed = true;
                    break;
                }
            }

            if closed {
                Ok((
                    Token::Literal(Literal::StringLiteral(s[..end].to_owned())),
                    &s[end..],
                ))
            } else {
                Err(LexicalError::new(
                    5,
                    &s[..end],
                    "string terminated too early",
                ))
            }
        } else {
            let (num_str, rest) = extract(s, |c| !c.is_ascii_digit());

            if num_str.is_empty() {
                return Err(LexicalError::new(6, "", "expected a literal"));
            }

            Ok((
                Token::Literal(Literal::NumberLiteral(num_str.to_owned())),
                rest,
            ))
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Assign;

impl Display for Assign {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "=")
    }
}

impl Validate for Assign {
    fn lex(s: &str) -> Result<(Token, &str), LexicalError> {
        tag("=", s)
            .map(|r| (Token::Assign(Self), r))
            .ok_or_else(|| LexicalError::new(7, &s[0..1], "expected '='"))
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Semicolon;

impl Display for Semicolon {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ";")
    }
}

impl Validate for Semicolon {
    fn lex(s: &str) -> Result<(Token, &str), LexicalError> {
        tag(";", s)
            .ok_or_else(|| LexicalError::new(8, &s[0..1], "expected ';'"))
            .map(|r| (Token::Semicolon(Self), r))
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl Operation {
    #[cfg(test)]
    #[must_use]
    pub const fn from_char(c: char) -> Self {
        match c {
            '+' => Self::Add,
            '-' => Self::Sub,
            '*' => Self::Mul,
            '/' => Self::Div,
            '%' => Self::Mod,
            _ => panic!(),
        }
    }

    #[must_use]
    pub fn as_char(&self) -> &str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
        }
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_char())
    }
}

impl Validate for Operation {
    fn lex(s: &str) -> Result<(Token, &str), LexicalError> {
        Ok((
            Token::Operation(
                match s.bytes().next().ok_or_else(|| {
                    LexicalError::new(9, &s[0..1], "expected valid mathematical operand")
                })? {
                    b'+' => Self::Add,
                    b'-' => Self::Sub,
                    b'*' => Self::Mul,
                    b'/' => Self::Div,
                    b'%' => Self::Mod,
                    _ => {
                        return Err(LexicalError::new(
                            9,
                            &s[0..1],
                            "expected valid mathematical operand",
                        ));
                    }
                },
            ),
            &s[1..],
        ))
    }
}
