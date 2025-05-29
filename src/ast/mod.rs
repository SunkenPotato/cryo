pub mod binding;
pub mod expr;
pub mod literal;

use std::fmt::Debug;

use internment::Intern;

#[derive(Debug, PartialEq)]
pub enum IdentifierParseError {
    InvalidToken,
    Empty,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(Intern<String>);

impl Identifier {
    pub fn new(s: impl Into<String>) -> Self {
        Identifier(Intern::new(s.into()))
    }

    pub fn get(&self) -> &str {
        &*self.0
    }
}

impl Parse for Identifier {
    type Error = IdentifierParseError;

    fn parse(mut input: &str) -> Result<(Identifier, &str), Self::Error> {
        input = extract_whitespace(input);
        let (id, rest) = extract(input, |c| !c.is_whitespace());
        if input.is_empty() {
            return Err(IdentifierParseError::Empty);
        }

        // this is OK because we checked if the string is empty, meaning the string has at least ONE character
        if id.chars().next().unwrap().is_ascii_digit() {
            return Err(IdentifierParseError::InvalidToken);
        }

        let mut is_invalid = false;

        for b in id.bytes().skip(1) {
            if !b.is_ascii_alphanumeric() {
                is_invalid = true;
                break;
            }
        }

        if is_invalid {
            return Err(IdentifierParseError::InvalidToken);
        }

        Ok((Identifier(Intern::new(id.to_string())), rest))
    }
}

pub trait Parse: Sized {
    type Error: Debug;

    fn parse(input: &str) -> Result<(Self, &str), Self::Error>;
}

/// Commodity for mapping parse results into their superclass results.
pub trait ParseResultInto<T> {
    fn into2(self) -> T;
}

impl<'s, T, TW, E, EW> ParseResultInto<Result<(TW, &'s str), EW>> for Result<(T, &'s str), E>
where
    T: Into<TW>,
    E: Into<EW>,
{
    fn into2(self) -> Result<(TW, &'s str), EW> {
        self.map(|(t, s)| (t.into(), s)).map_err(|e| e.into())
    }
}

pub fn strip_whitespace(s: &str) -> &str {
    extract(s, |c| c.is_ascii_whitespace()).1
}

pub fn extract(s: &str, predicate: impl Fn(&char) -> bool) -> (&str, &str) {
    let end = s
        .char_indices()
        .find_map(|(idx, c)| if predicate(&c) { None } else { Some(idx) })
        .unwrap_or(s.len());

    (&s[..end], &s[end..])
}

pub fn extract_whitespace(input: &str) -> &str {
    extract(input, |c| c.is_ascii_whitespace()).1
}

pub fn tag<'a, 'b>(pat: &'a str, s: &'b str) -> Option<&'b str> {
    if s.starts_with(pat) {
        return Some(&s[pat.len()..]);
    }

    None
}

pub fn tag_err<'a, 'b, E>(pat: &'a str, s: &'b str, err: E) -> Result<&'b str, E> {
    if s.starts_with(pat) {
        return Ok(&s[pat.len()..]);
    }

    Err(err)
}
