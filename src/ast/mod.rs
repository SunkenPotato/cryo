pub mod expr;
pub mod literal;

use std::fmt::Debug;

#[derive(Debug, PartialEq)]
pub enum IdentifierParseError {
    InvalidToken,
    Empty,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier<'a>(pub &'a str);

impl<'a> Parse<'a> for Identifier<'a> {
    type Error = IdentifierParseError;

    fn parse(mut input: &'a str) -> Result<(Identifier<'a>, &'a str), Self::Error> {
        input = extract_whitespace(input);
        let (id, rest) = extract(input, |c| !c.is_whitespace());
        if input.is_empty() {
            return Err(IdentifierParseError::Empty);
        }

        // this is OK because we checked if the string is empty, meaning the string has at least ONE character
        if input.chars().next().unwrap().is_ascii_digit() || !input.is_ascii() {
            return Err(IdentifierParseError::InvalidToken);
        }

        Ok((Identifier(id), rest))
    }
}

pub trait Parse<'a>: Sized {
    type Error: Debug;

    fn parse(input: &'a str) -> Result<(Self, &'a str), Self::Error>
    where
        Self: 'a;
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
