use std::fmt::Debug;

pub mod literal;

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
    let idx = s
        .char_indices()
        .take_while(|(_, c)| c.is_whitespace())
        .map(|(idx, _)| idx)
        .last()
        .unwrap_or(0);

    &s[idx..]
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
