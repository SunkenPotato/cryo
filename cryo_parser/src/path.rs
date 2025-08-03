//! Paths, such as `super::ParseResult`.

use std::ops::{Deref, DerefMut};

use cryo_lexer::{identifier::Identifier, stream::StreamLike};
use cryo_span::Spanned;

use crate::{Parse, Punctuated, atoms::DoubleColon, ident::Ident};

type Inner = Punctuated<Ident, DoubleColon>;

/// A path. This acts the same as a `Punctuated<Ident, DoubleColon>`, except that it requires at least one segment to be present and therefore can only be constructed with
/// [`Path::new`].
#[derive(Debug, PartialEq, Eq)]
pub struct Path {
    inner: Inner,
}

impl Path {
    /// Constructs a new [`Path`] from a `Punctuated`. This will error if the passed sequence has no segments, i.e., the `last` field and the `inner` field are empty.
    pub const fn new(inner: Inner) -> Result<Self, Inner> {
        if inner.inner.is_empty() && inner.last.is_none() {
            return Err(inner);
        }

        Ok(Self { inner })
    }
}

impl Parse for Path {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        tokens.peek_require::<Identifier>()?;

        Ok(Self::new(
            tokens
                .with(Inner::parse)
                .expect("`Punctuated` should never return `Err`"),
        )
        .expect("at least one segment should be present since we required an identifier before"))
    }
}

impl Deref for Path {
    type Target = Inner;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for Path {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl AsRef<Inner> for Path {
    fn as_ref(&self) -> &Inner {
        self.deref()
    }
}

#[cfg(false)]
impl From<Inner> for Path {
    fn from(value: Inner) -> Self {
        // unwrapping is ok because this is only on `test`.
        Self::new(value).unwrap()
    }
}

impl From<Ident> for Path {
    fn from(value: Ident) -> Self {
        Self {
            inner: Punctuated {
                inner: vec![],
                last: Some(Box::new(Spanned::new(value, value.sym.span))),
            },
        }
    }
}
