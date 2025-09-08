//! Attributes.

use cryo_lexer::{TokenKind, stream::StreamLike};
use cryo_parser_proc_macro::IsFail;
use cryo_span::Spanned;

use crate::{IsFail, Parse, Path};

/// An attribute.
///
/// Attributes are used to provide additional information to the compiler on how the annotated item should behave. \
/// They take the style of `@(#kind)`, where `#kind` is an attribute kind as defined in [`AttrKind`].
#[derive(Clone, PartialEq, Eq, Debug, IsFail)]
#[fail = false]
pub struct Attr {
    /// The style of this attribute, i.e., whether it is an inner or outer attribute.
    pub style: AttrStyle,
    /// The attribute kind.
    pub kind: Spanned<AttrKind>,
}

impl Parse for Attr {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        tokens.advance_require(TokenKind::At)?;
        let style = if tokens.advance_require(TokenKind::Bang).is_ok() {
            AttrStyle::Inner
        } else {
            AttrStyle::Outer
        };

        tokens.advance_require(TokenKind::LParen)?;
        let kind = tokens.spanning(AttrKind::parse)?;

        Ok(Self { style, kind })
    }
}

/// The attribute's style.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum AttrStyle {
    /// An outer attribute. This is defined as `@(#kind)`, where `#kind` is the attribute kind.
    ///
    /// This attribute style applies the attribute to the item below it.
    Outer,
    /// An inner attribute. This is defined as `@!(#kind)`, where `#kind` is the attribute kind.
    ///
    /// This attribute style applies the attribute to the item it is inside of.
    Inner,
}

/// An attribute kind. This is currently limited to only paths, but more variants will be added in the future, hence why this enum is `non_exhaustive`.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, IsFail)]
#[fail(bubble)]
pub enum AttrKind {
    /// A path attribute. This takes the form of `#attr_style(#path)`.
    Path(Path),
}

impl Parse for AttrKind {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        tokens.with(Path::parse).map(Self::Path)
    }
}
