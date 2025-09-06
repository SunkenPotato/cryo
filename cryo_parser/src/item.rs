//! Items.

use std::sync::LazyLock;

use crate::{
    CommaSeparated, IsFail, OneOrMany, Parse, ParseError, ParseErrorKind, Path, TypedIdent,
    ident::{ENUM, Ident, RECORD},
};
use cryo_lexer::{TokenKind, stream::StreamLike};
use cryo_parser_proc_macro::IsFail;
use cryo_span::Spanned;
use internment::Intern;

/// A top-level item, such as a struct.
#[derive(Clone, PartialEq, Eq, Debug, IsFail)]
#[fail(bubble)]
pub enum Item {
    /// A record definition.
    Record(RecordDef),
    /// An enum definition.
    Enum(EnumDef),
}

impl Item {
    const ACCEPTED_KEYWORDS: &[&LazyLock<Intern<str>>] = &[&RECORD];
}

/// A record definition.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RecordDef {
    /// The name, or identifier of this record.
    pub ident: Spanned<Ident>,
    /// The fields of this record.
    pub fields: Spanned<CommaSeparated<TypedIdent>>,
}

impl IsFail for RecordDef {
    fn is_fail(&self) -> bool {
        self.ident.is_fail() && self.fields.is_fail()
    }
}

/// An enum definition.
#[derive(Clone, PartialEq, Eq, Debug, IsFail)]
#[fail = false]
pub struct EnumDef {
    /// The name, or identifier of this record.
    pub ident: Spanned<Ident>,
    /// The variants of this enum.
    pub variants: Spanned<CommaSeparated<EnumVariant>>,
}

/// An enum variant, such as `Err(T)`.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct EnumVariant {
    /// The name, or identifier of this variant.
    pub ident: Spanned<Ident>,
    /// The type of this variant.
    pub ty: Option<Spanned<Path>>,
}

impl IsFail for EnumVariant {
    fn is_fail(&self) -> bool {
        self.ident.is_fail() && self.ty.as_ref().map(|v| v.is_fail()).unwrap_or(true)
    }
}

////////////////////////////////////
// Parsers                        //
////////////////////////////////////

impl Parse for RecordDef {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let rec_kw = tokens.spanning(Ident::parse)?;
        if *rec_kw.t != *RECORD {
            return Err(ParseError::new(
                rec_kw.span,
                rec_kw.span,
                ParseErrorKind::ExpectedKeyword(OneOrMany::Owned(&RECORD)),
            ));
        }

        let ident = tokens.spanning(Ident::parse)?;
        tokens.advance_require(TokenKind::LParen)?;
        let fields = tokens.spanning(CommaSeparated::parse)?;
        tokens.advance_require(TokenKind::RParen)?;
        tokens.advance_require(TokenKind::Semi)?;

        Ok(Self { ident, fields })
    }
}

impl Parse for EnumVariant {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let ident = tokens.spanning(Ident::parse)?;
        let ty = if tokens.advance_require(TokenKind::LParen).is_ok() {
            let ty = tokens.spanning(Path::parse)?;
            tokens.advance_require(TokenKind::RParen)?;
            Some(ty)
        } else {
            None
        };

        Ok(Self { ident, ty })
    }
}

impl Parse for EnumDef {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let enum_kw = tokens.spanning(Ident::parse)?;

        if enum_kw.0 != *ENUM {
            return Err(ParseError {
                span: enum_kw.span,
                context: enum_kw.span,
                kind: ParseErrorKind::ExpectedKeyword(OneOrMany::Owned(&ENUM)),
            });
        }

        let ident = tokens.spanning(Ident::parse)?;
        tokens.advance_require(TokenKind::LCurly)?;
        let variants = tokens.spanning(CommaSeparated::parse)?;
        tokens.advance_require(TokenKind::RCurly)?;

        Ok(Self { ident, variants })
    }
}

impl Parse for Item {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let next = tokens.peek_require(TokenKind::Identifier)?;

        if next.t == *RECORD {
            tokens.with(RecordDef::parse).map(Self::Record)
        } else if next.t == *ENUM {
            tokens.with(EnumDef::parse).map(Self::Enum)
        } else {
            Err(ParseError {
                span: next.span,
                context: next.span,
                kind: ParseErrorKind::ExpectedKeyword(OneOrMany::Multiple(Self::ACCEPTED_KEYWORDS)),
            })
        }
    }
}

////////////////////////////////////
// Tests                          //
////////////////////////////////////

#[cfg(test)]
mod tests {
    use cryo_lexer::{Symbol, atoms::Comma};
    use cryo_span::{Span, Spanned};

    use crate::{
        CommaSeparated, TypedIdent,
        ident::Ident,
        item::{EnumDef, EnumVariant, Item, RecordDef},
        test_util::assert_parse,
    };

    #[test]
    fn parse_record() {
        assert_parse(
            "record Point(x: int, y: int);",
            Spanned::new(
                Item::Record(RecordDef {
                    ident: Spanned::new(Ident(Symbol::from("Point")), Span::new(7, 12)),
                    fields: Spanned::new(
                        CommaSeparated {
                            inner: vec![(
                                Spanned::new(
                                    TypedIdent {
                                        ident: Spanned::new(
                                            Ident(Symbol::from("x")),
                                            Span::new(13, 14),
                                        ),
                                        ty: Spanned::new(
                                            Spanned::new("int", Span::new(16, 19)).into(),
                                            Span::new(16, 19),
                                        ),
                                    },
                                    Span::new(13, 19),
                                ),
                                Spanned::new(Comma, Span::new(19, 20)),
                            )],
                            last: Some(Box::new(Spanned::new(
                                TypedIdent {
                                    ident: Spanned::new(
                                        Ident(Symbol::from("y")),
                                        Span::new(21, 22),
                                    ),
                                    ty: Spanned::new(
                                        Spanned::new("int", Span::new(24, 27)).into(),
                                        Span::new(24, 27),
                                    ),
                                },
                                Span::new(21, 27),
                            ))),
                        },
                        Span::new(13, 27),
                    ),
                }),
                Span::new(0, 29),
            ),
        );
    }

    #[test]
    fn parse_enum_def() {
        assert_parse(
            "enum X { A(i32), B(i32) }",
            Spanned::new(
                Item::Enum(EnumDef {
                    ident: Spanned::new(Ident(Symbol::from("X")), Span::new(5, 6)),
                    variants: Spanned::new(
                        CommaSeparated {
                            inner: vec![(
                                Spanned::new(
                                    EnumVariant {
                                        ident: Spanned::new(
                                            Ident(Symbol::from("A")),
                                            Span::new(9, 10),
                                        ),
                                        ty: Some(Spanned::new(
                                            Spanned::new("i32", Span::new(11, 14)).into(),
                                            Span::new(11, 14),
                                        )),
                                    },
                                    Span::new(9, 15),
                                ),
                                Spanned::new(Comma, Span::new(15, 16)),
                            )],
                            last: Some(Box::new(Spanned::new(
                                EnumVariant {
                                    ident: Spanned::new(
                                        Ident(Symbol::from("B")),
                                        Span::new(17, 18),
                                    ),
                                    ty: Some(Spanned::new(
                                        Spanned::new("i32", Span::new(19, 22)).into(),
                                        Span::new(19, 22),
                                    )),
                                },
                                Span::new(17, 23),
                            ))),
                        },
                        Span::new(9, 23),
                    ),
                }),
                Span::new(0, 25),
            ),
        );
    }
}
