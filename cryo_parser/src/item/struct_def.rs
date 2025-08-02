//! Struct, enum, and union definitions.

use cryo_lexer::{
    atoms::{Comma as CommaToken, LCurly, LParen, RCurly, RParen, Semi},
    stream::{Guard, StreamLike},
};
use cryo_span::Spanned;

use crate::{
    Parse, Punctuated,
    atoms::Comma,
    ident::{ENUM, Ident, STRUCT},
    stmt::TypedIdent,
};

use super::Ty;

/// A structure definition.
#[derive(Debug, PartialEq, Eq)]
pub struct StructDef {
    /// The identifier.
    pub ident: Ident,
    /// The body, or fields of this structure.
    pub body: Spanned<StructBody>,
}

impl Parse for StructDef {
    fn parse(tokens: &mut Guard) -> crate::ParseResult<Self> {
        tokens
            .with(Ident::parse)?
            .require(&STRUCT)
            .map_err(|v| v.sym.map(|_| STRUCT.with(Clone::clone)))?;

        let ident = tokens.with(Ident::parse)?;
        let body = tokens.spanning(StructBody::parse::<true>)?;

        Ok(Self { ident, body })
    }
}

/// A structure body.
#[derive(Debug, PartialEq, Eq)]
pub enum StructBody {
    /// A unit structure, i.e., no fields.
    Unit,
    /// A tuple structure, i.e., nameless fields within `(` and `)`.
    Tuple(Punctuated<Ty, Comma>),
    /// A named field structure.
    Named(Punctuated<TypedIdent, Comma>),
}

impl StructBody {
    fn parse<const REQUIRE_SEMI: bool>(tokens: &mut Guard) -> crate::ParseResult<Self> {
        tokens
            .with(|tokens| {
                tokens
                    .advance_require::<LParen>()
                    .map_err(Into::into)
                    .and_then(|_| tokens.with(Punctuated::parse))
                    .and_then(|v| {
                        tokens.advance_require::<RParen>()?;
                        if REQUIRE_SEMI {
                            tokens.advance_require::<Semi>()?;
                        }

                        Ok(v)
                    })
                    .map(Self::Tuple)
            })
            .or_else(|_| {
                tokens.with(|tokens| {
                    tokens
                        .advance_require::<LCurly>()
                        .map_err(Into::into)
                        .and_then(|_| tokens.with(Punctuated::parse))
                        .and_then(|v| {
                            tokens.advance_require::<RCurly>()?;
                            Ok(v)
                        })
                        .map(Self::Named)
                })
            })
            .or_else(|_| {
                if REQUIRE_SEMI {
                    tokens.advance_require::<Semi>().map(|_| ())
                } else {
                    tokens
                        .peek_require::<CommaToken>()
                        .map(|_| ())
                        .or_else(|_| tokens.peek_require::<LCurly>().map(|_| ()))
                }
                .map(|_| Self::Unit)
            })
            .map_err(Into::into)
    }
}

/// An enum definition.
#[derive(Debug, PartialEq, Eq)]
pub struct EnumDef {
    /// The identifier of this enum.
    pub ident: Ident,
    /// The enum variants.
    pub variants: Spanned<Punctuated<EnumVariant, Comma>>,
}

impl Parse for EnumDef {
    fn parse(tokens: &mut Guard) -> crate::ParseResult<Self> {
        tokens
            .with(Ident::parse)?
            .require(&ENUM)
            .map_err(|v| v.sym.map(|_| ENUM.with(Clone::clone)))?;

        let ident = tokens.with(Ident::parse)?;
        tokens.advance_require::<LCurly>()?;
        let variants = tokens.spanning(Punctuated::parse)?;
        tokens.advance_require::<RCurly>()?;

        Ok(Self { ident, variants })
    }
}

/// An enum variant.
#[derive(Debug, PartialEq, Eq)]
pub struct EnumVariant {
    /// The identifier of this enum.
    pub ident: Ident,
    /// The body of this variant.
    pub body: Spanned<StructBody>,
}

impl Parse for EnumVariant {
    fn parse(tokens: &mut Guard) -> crate::ParseResult<Self> {
        let ident = tokens.with(Ident::parse)?;
        let body = tokens.spanning(StructBody::parse::<false>)?;

        Ok(Self { ident, body })
    }
}

#[cfg(test)]
mod tests {
    use cryo_lexer::Symbol;
    use cryo_span::{Span, Spanned};

    use crate::{
        Punctuated, atoms::Comma, ident::Ident, item::Item, stmt::TypedIdent,
        test_util::assert_parse,
    };

    use super::{EnumDef, EnumVariant, StructBody, StructDef};

    #[test]
    fn parse_unit_struct() {
        assert_parse(
            "struct X;",
            Spanned::new(
                Item::StructDef(StructDef {
                    ident: Ident {
                        sym: Spanned {
                            t: Symbol::new("X"),
                            span: Span::new(7, 8),
                        },
                        valid: true,
                    },
                    body: Spanned::new(StructBody::Unit, Span::new(8, 9)),
                }),
                Span::new(0, 9),
            ),
        );
    }

    #[test]
    fn parse_tuple_struct() {
        assert_parse(
            "struct Vec2(int, int);",
            Spanned::new(
                Item::StructDef(StructDef {
                    ident: Ident {
                        sym: Spanned::new(Symbol::new("Vec2"), Span::new(7, 11)),
                        valid: true,
                    },
                    body: Spanned::new(
                        StructBody::Tuple(Punctuated {
                            inner: vec![(
                                Spanned::new(
                                    Ident {
                                        sym: Spanned::new(Symbol::new("int"), Span::new(12, 15)),
                                        valid: true,
                                    },
                                    Span::new(12, 15),
                                ),
                                Spanned::new(Comma, Span::new(15, 16)),
                            )],
                            last: Some(Box::new(Spanned::new(
                                Ident {
                                    sym: Spanned::new(Symbol::new("int"), Span::new(17, 20)),
                                    valid: true,
                                },
                                Span::new(17, 20),
                            ))),
                        }),
                        Span::new(11, 22),
                    ),
                }),
                Span::new(0, 22),
            ),
        );
    }

    #[test]
    fn parse_named_struct() {
        assert_parse(
            "struct Vec2 { x: int, y: int }",
            Spanned::new(
                Item::StructDef(StructDef {
                    ident: Ident {
                        sym: Spanned::new(Symbol::new("Vec2"), Span::new(7, 11)),
                        valid: true,
                    },
                    body: Spanned::new(
                        StructBody::Named(Punctuated {
                            inner: vec![(
                                Spanned::new(
                                    TypedIdent {
                                        ident: Ident {
                                            sym: Spanned::new(Symbol::new("x"), Span::new(14, 15)),
                                            valid: true,
                                        },
                                        id_ty: Ident {
                                            sym: Spanned::new(
                                                Symbol::new("int"),
                                                Span::new(17, 20),
                                            ),
                                            valid: true,
                                        },
                                    },
                                    Span::new(14, 20),
                                ),
                                Spanned::new(Comma, Span::new(20, 21)),
                            )],
                            last: Some(Box::new(Spanned::new(
                                TypedIdent {
                                    ident: Ident {
                                        sym: Spanned::new(Symbol::new("y"), Span::new(22, 23)),
                                        valid: true,
                                    },
                                    id_ty: Ident {
                                        sym: Spanned::new(Symbol::new("int"), Span::new(25, 28)),
                                        valid: true,
                                    },
                                },
                                Span::new(22, 28),
                            ))),
                        }),
                        Span::new(12, 30),
                    ),
                }),
                Span::new(0, 30),
            ),
        );
    }

    #[test]
    fn parse_enum_def() {
        assert_parse(
            "enum Enum { Unit, Tuple(int, int), Named { x: int, y: int } }",
            Spanned::new(
                Item::EnumDef(EnumDef {
                    ident: Ident {
                        sym: Spanned::new(Symbol::new("Enum"), Span::new(5, 9)),
                        valid: true,
                    },
                    variants: Spanned::new(
                        Punctuated {
                            inner: vec![
                                (
                                    Spanned::new(
                                        EnumVariant {
                                            ident: Ident {
                                                sym: Spanned::new(
                                                    Symbol::new("Unit"),
                                                    Span::new(12, 16),
                                                ),
                                                valid: true,
                                            },
                                            body: Spanned::new(StructBody::Unit, Span::new(16, 16)),
                                        },
                                        Span::new(12, 16),
                                    ),
                                    Spanned::new(Comma, Span::new(16, 17)),
                                ),
                                (
                                    Spanned::new(
                                        EnumVariant {
                                            ident: Ident {
                                                sym: Spanned::new(
                                                    Symbol::new("Tuple"),
                                                    Span::new(18, 23),
                                                ),
                                                valid: true,
                                            },
                                            body: Spanned::new(
                                                StructBody::Tuple(Punctuated {
                                                    inner: vec![(
                                                        Spanned::new(
                                                            Ident {
                                                                sym: Spanned::new(
                                                                    Symbol::new("int"),
                                                                    Span::new(24, 27),
                                                                ),
                                                                valid: true,
                                                            },
                                                            Span::new(24, 27),
                                                        ),
                                                        Spanned::new(Comma, Span::new(27, 28)),
                                                    )],
                                                    last: Some(Box::new(Spanned::new(
                                                        Ident {
                                                            sym: Spanned::new(
                                                                Symbol::new("int"),
                                                                Span::new(29, 32),
                                                            ),
                                                            valid: true,
                                                        },
                                                        Span::new(29, 32),
                                                    ))),
                                                }),
                                                Span::new(23, 33),
                                            ),
                                        },
                                        Span::new(18, 33),
                                    ),
                                    Spanned::new(Comma, Span::new(33, 34)),
                                ),
                            ],
                            last: Some(Box::new(Spanned::new(
                                EnumVariant {
                                    ident: Ident {
                                        sym: Spanned::new(Symbol::new("Named"), Span::new(35, 40)),
                                        valid: true,
                                    },
                                    body: Spanned::new(
                                        StructBody::Named(Punctuated {
                                            inner: vec![(
                                                Spanned::new(
                                                    TypedIdent {
                                                        ident: Ident {
                                                            sym: Spanned::new(
                                                                Symbol::new("x"),
                                                                Span::new(43, 44),
                                                            ),
                                                            valid: true,
                                                        },
                                                        id_ty: Ident {
                                                            sym: Spanned::new(
                                                                Symbol::new("int"),
                                                                Span::new(46, 49),
                                                            ),
                                                            valid: true,
                                                        },
                                                    },
                                                    Span::new(43, 49),
                                                ),
                                                Spanned::new(Comma, Span::new(49, 50)),
                                            )],
                                            last: Some(Box::new(Spanned::new(
                                                TypedIdent {
                                                    ident: Ident {
                                                        sym: Spanned::new(
                                                            Symbol::new("y"),
                                                            Span::new(51, 52),
                                                        ),
                                                        valid: true,
                                                    },
                                                    id_ty: Ident {
                                                        sym: Spanned::new(
                                                            Symbol::new("int"),
                                                            Span::new(54, 57),
                                                        ),
                                                        valid: true,
                                                    },
                                                },
                                                Span::new(51, 57),
                                            ))),
                                        }),
                                        Span::new(41, 59),
                                    ),
                                },
                                Span::new(35, 59),
                            ))),
                        },
                        Span::new(12, 59),
                    ),
                }),
                Span::new(0, 61),
            ),
        );
    }
}
