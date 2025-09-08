//! Items.

use std::sync::LazyLock;

use crate::{
    CommaSeparated, IsFail, OneOrMany, Parse, ParseError, ParseErrorKind, Path, TypedIdent,
    expr::{BlockExpr, Expr},
    ident::{CONST, ENUM, FUNC, Ident, MOD, RECORD, STATIC},
};
use cryo_lexer::{TokenKind, stream::StreamLike};
use cryo_parser_proc_macro::IsFail;
use cryo_span::Spanned;
use internment::Intern;

/// A top-level item, such as a record.
#[derive(Clone, PartialEq, Eq, Debug, IsFail)]
#[fail(bubble)]
pub enum Item {
    /// A record definition.
    Record(RecordDef),
    /// An enum definition.
    Enum(EnumDef),
    /// A function definition.
    FuncDef(FuncDef),
    /// A static definition.
    StaticDef(StaticDef),
    /// A module definition.
    Module(Module),
}

impl Item {
    const ACCEPTED_KEYWORDS: &[&LazyLock<Intern<str>>] = &[&RECORD, &ENUM, &FUNC, &STATIC, &MOD];
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

/// Whether or not a function is callable from const contexts.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Const;

// fn #name(#(#args),*) #(is_const)? #(-> #ret_ty)? #body
/// A function definition.
#[derive(Clone, PartialEq, Eq, Debug, IsFail)]
#[fail = false]
pub struct FuncDef {
    /// The identifier.
    pub ident: Spanned<Ident>,
    /// The arguments to this function.
    pub args: Spanned<CommaSeparated<TypedIdent>>,
    /// Whether this function can be called from a const context or not.
    pub is_const: Option<Spanned<Const>>,
    /// The return type.
    pub ret_ty: Option<Spanned<Path>>,
    /// The body.
    pub body: Spanned<BlockExpr>,
}

// static #ident: #ty = #value;
/// A static variable definition.
#[derive(Clone, PartialEq, Eq, Debug, IsFail)]
#[fail = false]
pub struct StaticDef {
    /// The name of this static.
    pub ident: Spanned<TypedIdent>,
    /// The value.
    pub value: Spanned<Expr>,
}

/// A module.
#[derive(Clone, PartialEq, Eq, Debug, IsFail)]
#[fail = false]
pub struct Module {
    /// The identifier.
    pub ident: Spanned<Ident>,
    /// The contained items.
    pub items: OutlineModule,
}

/// An outlined module, such as the one defined in your main source file.
pub type OutlineModule = Vec<Spanned<Item>>;

#[rustfmt::skip]
impl IsFail for OutlineModule { fn is_fail(&self) -> bool { false } }

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

impl Parse for FuncDef {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let func_kw = tokens.advance_require(TokenKind::Identifier)?;
        if *func_kw != *FUNC {
            return Err(ParseError {
                span: func_kw.span,
                context: func_kw.span,
                kind: ParseErrorKind::ExpectedKeyword(OneOrMany::Owned(&FUNC)),
            });
        }

        let ident = tokens.spanning(Ident::parse)?;
        tokens.advance_require(TokenKind::LParen)?;
        let args = tokens.spanning(CommaSeparated::parse)?;
        tokens.advance_require(TokenKind::RParen)?;
        let mut is_const = None;

        if let Ok(token) = tokens.spanning(Ident::parse) {
            if token.t.0 != *CONST {
                return Err(ParseError {
                    span: token.span,
                    context: token.span,
                    kind: ParseErrorKind::ExpectedKeyword(OneOrMany::Owned(&CONST)),
                });
            }

            is_const.replace(token.map(|_| Const));
        }

        let mut ret_ty = None;

        if tokens.advance_require(TokenKind::Colon).is_ok() {
            ret_ty.replace(tokens.spanning(Path::parse)?);
        }

        let body = tokens.spanning(BlockExpr::parse)?;

        Ok(Self {
            ident,
            args,
            is_const,
            ret_ty,
            body,
        })
    }
}

impl Parse for StaticDef {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let static_kw = tokens.advance_require(TokenKind::Identifier)?;

        if *static_kw != *STATIC {
            return Err(ParseError {
                span: static_kw.span,
                context: static_kw.span,
                kind: ParseErrorKind::ExpectedKeyword(OneOrMany::Owned(&STATIC)),
            });
        }

        let ident = tokens.spanning(TypedIdent::parse)?;
        tokens.advance_require(TokenKind::Equal)?;

        let value = tokens.spanning(Expr::parse)?;
        tokens.advance_require(TokenKind::Semi)?;

        Ok(Self { ident, value })
    }
}

impl Parse for Module {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let mod_kw = tokens.advance_require(TokenKind::Identifier)?;

        if *mod_kw != *MOD {
            return Err(ParseError {
                span: mod_kw.span,
                context: mod_kw.span,
                kind: ParseErrorKind::ExpectedKeyword(OneOrMany::Owned(&MOD)),
            });
        }

        let ident = tokens.spanning(Ident::parse)?;

        tokens.advance_require(TokenKind::LCurly)?;
        let items = tokens.with(OutlineModule::parse)?;
        tokens.advance_require(TokenKind::RCurly)?;

        Ok(Self { ident, items })
    }
}

impl Parse for OutlineModule {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let mut buf = Self::new();

        while let Ok(item) = tokens.spanning(Item::parse) {
            buf.push(item);
        }

        Ok(buf)
    }
}

impl Parse for Item {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let next = tokens.peek_require(TokenKind::Identifier)?;

        if next.t == *RECORD {
            tokens.with(RecordDef::parse).map(Self::Record)
        } else if next.t == *ENUM {
            tokens.with(EnumDef::parse).map(Self::Enum)
        } else if next.t == *FUNC {
            tokens.with(FuncDef::parse).map(Self::FuncDef)
        } else if next.t == *STATIC {
            tokens.with(StaticDef::parse).map(Self::StaticDef)
        } else if next.t == *MOD {
            tokens.with(Module::parse).map(Self::Module)
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
        expr::{
            BaseExpr, BinaryExpr, BinaryOp, BlockExpr, Expr,
            literal::{Literal, StringLiteral},
        },
        ident::Ident,
        item::{Const, EnumDef, EnumVariant, FuncDef, Item, Module, RecordDef, StaticDef},
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

    #[test]
    fn parse_fn_def() {
        assert_parse(
            "func add(lhs: int, rhs: int) const: int { lhs + rhs }",
            Spanned::new(
                Item::FuncDef(FuncDef {
                    ident: Spanned::new(Ident(Symbol::from("add")), Span::new(5, 8)),
                    args: Spanned::new(
                        CommaSeparated {
                            inner: vec![(
                                Spanned::new(
                                    TypedIdent {
                                        ident: Spanned::new(
                                            Ident(Symbol::from("lhs")),
                                            Span::new(9, 12),
                                        ),
                                        ty: Spanned::new(
                                            Spanned::new("int", Span::new(14, 17)).into(),
                                            Span::new(14, 17),
                                        ),
                                    },
                                    Span::new(9, 17),
                                ),
                                Spanned::new(Comma, Span::new(17, 18)),
                            )],
                            last: Some(Box::new(Spanned::new(
                                TypedIdent {
                                    ident: Spanned::new(
                                        Ident(Symbol::from("rhs")),
                                        Span::new(19, 22),
                                    ),
                                    ty: Spanned::new(
                                        Spanned::new("int", Span::new(24, 27)).into(),
                                        Span::new(24, 27),
                                    ),
                                },
                                Span::new(19, 27),
                            ))),
                        },
                        Span::new(9, 27),
                    ),
                    is_const: Some(Spanned::new(Const, Span::new(29, 34))),
                    ret_ty: Some(Spanned::new(
                        Spanned::new("int", Span::new(36, 39)).into(),
                        Span::new(36, 39),
                    )),
                    body: Spanned::new(
                        BlockExpr {
                            stmts: vec![],
                            tail: Some(Box::new(Spanned::new(
                                Expr::BinaryExpr(BinaryExpr {
                                    lhs: Box::new(Spanned::new(
                                        Expr::BaseExpr(BaseExpr::Path(
                                            Spanned::new("lhs", Span::new(42, 45)).into(),
                                        )),
                                        Span::new(42, 45),
                                    )),
                                    op: Spanned::new(BinaryOp::Add, Span::new(46, 47)),
                                    rhs: Box::new(Spanned::new(
                                        Expr::BaseExpr(BaseExpr::Path(
                                            Spanned::new("rhs", Span::new(48, 51)).into(),
                                        )),
                                        Span::new(48, 51),
                                    )),
                                }),
                                Span::new(42, 51),
                            ))),
                        },
                        Span::new(40, 53),
                    ),
                }),
                Span::new(0, 53),
            ),
        );
    }

    #[test]
    fn parse_static_def() {
        assert_parse(
            "static VERSION: str = \"1.0.0\";",
            Spanned::new(
                Item::StaticDef(StaticDef {
                    ident: Spanned::new(
                        TypedIdent {
                            ident: Spanned::new(Ident(Symbol::from("VERSION")), Span::new(7, 14)),
                            ty: Spanned::new(
                                Spanned::new("str", Span::new(16, 19)).into(),
                                Span::new(16, 19),
                            ),
                        },
                        Span::new(7, 19),
                    ),
                    value: Spanned::new(
                        Expr::BaseExpr(BaseExpr::Literal(Literal::StringLiteral(
                            StringLiteral::Value(Symbol::from("1.0.0")),
                        ))),
                        Span::new(22, 29),
                    ),
                }),
                Span::new(0, 30),
            ),
        );
    }

    #[test]
    fn parse_mod() {
        assert_parse(
            "mod greet { func add(lhs: int, rhs: int) const: int { lhs + rhs } }",
            Spanned::new(
                Item::Module(Module {
                    ident: Spanned::new(Ident(Symbol::from("greet")), Span::new(4, 9)),
                    items: vec![Spanned::new(
                        Item::FuncDef(FuncDef {
                            ident: Spanned::new(
                                Ident(Symbol::from("add")),
                                Span::new(5 + 12, 8 + 12),
                            ),
                            args: Spanned::new(
                                CommaSeparated {
                                    inner: vec![(
                                        Spanned::new(
                                            TypedIdent {
                                                ident: Spanned::new(
                                                    Ident(Symbol::from("lhs")),
                                                    Span::new(9 + 12, 12 + 12),
                                                ),
                                                ty: Spanned::new(
                                                    Spanned::new(
                                                        "int",
                                                        Span::new(14 + 12, 17 + 12),
                                                    )
                                                    .into(),
                                                    Span::new(14 + 12, 17 + 12),
                                                ),
                                            },
                                            Span::new(9 + 12, 17 + 12),
                                        ),
                                        Spanned::new(Comma, Span::new(17 + 12, 18 + 12)),
                                    )],
                                    last: Some(Box::new(Spanned::new(
                                        TypedIdent {
                                            ident: Spanned::new(
                                                Ident(Symbol::from("rhs")),
                                                Span::new(19 + 12, 22 + 12),
                                            ),
                                            ty: Spanned::new(
                                                Spanned::new("int", Span::new(24 + 12, 27 + 12))
                                                    .into(),
                                                Span::new(24 + 12, 27 + 12),
                                            ),
                                        },
                                        Span::new(19 + 12, 27 + 12),
                                    ))),
                                },
                                Span::new(9 + 12, 27 + 12),
                            ),
                            is_const: Some(Spanned::new(Const, Span::new(29 + 12, 34 + 12))),
                            ret_ty: Some(Spanned::new(
                                Spanned::new("int", Span::new(36 + 12, 39 + 12)).into(),
                                Span::new(36 + 12, 39 + 12),
                            )),
                            body: Spanned::new(
                                BlockExpr {
                                    stmts: vec![],
                                    tail: Some(Box::new(Spanned::new(
                                        Expr::BinaryExpr(BinaryExpr {
                                            lhs: Box::new(Spanned::new(
                                                Expr::BaseExpr(BaseExpr::Path(
                                                    Spanned::new(
                                                        "lhs",
                                                        Span::new(42 + 12, 45 + 12),
                                                    )
                                                    .into(),
                                                )),
                                                Span::new(42 + 12, 45 + 12),
                                            )),
                                            op: Spanned::new(
                                                BinaryOp::Add,
                                                Span::new(46 + 12, 47 + 12),
                                            ),
                                            rhs: Box::new(Spanned::new(
                                                Expr::BaseExpr(BaseExpr::Path(
                                                    Spanned::new(
                                                        "rhs",
                                                        Span::new(48 + 12, 51 + 12),
                                                    )
                                                    .into(),
                                                )),
                                                Span::new(48 + 12, 51 + 12),
                                            )),
                                        }),
                                        Span::new(42 + 12, 51 + 12),
                                    ))),
                                },
                                Span::new(40 + 12, 53 + 12),
                            ),
                        }),
                        Span::new(12, 53 + 12),
                    )],
                }),
                Span::new(0, 67),
            ),
        );
    }
}
