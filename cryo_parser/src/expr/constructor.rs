//! Structure, Enum, and union constructors.

use cryo_lexer::{
    atoms::{Colon, LCurly, RCurly},
    stream::StreamLike,
};
use cryo_span::Spanned;

use crate::{Parse, Punctuated, atoms::Comma, ident::Ident, path::Path};

use super::Expr;

/// A struct constructor for `StructBody::Named` structs.
///
/// Other struct types (`Tuple` and `Unit`) are resolved by the name resolver, not the parser, since they are ambiguous to function calls and simple binding usages.
#[derive(Debug, PartialEq, Eq)]
pub struct StructConstructor {
    /// The name of the struct to be constructed.
    pub ident: Path,
    /// The fields.
    pub fields: Spanned<Punctuated<StructField, Comma>>,
}

impl Parse for StructConstructor {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let ident = tokens.with(Path::parse)?;
        tokens.advance_require::<LCurly>()?;
        let fields = tokens.spanning(Punctuated::parse)?;
        tokens.advance_require::<RCurly>()?;

        Ok(Self { ident, fields })
    }
}

/// A struct constructor field.
#[derive(Debug, PartialEq, Eq)]
pub struct StructField {
    /// The name of the field.
    pub ident: Ident,
    /// The associated value.
    pub value: Spanned<Expr>,
}

impl Parse for StructField {
    // TODO: add support for shorthand initialisation, i.e., where the value can be omitted if a binding exists with the same name as the struct field (e.g., see tail of this function)
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let ident = tokens.with(Ident::parse)?;
        tokens.advance_require::<Colon>()?;
        let value = tokens.spanning(Expr::parse)?;

        Ok(Self { ident, value })
    }
}

#[cfg(test)]
mod tests {
    use cryo_lexer::Symbol;
    use cryo_span::{Span, Spanned};

    use crate::{
        Punctuated,
        atoms::Comma,
        expr::{
            BaseExpr, Expr,
            literal::{IntegerLiteral, Literal},
        },
        ident::Ident,
        test_util::assert_parse,
    };

    use super::{StructConstructor, StructField};

    #[test]
    fn parse_struct_constructor() {
        assert_parse(
            "Vec2 { x: 0, y: 0 }",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::StructConstructor(StructConstructor {
                    ident: Ident {
                        sym: Spanned::new(Symbol::new("Vec2"), Span::new(0, 4)),
                        valid: true,
                    }
                    .into(),
                    fields: Spanned::new(
                        Punctuated {
                            inner: vec![(
                                Spanned::new(
                                    StructField {
                                        ident: Ident {
                                            sym: Spanned::new(Symbol::new("x"), Span::new(7, 8)),
                                            valid: true,
                                        },
                                        value: Spanned::new(
                                            Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                                Spanned::new(
                                                    IntegerLiteral::Value(0),
                                                    Span::new(10, 11),
                                                ),
                                            ))),
                                            Span::new(10, 11),
                                        ),
                                    },
                                    Span::new(7, 11),
                                ),
                                Spanned::new(Comma, Span::new(11, 12)),
                            )],
                            last: Some(Box::new(Spanned::new(
                                StructField {
                                    ident: Ident {
                                        sym: Spanned::new(Symbol::new("y"), Span::new(13, 14)),
                                        valid: true,
                                    },
                                    value: Spanned::new(
                                        Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                            Spanned::new(
                                                IntegerLiteral::Value(0),
                                                Span::new(16, 17),
                                            ),
                                        ))),
                                        Span::new(16, 17),
                                    ),
                                },
                                Span::new(13, 17),
                            ))),
                        },
                        Span::new(7, 17),
                    ),
                })),
                Span::new(0, 19),
            ),
        );
    }
}
