//! Function definitions.

use cryo_lexer::{
    atoms::{Colon, LParen, RParen},
    stream::StreamLike,
};

use crate::{
    Parse, ParseError, Punctuated,
    atoms::Comma,
    expr::BlockExpr,
    ident::{FUN, Ident},
    stmt::TypedIdent,
};

use super::Ty;

/// A function definition.
#[derive(Debug, PartialEq, Eq)]
pub struct FnDef {
    /// The name of this function.
    pub ident: Ident,
    /// The arguments that this function requires.
    pub args: Punctuated<TypedIdent, Comma>,
    /// The return type of this function.
    pub ret_ty: Option<Ty>,
    /// The body of this function.
    pub body: BlockExpr,
}

impl Parse for FnDef {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        tokens
            .with(Ident::parse)?
            .require(&FUN)
            .map_err(|_| ParseError::MissingKw(FUN.with(Clone::clone)))?;

        let ident = tokens.with(Ident::parse)?;
        tokens.advance_require::<LParen>()?;
        let args = tokens.with(Punctuated::parse)?;
        tokens.advance_require::<RParen>()?;
        let ret_ty = match tokens.advance_require::<Colon>() {
            Ok(_) => Some(tokens.with(Ty::parse)?),
            Err(_) => None,
        };
        let body = tokens.with(BlockExpr::parse)?;

        Ok(Self {
            ident,
            args,
            ret_ty,
            body,
        })
    }
}

#[cfg(test)]
mod tests {
    use cryo_lexer::Symbol;
    use cryo_span::{Span, Spanned};

    use crate::{
        Punctuated,
        atoms::Comma,
        expr::{BaseExpr, BinaryExpr, BlockExpr, Expr},
        ident::Ident,
        stmt::TypedIdent,
        test_util::assert_parse,
    };

    use super::FnDef;

    #[test]
    fn parse_fn_def() {
        assert_parse(
            "fun add(lhs: int, rhs: int): int { lhs + rhs }",
            Spanned::new(
                FnDef {
                    ident: Ident {
                        sym: Symbol::new("add"),
                        valid: true,
                    },
                    args: Punctuated {
                        inner: vec![(
                            TypedIdent {
                                ident: Ident {
                                    sym: Symbol::new("lhs"),
                                    valid: true,
                                },
                                id_ty: Ident {
                                    sym: Symbol::new("int"),
                                    valid: true,
                                },
                            },
                            Comma,
                        )],
                        last: Some(Box::new(TypedIdent {
                            ident: Ident {
                                sym: Symbol::new("rhs"),
                                valid: true,
                            },
                            id_ty: Ident {
                                sym: Symbol::new("int"),
                                valid: true,
                            },
                        })),
                    },
                    ret_ty: Some(Ident {
                        sym: Symbol::new("int"),
                        valid: true,
                    }),
                    body: BlockExpr {
                        stmts: Box::new([]),
                        tail: Some(Box::new(Expr::BinaryExpr(BinaryExpr {
                            lhs: Box::new(Expr::BaseExpr(BaseExpr::BindingUsage(Ident {
                                sym: Symbol::new("lhs"),
                                valid: true,
                            }))),
                            op: crate::expr::Operator::Add,
                            rhs: Box::new(Expr::BaseExpr(BaseExpr::BindingUsage(Ident {
                                sym: Symbol::new("rhs"),
                                valid: true,
                            }))),
                        }))),
                    },
                },
                Span::new(0, 46),
            ),
        );
    }
}
