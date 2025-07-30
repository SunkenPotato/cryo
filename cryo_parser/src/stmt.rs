//! Statements.
//!
//! Statements are components of code, that unlike expressions, do not produce a value when evaluated.

use cryo_lexer::{
    atoms::{Colon, Equal, Semi},
    stream::{Guard, StreamLike},
};

use crate::{
    Parse, ParseError,
    expr::Expr,
    ident::{Ident, LET, MUT},
    item::Ty,
};

/// A statement.
#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    /// An expression followed by a semicolon.
    ExprSemi(Expr),
    /// A variable binding.
    Binding(Binding),
}

fn parse_expr_semi(tokens: &mut Guard) -> crate::ParseResult<Expr> {
    let expr = Expr::parse(tokens)?;
    tokens.advance_require::<Semi>()?;

    Ok(expr)
}

impl Parse for Stmt {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        tokens
            .with(parse_expr_semi)
            .map(Self::ExprSemi)
            .or_else(|_| tokens.with(Binding::parse).map(Self::Binding))
    }
}

/// A typed identifier, such as `x: int`.
#[derive(Debug, PartialEq, Eq)]
pub struct TypedIdent {
    /// The name of the identifier.
    pub ident: Ident,
    /// The type of this identifier
    // TODO: replace with Ty once it becomes available
    pub id_ty: Ty,
}

impl Parse for TypedIdent {
    fn parse(tokens: &mut Guard) -> crate::ParseResult<Self> {
        let ident = tokens.with(Ident::parse)?;
        tokens.advance_require::<Colon>()?;
        let id_ty = tokens.with(Ty::parse)?;

        Ok(Self { ident, id_ty })
    }
}

/// A variable binding.
#[derive(Debug, PartialEq, Eq)]
pub struct Binding {
    /// The mutability of this variable.
    pub mutability: Option<Mutable>,
    /// The identifier of this variable.
    pub ident: TypedIdent,
    /// The expression of this binding.
    pub expr: Expr,
}

/// Mutability indicator.
#[derive(Debug, PartialEq, Eq)]
pub struct Mutable;

impl Parse for Binding {
    fn parse(tokens: &mut Guard) -> crate::ParseResult<Self> {
        tokens
            .with(Ident::parse)?
            .require(&LET)
            .map_err(|_| ParseError::MissingKw(LET.with(Clone::clone)))?;

        let mutability = match tokens.with(|tokens| {
            let ident = Ident::parse(tokens).map_err(Err)?;
            ident.require(&MUT).map_err(Ok)?;
            Ok(Mutable)
        }) {
            Ok(v) => Some(v),
            Err(Ok(_)) => None,
            Err(Err(e)) => return Err(e),
        };

        let ident = tokens.with(TypedIdent::parse)?;
        tokens.advance_require::<Equal>()?;
        let expr = tokens.with(parse_expr_semi)?;
        Ok(Binding {
            mutability,
            ident,
            expr,
        })
    }
}

#[cfg(test)]
mod tests {
    use cryo_lexer::Symbol;
    use cryo_span::{Span, Spanned};

    use crate::{
        expr::{
            BaseExpr, BinaryExpr, Expr, Operator,
            literal::{IntegerLiteral, Literal},
        },
        ident::Ident,
        item::Ty,
        test_util::assert_parse,
    };

    use super::{Binding, Mutable, Stmt, TypedIdent};

    #[test]
    fn parse_immut_binding() {
        assert_parse(
            "let x: int = 5 + 5;",
            Spanned::new(
                Stmt::Binding(Binding {
                    mutability: None,
                    ident: TypedIdent {
                        ident: Ident {
                            sym: Symbol::new("x"),
                            valid: true,
                        },
                        id_ty: Ty {
                            sym: Symbol::new("int"),
                            valid: true,
                        },
                    },
                    expr: Expr::BinaryExpr(BinaryExpr {
                        lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(5),
                        )))),
                        op: Operator::Add,
                        rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(5),
                        )))),
                    }),
                }),
                Span::new(0, 19),
            ),
        );
    }

    #[test]
    fn parse_mut_binding() {
        assert_parse(
            "let mut x: int = 5;",
            Spanned::new(
                Stmt::Binding(Binding {
                    mutability: Some(Mutable),
                    ident: TypedIdent {
                        ident: Ident {
                            sym: Symbol::new("x"),
                            valid: true,
                        },
                        id_ty: Ty {
                            sym: Symbol::new("int"),
                            valid: true,
                        },
                    },
                    expr: Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                        IntegerLiteral::Value(5),
                    ))),
                }),
                Span::new(0, 19),
            ),
        );
    }
}
