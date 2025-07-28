//! Statements.
//!
//! Statements are components of code, that unlike expressions, do not produce a value when evaluated.

use cryo_lexer::{
    atoms::{Equal, Semi},
    stream::{Guard, StreamLike},
};

use crate::{
    Parse, ParseError,
    expr::Expr,
    ident::{Ident, LET, MUT},
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

/// A variable binding.
#[derive(Debug, PartialEq, Eq)]
pub struct Binding {
    /// The mutability of this variable.
    pub mutability: Option<Mutable>,
    /// The identifier of this variable.
    pub ident: Ident,
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

        let ident = tokens.with(Ident::parse)?;
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
        test_util::assert_parse,
    };

    use super::{Binding, Mutable, Stmt};

    #[test]
    fn parse_immut_binding() {
        assert_parse(
            "let x = 5 + 5;",
            Spanned::new(
                Stmt::Binding(Binding {
                    mutability: None,
                    ident: Ident {
                        sym: Symbol::new("x"),
                        valid: true,
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
                Span::new(0, 14),
            ),
        );
    }

    #[test]
    fn parse_mut_binding() {
        assert_parse(
            "let mut x = 5;",
            Spanned::new(
                Stmt::Binding(Binding {
                    mutability: Some(Mutable),
                    ident: Ident {
                        sym: Symbol::new("x"),
                        valid: true,
                    },
                    expr: Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                        IntegerLiteral::Value(5),
                    ))),
                }),
                Span::new(0, 14),
            ),
        );
    }
}
