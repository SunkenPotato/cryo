//! Statements.

use cryo_lexer::{TokenKind, stream::StreamLike};
use cryo_parser_proc_macro::IsFail;
use cryo_span::Spanned;

use crate::{
    IsFail, Parse, ParseError, ParseErrorKind,
    expr::Expr,
    ident::{Ident, LET, MUT},
};

/// A statement.
#[derive(Clone, PartialEq, Eq, Debug, IsFail)]
#[fail = true]
pub enum Stmt {
    /// A binding definition.
    BindingDef(BindingDef),
    /// An expression terminated by a semicolon, such as `print("Hello");`.
    ExprSemi(Expr),
}

/// A mutability marker.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Mutable;

/// A binding definition.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BindingDef {
    /// The identifier.
    pub ident: Spanned<Ident>,
    /// Whether or not this binding is mutable.
    pub mutable: Option<Spanned<Mutable>>,
    /// The value of this binding.
    pub value: Spanned<Expr>,
}

impl IsFail for BindingDef {
    fn is_fail(&self) -> bool {
        self.ident.is_fail() && self.value.is_fail()
    }
}

////////////////////////////////////
// Parsers                        //
////////////////////////////////////

/// Parse an expression followed by a semicolon.
pub fn parse_expr_semi(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Expr> {
    let expr = tokens.with(Expr::parse)?;
    tokens.advance_require(TokenKind::Semi)?;
    Ok(expr)
}

impl Parse for BindingDef {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let let_kw = tokens.spanning(Ident::parse)?;
        if let_kw.0 != *LET {
            return Err(ParseError {
                span: let_kw.span,
                context: let_kw.span,
                kind: ParseErrorKind::ExpectedKeyword(*LET),
            });
        }

        let mut mutable = None;
        let mut_or_ident = tokens.spanning(Ident::parse)?;
        let ident;

        if mut_or_ident.0 == *MUT {
            ident = tokens.spanning(Ident::parse)?;
            mutable = Some(mut_or_ident.map(|_| Mutable));
        } else {
            ident = mut_or_ident;
        }

        tokens.advance_require(TokenKind::Equal)?;
        let value = tokens.spanning(parse_expr_semi)?;

        Ok(Self {
            ident,
            mutable,
            value,
        })
    }
}

impl Parse for Stmt {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        tokens
            .with(BindingDef::parse)
            .map(Self::BindingDef)
            .or_else(|_| tokens.with(parse_expr_semi).map(Self::ExprSemi))
    }
}

////////////////////////////////////
// Tests                          //
////////////////////////////////////

#[cfg(test)]
mod tests {
    use cryo_lexer::Symbol;
    use cryo_span::{Span, Spanned};

    use crate::{
        expr::{
            BaseExpr, Expr,
            literal::{IntegerLiteral, Literal},
        },
        ident::Ident,
        stmt::{BindingDef, Mutable, Stmt},
        test_util::assert_parse,
    };

    #[test]
    fn parse_binding_def() {
        assert_parse(
            "let x = 5;",
            Spanned::new(
                Stmt::BindingDef(BindingDef {
                    ident: Spanned::new(Ident(Symbol::from("x")), Span::new(4, 5)),
                    mutable: None,
                    value: Spanned::new(
                        Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                            IntegerLiteral::Value(5),
                        ))),
                        Span::new(8, 10),
                    ),
                }),
                Span::new(0, 10),
            ),
        );
    }

    #[test]
    fn parse_mut_binding_def() {
        assert_parse(
            "let mut x = 5;",
            Spanned::new(
                Stmt::BindingDef(BindingDef {
                    ident: Spanned::new(Ident(Symbol::from("x")), Span::new(8, 9)),
                    mutable: Some(Spanned::new(Mutable, Span::new(4, 7))),
                    value: Spanned::new(
                        Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                            IntegerLiteral::Value(5),
                        ))),
                        Span::new(12, 14),
                    ),
                }),
                Span::new(0, 14),
            ),
        );
    }
}
