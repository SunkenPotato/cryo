use cryo_span::{Span, Spanned};
use internment::Intern;

use crate::{
    atoms::{Assign, Let, Mut, Semi},
    expr::{
        Expr, ReducedExpr,
        binary_expr::MathExpr,
        literal::{IntegerLiteral, Literal},
    },
    ident::Ident,
    stmt::{ExprStmt, Stmt, binding::Binding},
    test_util::{assert_parse, stream},
};

#[test]
fn parse_expr_stmt() {
    let input = stream("5 + 5;");

    assert_parse::<Stmt>(
        input,
        Spanned::new(
            Stmt::ExprStmt(ExprStmt {
                expr: Expr::MathExpr(Box::new(MathExpr {
                    lhs: ReducedExpr::Literal(Literal::IntegerLiteral(IntegerLiteral(5))),
                    op: crate::expr::binary_expr::Operator::Add,
                    rhs: Expr::ReducedExpr(ReducedExpr::Literal(Literal::IntegerLiteral(
                        IntegerLiteral(5),
                    ))),
                })),
                semi: Semi,
            }),
            Span::new(0, 6),
        ),
    );
}

#[test]
fn parse_immutable_binding() {
    let input = stream("let x = 5;");

    assert_parse::<Binding>(
        input,
        Spanned::new(
            Binding {
                let_kw: Let,
                mut_kw: None,
                ident: Ident(Intern::from("x")),
                assign: Assign,
                rhs: Expr::ReducedExpr(ReducedExpr::Literal(Literal::IntegerLiteral(
                    IntegerLiteral(5),
                ))),
                semi: Semi,
            },
            Span::new(0, 10),
        ),
    )
}

#[test]
fn parse_mutable_binding() {
    let input = stream("let mut x = 7;");

    assert_parse::<Binding>(
        input,
        Spanned::new(
            Binding {
                let_kw: Let,
                mut_kw: Some(Mut),
                ident: Ident(Intern::from("x")),
                assign: Assign,
                rhs: Expr::ReducedExpr(ReducedExpr::Literal(Literal::IntegerLiteral(
                    IntegerLiteral(7),
                ))),
                semi: Semi,
            },
            Span::new(0, 14),
        ),
    )
}
