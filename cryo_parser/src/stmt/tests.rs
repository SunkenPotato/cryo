use cryo_span::Span;

use crate::{
    atoms::Semi,
    expr::{
        Expr, ReducedExpr,
        literal::{IntegerLiteral, Literal},
        math_expr::MathExpr,
    },
    stmt::{ExprStmt, Stmt},
    test_util::{assert_parse, stream},
};

#[test]
fn parse_expr_stmt() {
    let input = stream("5 + 5;");

    assert_parse::<Stmt>(
        input,
        cryo_span::Spanned::new(
            Stmt::ExprStmt(ExprStmt {
                expr: Expr::MathExpr(Box::new(MathExpr {
                    lhs: ReducedExpr::Literal(Literal::IntegerLiteral(IntegerLiteral(5))),
                    op: crate::expr::math_expr::Operator::Add,
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
