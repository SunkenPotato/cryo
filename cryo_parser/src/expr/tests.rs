use cryo_span::{Span, Spanned};
use internment::Intern;

use crate::{
    atoms::{Assign, LCurly, Let, RCurly, Semi},
    error::MetaError,
    expr::{
        Expr, ReducedExpr,
        binding_ref::BindingRef,
        block::Block,
        literal::{IntegerLiteral, Literal, StringLiteral},
        math_expr::{MathExpr, Operator},
    },
    stmt::{Stmt, binding::Binding},
    test_util::{assert_parse, assert_parse_fail, stream},
};

#[test]
fn parse_int_lit() {
    let stream = stream("-123_456_7");

    assert_parse::<IntegerLiteral>(
        stream,
        Spanned::new(IntegerLiteral(-1234567), Span::new(0, 10)),
    );
}

#[test]
fn do_not_parse_int_overflow() {
    let stream = stream("2147483648");
    assert_parse_fail::<IntegerLiteral>(stream, MetaError::new(1, 0))
}

#[test]
fn parse_str_lit() {
    let stream = stream("\"Hello, \\\"world\\\"\"");
    assert_parse::<StringLiteral>(
        stream,
        Spanned::new(
            StringLiteral(String::from("Hello, \"world\"")),
            Span::new(0, 18),
        ),
    );
}

#[test]
fn parse_op() {
    let stream = stream("+");
    assert_parse::<Operator>(stream, Spanned::new(Operator::Add, Span::new(0, 1)))
}

#[test]
fn parse_math_expr() {
    let stream = stream("5 + 4");
    assert_parse::<MathExpr>(
        stream,
        Spanned::new(
            MathExpr {
                lhs: ReducedExpr::Literal(Literal::IntegerLiteral(IntegerLiteral(5))),
                op: super::math_expr::Operator::Add,
                rhs: Expr::ReducedExpr(ReducedExpr::Literal(Literal::IntegerLiteral(
                    IntegerLiteral(4),
                ))),
            },
            Span::new(0, 5),
        ),
    );
}

#[test]
fn parse_binding_ref() {
    let stream = stream("binding");

    assert_parse::<BindingRef>(
        stream,
        Spanned::new(BindingRef(Intern::from("binding")), Span::new(0, 7)),
    );
}

#[test]
fn parse_block_expr_only_tail_expr() {
    let stream = stream("{ 5 }");

    assert_parse::<Block>(
        stream,
        Spanned::new(
            Block {
                l_curly: LCurly,
                stmts: vec![],
                tail: Some(Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                    Literal::IntegerLiteral(IntegerLiteral(5)),
                )))),
                r_curly: RCurly,
            },
            Span::new(0, 5),
        ),
    );
}

#[test]
fn parse_block_expr_only_stmts() {
    let stream = stream("{ let x = 5; }");

    assert_parse::<Block>(
        stream,
        Spanned::new(
            Block {
                l_curly: LCurly,
                stmts: vec![Stmt::Binding(Binding {
                    let_kw: Let,
                    mut_kw: None,
                    ident: BindingRef(Intern::from("x")),
                    assign: Assign,
                    rhs: Expr::ReducedExpr(ReducedExpr::Literal(Literal::IntegerLiteral(
                        IntegerLiteral(5),
                    ))),
                    semi: Semi,
                })],
                tail: None,
                r_curly: RCurly,
            },
            Span::new(0, 14),
        ),
    )
}

#[test]
fn parse_block_expr_stmts_tail_expr() {
    let tokens = stream("{ let x = 5; x }");

    assert_parse::<Block>(
        tokens,
        Spanned::new(
            Block {
                l_curly: LCurly,
                stmts: vec![Stmt::Binding(Binding {
                    let_kw: Let,
                    mut_kw: None,
                    ident: BindingRef(Intern::from("x")),
                    assign: Assign,
                    rhs: Expr::ReducedExpr(ReducedExpr::Literal(Literal::IntegerLiteral(
                        IntegerLiteral(5),
                    ))),
                    semi: Semi,
                })],
                tail: Some(Box::new(Expr::ReducedExpr(ReducedExpr::BindingRef(
                    BindingRef(Intern::from("x")),
                )))),
                r_curly: RCurly,
            },
            Span::new(0, 16),
        ),
    );
}

#[test]
fn parse_empty_block_expr() {
    let stream = stream("{}");

    assert_parse::<Block>(
        stream,
        Spanned::new(
            Block {
                l_curly: LCurly,
                stmts: vec![],
                tail: None,
                r_curly: RCurly,
            },
            Span::new(0, 2),
        ),
    );
}
