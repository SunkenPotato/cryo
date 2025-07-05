use cryo_span::{Span, Spanned};

use crate::{
    expr::{
        Expr, ReducedExpr,
        literal::{IntegerLiteral, Literal, StringLiteral},
        math_expr::{MathExpr, Operator},
    },
    test_util::{TestError, assert_parse, assert_parse_fail, stream},
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
    assert_parse_fail::<IntegerLiteral>(stream, TestError::new(1, 0))
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
