use cryo_span::{Span, Spanned};
use internment::Intern;

use crate::{
    atoms::{Assign, Colon, Comma, Else, If, LCurly, Let, RCurly, Semi},
    error::MetaError,
    expr::{
        Expr, ReducedExpr,
        binary_expr::{MathExpr, Operator},
        block::Block,
        cond_expr::{ElseIfBlock, IfBlock, IfExpr},
        literal::{IntegerLiteral, Literal, StringLiteral},
        struct_expr::{NamedExpr, StructExpr, StructExprBody},
    },
    ident::Ident,
    parser::Punct,
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
                op: super::binary_expr::Operator::Add,
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

    assert_parse::<Ident>(
        stream,
        Spanned::new(Ident(Intern::from("binding")), Span::new(0, 7)),
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
                    ident: Ident(Intern::from("x")),
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
                    ident: Ident(Intern::from("x")),
                    assign: Assign,
                    rhs: Expr::ReducedExpr(ReducedExpr::Literal(Literal::IntegerLiteral(
                        IntegerLiteral(5),
                    ))),
                    semi: Semi,
                })],
                tail: Some(Box::new(Expr::ReducedExpr(ReducedExpr::BindingRef(Ident(
                    Intern::from("x"),
                ))))),
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

#[test]
fn parse_if_block() {
    let stream = stream("if 5 { 7 }");

    assert_parse::<IfExpr>(
        stream,
        Spanned::new(
            IfExpr {
                if_block: IfBlock {
                    if_kw: If,
                    condition: Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                        Literal::IntegerLiteral(IntegerLiteral(5)),
                    ))),
                    if_block: Block {
                        l_curly: LCurly,
                        stmts: vec![],
                        tail: Some(Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                            Literal::IntegerLiteral(IntegerLiteral(7)),
                        )))),
                        r_curly: RCurly,
                    },
                },
                else_if_blocks: vec![],
                else_block: None,
            },
            Span::new(0, 10),
        ),
    )
}

#[test]
fn parse_if_with_else_if() {
    let stream = stream("if 5 { 7 } else if 9 { 8 }");

    assert_parse::<IfExpr>(
        stream,
        Spanned::new(
            IfExpr {
                if_block: IfBlock {
                    if_kw: If,
                    condition: Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                        Literal::IntegerLiteral(IntegerLiteral(5)),
                    ))),
                    if_block: Block {
                        l_curly: LCurly,
                        stmts: vec![],
                        tail: Some(Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                            Literal::IntegerLiteral(IntegerLiteral(7)),
                        )))),
                        r_curly: RCurly,
                    },
                },
                else_if_blocks: vec![ElseIfBlock {
                    else_kw: Else,
                    if_block: IfBlock {
                        if_kw: If,
                        condition: Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                            Literal::IntegerLiteral(IntegerLiteral(9)),
                        ))),
                        if_block: Block {
                            l_curly: LCurly,
                            stmts: vec![],
                            tail: Some(Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                                Literal::IntegerLiteral(IntegerLiteral(8)),
                            )))),
                            r_curly: RCurly,
                        },
                    },
                }],
                else_block: None,
            },
            Span::new(0, 26),
        ),
    );
}

#[test]
fn parse_if_with_else_block() {
    let stream = stream("if 5 { 7 } else { 2 }");

    assert_parse::<IfExpr>(
        stream,
        Spanned::new(
            IfExpr {
                if_block: IfBlock {
                    if_kw: If,
                    condition: Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                        Literal::IntegerLiteral(IntegerLiteral(5)),
                    ))),
                    if_block: Block {
                        l_curly: LCurly,
                        stmts: vec![],
                        tail: Some(Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                            Literal::IntegerLiteral(IntegerLiteral(7)),
                        )))),
                        r_curly: RCurly,
                    },
                },
                else_if_blocks: vec![],
                else_block: Some(crate::expr::cond_expr::ElseBlock {
                    else_kw: Else,
                    block: Block {
                        l_curly: LCurly,
                        stmts: vec![],
                        tail: Some(Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                            Literal::IntegerLiteral(IntegerLiteral(2)),
                        )))),
                        r_curly: RCurly,
                    },
                }),
            },
            Span::new(0, 21),
        ),
    );
}

#[test]
fn parse_if_with_else_if_and_else_block() {
    let stream =
        stream("if 1 { 2 } else if 3 { let x = 4; x } else if 5 { let y = 6; y + 0 } else { 7 }");

    assert_parse::<IfExpr>(
        stream,
        Spanned::new(
            IfExpr {
                if_block: IfBlock {
                    if_kw: If,
                    condition: Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                        Literal::IntegerLiteral(IntegerLiteral(1)),
                    ))),
                    if_block: Block {
                        l_curly: LCurly,
                        stmts: vec![],
                        tail: Some(Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                            Literal::IntegerLiteral(IntegerLiteral(2)),
                        )))),
                        r_curly: RCurly,
                    },
                },
                else_if_blocks: vec![
                    ElseIfBlock {
                        else_kw: Else,
                        if_block: IfBlock {
                            if_kw: If,
                            condition: Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                                Literal::IntegerLiteral(IntegerLiteral(3)),
                            ))),
                            if_block: Block {
                                l_curly: LCurly,
                                stmts: vec![Stmt::Binding(Binding {
                                    let_kw: Let,
                                    mut_kw: None,
                                    ident: Ident(Intern::from("x")),
                                    assign: Assign,
                                    rhs: Expr::ReducedExpr(ReducedExpr::Literal(
                                        Literal::IntegerLiteral(IntegerLiteral(4)),
                                    )),
                                    semi: Semi,
                                })],
                                tail: Some(Box::new(Expr::ReducedExpr(ReducedExpr::BindingRef(
                                    Ident(Intern::from("x")),
                                )))),
                                r_curly: RCurly,
                            },
                        },
                    },
                    ElseIfBlock {
                        else_kw: Else,
                        if_block: IfBlock {
                            if_kw: If,
                            condition: Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                                Literal::IntegerLiteral(IntegerLiteral(5)),
                            ))),
                            if_block: Block {
                                l_curly: LCurly,
                                stmts: vec![Stmt::Binding(Binding {
                                    let_kw: Let,
                                    mut_kw: None,
                                    ident: Ident(Intern::from("y")),
                                    assign: Assign,
                                    rhs: Expr::ReducedExpr(ReducedExpr::Literal(
                                        Literal::IntegerLiteral(IntegerLiteral(6)),
                                    )),
                                    semi: Semi,
                                })],
                                tail: Some(Box::new(Expr::MathExpr(Box::new(MathExpr {
                                    lhs: ReducedExpr::BindingRef(Ident(Intern::from("y"))),
                                    op: Operator::Add,
                                    rhs: Expr::ReducedExpr(ReducedExpr::Literal(
                                        Literal::IntegerLiteral(IntegerLiteral(0)),
                                    )),
                                })))),
                                r_curly: RCurly,
                            },
                        },
                    },
                ],
                else_block: Some(crate::expr::cond_expr::ElseBlock {
                    else_kw: Else,
                    block: Block {
                        l_curly: LCurly,
                        stmts: vec![],
                        tail: Some(Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                            Literal::IntegerLiteral(IntegerLiteral(7)),
                        )))),
                        r_curly: RCurly,
                    },
                }),
            },
            Span::new(0, 79),
        ),
    );
}

#[test]
fn parse_struct_expr() {
    let input = stream("X { a: 0, b: 5 + 5 }");

    assert_parse::<StructExpr>(
        input,
        Spanned::new(
            StructExpr {
                ident: Ident(Intern::from("X")),
                body: StructExprBody {
                    l_paren: LCurly,
                    fields: Punct {
                        inner: vec![(
                            NamedExpr {
                                field: Ident(Intern::from("a")),
                                colon: Colon,
                                expr: Expr::ReducedExpr(ReducedExpr::Literal(
                                    Literal::IntegerLiteral(IntegerLiteral(0)),
                                )),
                            },
                            Comma,
                        )],
                        tail: Some(Box::new(NamedExpr {
                            field: Ident(Intern::from("b")),
                            colon: Colon,
                            expr: Expr::MathExpr(Box::new(MathExpr {
                                lhs: ReducedExpr::Literal(Literal::IntegerLiteral(IntegerLiteral(
                                    5,
                                ))),
                                op: Operator::Add,
                                rhs: Expr::ReducedExpr(ReducedExpr::Literal(
                                    Literal::IntegerLiteral(IntegerLiteral(5)),
                                )),
                            })),
                        })),
                    },
                    r_paren: RCurly,
                },
            },
            Span::new(0, 20),
        ),
    )
}
