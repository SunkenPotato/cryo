use cryo_span::{Span, Spanned};
use internment::Intern;

use crate::{
    atoms::{Colon, Comma, Fun, LCurly, LParen, RCurly, RParen, Struct},
    expr::{
        Expr, ReducedExpr,
        binary_expr::BinaryExpr,
        block::Block,
        fn_call::FnCall,
        literal::{IntegerLiteral, Literal},
    },
    ident::Ident,
    item::{
        TypedBinding,
        fn_def::{FnDef, FnRetTy},
        struct_item::{StructBody, StructDef},
    },
    parser::Punct,
    test_util::{assert_parse, stream},
};

#[test]
fn parse_named_struct() {
    let input = stream("struct X { a: int, b: str }");

    assert_parse::<StructDef>(
        input,
        Spanned::new(
            StructDef {
                kw_struct: Struct,
                ident: Ident(Intern::from("X")),
                body: StructBody {
                    l_paren: LCurly,
                    fields: Punct {
                        inner: vec![(
                            TypedBinding {
                                ident: Ident(Intern::from("a")),
                                colon: Colon,
                                ty: Ident(Intern::from("int")),
                            },
                            Comma,
                        )],
                        tail: Some(Box::new(TypedBinding {
                            ident: Ident(Intern::from("b")),
                            colon: Colon,
                            ty: Ident(Intern::from("str")),
                        })),
                    },
                    r_paren: RCurly,
                },
            },
            Span::new(0, 27),
        ),
    )
}

#[test]
fn parse_fn_def_no_args_no_ret() {
    let stream = stream("fun x() { 5 }");

    assert_parse::<FnDef>(
        stream,
        Spanned::new(
            FnDef {
                fn_kw: Fun,
                ident: Ident(Intern::from("x")),
                l_arg_paren: crate::atoms::LParen,
                args: Punct {
                    inner: vec![],
                    tail: None,
                },
                r_arg_paren: RParen,
                ret_ty: None,
                body: Block {
                    l_curly: LCurly,
                    stmts: vec![],
                    tail: Some(Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                        Literal::IntegerLiteral(IntegerLiteral(5)),
                    )))),
                    r_curly: RCurly,
                },
            },
            Span::new(0, 13),
        ),
    );
}

#[test]
fn parse_fn_def_args_ret() {
    let stream = stream("fun add(a: int, b: int): int { a + b }");

    assert_parse::<FnDef>(
        stream,
        Spanned::new(
            FnDef {
                fn_kw: Fun,
                ident: Ident(Intern::from("add")),
                l_arg_paren: LParen,
                args: Punct {
                    inner: vec![(
                        TypedBinding {
                            ident: Ident(Intern::from("a")),
                            colon: Colon,
                            ty: Ident(Intern::from("int")),
                        },
                        Comma,
                    )],
                    tail: Some(Box::new(TypedBinding {
                        ident: Ident(Intern::from("b")),
                        colon: Colon,
                        ty: Ident(Intern::from("int")),
                    })),
                },
                r_arg_paren: RParen,
                ret_ty: Some(FnRetTy {
                    colon: Colon,
                    ret_ty: Ident(Intern::from("int")),
                }),
                body: Block {
                    l_curly: LCurly,
                    stmts: vec![],
                    tail: Some(Box::new(Expr::BinaryExpr(Box::new(BinaryExpr {
                        lhs: ReducedExpr::BindingRef(Ident(Intern::from("a"))),
                        op: crate::expr::binary_expr::Operator::Add,
                        rhs: Expr::ReducedExpr(ReducedExpr::BindingRef(Ident(Intern::from("b")))),
                    })))),
                    r_curly: RCurly,
                },
            },
            Span::new(0, 38),
        ),
    );
}

#[test]
fn parse_fn_call() {
    let stream = stream("add(1, 2)");

    assert_parse::<FnCall>(
        stream,
        Spanned::new(
            FnCall {
                fn_ident: Ident(Intern::from("add")),
                l_arg_paren: LParen,
                args: Punct {
                    inner: vec![(
                        Expr::ReducedExpr(ReducedExpr::Literal(Literal::IntegerLiteral(
                            IntegerLiteral(1),
                        ))),
                        Comma,
                    )],
                    tail: Some(Box::new(Expr::ReducedExpr(ReducedExpr::Literal(
                        Literal::IntegerLiteral(IntegerLiteral(2)),
                    )))),
                },
                r_arg_paren: RParen,
            },
            Span::new(0, 9),
        ),
    )
}
