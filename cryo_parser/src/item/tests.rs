use cryo_span::{Span, Spanned};
use internment::Intern;

use crate::{
    atoms::{Colon, Comma, LCurly, LParen, RCurly, RParen, Semi, Struct},
    ident::Ident,
    item::{
        TypedBinding,
        struct_item::{NamedStructBody, StructBody, StructItem, StructTupleBody},
    },
    parser::Punct,
    test_util::{assert_parse, stream},
};

#[test]
fn parse_empty_struct() {
    let input = stream("struct X;");

    assert_parse::<StructItem>(
        input,
        Spanned::new(
            StructItem {
                kw_struct: Struct,
                ident: Ident(Intern::from("X")),
                body: StructBody::Empty(Semi),
            },
            Span::new(0, 9),
        ),
    )
}

#[test]
fn parse_tuple_struct() {
    let input = stream("struct X(int, str,);");

    assert_parse::<StructItem>(
        input,
        Spanned::new(
            StructItem {
                kw_struct: Struct,
                ident: Ident(Intern::from("X")),
                body: StructBody::Tuple(StructTupleBody {
                    l_paren: LParen,
                    fields: Punct {
                        inner: vec![
                            (Ident(Intern::from("int")), Comma),
                            (Ident(Intern::from("str")), Comma),
                        ],
                        tail: None,
                    },
                    r_paren: RParen,
                    semi: Semi,
                }),
            },
            Span::new(0, 20),
        ),
    );
}

#[test]
fn parse_named_struct() {
    let input = stream("struct X { a: int, b: str }");

    assert_parse::<StructItem>(
        input,
        Spanned::new(
            StructItem {
                kw_struct: Struct,
                ident: Ident(Intern::from("X")),
                body: StructBody::Named(NamedStructBody {
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
                }),
            },
            Span::new(0, 27),
        ),
    )
}
