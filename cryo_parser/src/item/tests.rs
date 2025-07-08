use cryo_span::{Span, Spanned};
use internment::Intern;

use crate::{
    atoms::{Colon, Comma, LCurly, RCurly, Struct},
    ident::Ident,
    item::{
        TypedBinding,
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
