use cryo_span::{Span, Spanned};

use crate::{
    expr::literal::{IntegerLiteral, StringLiteral},
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
