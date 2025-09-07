use std::fmt::Debug;

use cryo_diagnostic::SourceFile;
use cryo_lexer::{Lexer, stream::StreamLike};
use cryo_span::Spanned;

use crate::{Parse, ParseError};

#[track_caller]
pub fn assert_parse<T>(input: &str, expected: Spanned<T>)
where
    T: Parse + Debug + PartialEq,
{
    let lexer = Lexer::new(SourceFile::Memory(input.to_owned().into_boxed_str()));
    let (_, mut stream) = lexer.lex().expect("input should be valid");

    match stream.spanning(T::parse) {
        Ok(v) => {
            pretty_assertions::assert_eq!(expected, v)
        }
        Err(e) => {
            panic!("failed to parse {}: {e:?}", ::core::any::type_name::<T>())
        }
    }
}

#[expect(unused)]
#[track_caller]
pub fn assert_parse_fail<T>(input: &str, expected: ParseError)
where
    T: Parse + Debug,
{
    let lexer = Lexer::new(SourceFile::Memory(input.to_owned().into_boxed_str()));
    let (_, mut stream) = lexer.lex().expect("input should be valid");

    match stream.spanning(T::parse) {
        Ok(v) => panic!("parse succeeded: {v:?}"),
        Err(e) => pretty_assertions::assert_eq!(e, expected),
    }
}
