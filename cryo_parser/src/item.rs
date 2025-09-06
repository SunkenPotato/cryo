//! Items.

use std::sync::LazyLock;

use crate::{
    CommaSeparated, IsFail, OneOrMany, Parse, ParseError, ParseErrorKind, TypedIdent,
    ident::{Ident, RECORD},
};
use cryo_lexer::{TokenKind, stream::StreamLike};
use cryo_parser_proc_macro::IsFail;
use cryo_span::Spanned;
use internment::Intern;

/// A top-level item, such as a struct.
#[derive(Clone, PartialEq, Eq, Debug, IsFail)]
#[fail(bubble)]
pub enum Item {
    /// A record definition.
    Record(RecordDef),
}

impl Item {
    const ACCEPTED_KEYWORDS: &[&LazyLock<Intern<str>>] = &[&RECORD];
}

/// A record definition.
#[derive(Clone, PartialEq, Eq, Debug, IsFail)]
#[fail = false] // TODO: remove
pub struct RecordDef {
    /// The name, or identifier of this record.
    pub ident: Spanned<Ident>,
    /// The fields of this record.
    pub fields: Spanned<CommaSeparated<TypedIdent>>,
}

/// An enum definition.
pub struct EnumDef {}

////////////////////////////////////
// Parsers                        //
////////////////////////////////////

impl Parse for RecordDef {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let rec_kw = tokens.spanning(Ident::parse)?;
        if *rec_kw.t != *RECORD {
            return Err(ParseError::new(
                rec_kw.span,
                rec_kw.span,
                ParseErrorKind::ExpectedKeyword(OneOrMany::Owned(&RECORD)),
            ));
        }

        let ident = tokens.spanning(Ident::parse)?;
        tokens.advance_require(TokenKind::LParen)?;
        let fields = tokens.spanning(CommaSeparated::parse)?;
        tokens.advance_require(TokenKind::RParen)?;
        tokens.advance_require(TokenKind::Semi)?;

        Ok(Self { ident, fields })
    }
}

impl Parse for Item {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let next = tokens.peek_require(TokenKind::Identifier)?;

        if next.t == *RECORD {
            tokens.with(RecordDef::parse).map(Self::Record)
        } else {
            Err(ParseError {
                span: next.span,
                context: next.span,
                kind: ParseErrorKind::ExpectedKeyword(OneOrMany::Multiple(Self::ACCEPTED_KEYWORDS)),
            })
        }
    }
}

////////////////////////////////////
// Tests                          //
////////////////////////////////////

#[cfg(test)]
mod tests {
    use cryo_lexer::{Symbol, atoms::Comma};
    use cryo_span::{Span, Spanned};

    use crate::{
        CommaSeparated, TypedIdent,
        ident::Ident,
        item::{Item, RecordDef},
        test_util::assert_parse,
    };

    #[test]
    fn parse_record() {
        assert_parse(
            "record Point(x: int, y: int);",
            Spanned::new(
                Item::Record(RecordDef {
                    ident: Spanned::new(Ident(Symbol::from("Point")), Span::new(7, 12)),
                    fields: Spanned::new(
                        CommaSeparated {
                            inner: vec![(
                                Spanned::new(
                                    TypedIdent {
                                        ident: Ident(Symbol::from("x")),
                                        ty: Spanned::new("int", Span::new(16, 19)).into(),
                                    },
                                    Span::new(13, 19),
                                ),
                                Spanned::new(Comma, Span::new(19, 20)),
                            )],
                            last: Some(Box::new(Spanned::new(
                                TypedIdent {
                                    ident: Ident(Symbol::from("y")),
                                    ty: Spanned::new("int", Span::new(24, 27)).into(),
                                },
                                Span::new(21, 27),
                            ))),
                        },
                        Span::new(13, 27),
                    ),
                }),
                Span::new(0, 29),
            ),
        );
    }
}
