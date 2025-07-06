//! Literal expressions.
//!
//! Literal expressions are expressions that consist of literal tokens, i.e., they are direct and require no further computation when evaluated.

use crate::{parse_error, parser::Parse};
use cryo_lexer::literal::{IntegerLiteral as IToken, StringLiteral as SToken};
use cryo_parser_proc_macro::Parse;
use cryo_span::Spanned;
use itertools::Itertools;

parse_error! {
    /// Errors returned when parsing literals.
    #(group)
    pub enum LiteralError {
        /// An error that may occur when parsing a string literal.
        StringLiteralError(StringLiteralError),
        /// An error that may occur when parsing an integer literal.
        IntegerLiteralError(IntegerLiteralError),
    }
}

/// A literal expression.
///
/// View the module-level docs for more information.
#[derive(Parse, Debug, PartialEq)]
pub enum Literal {
    /// An integer literal.
    IntegerLiteral(IntegerLiteral),
    /// A string literal.
    StringLiteral(StringLiteral),
}

/// An integer literal.
///
/// This is a wrapper around [`i32`] and therefore has the same properties as `i32`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntegerLiteral(pub i32);

parse_error! {
    /// Errors that may occur when parsing integer literals.
    #(concrete, "integer parse literal error", 1)
    pub enum IntegerLiteralError {
        /// The given input would overflow if evaluated, since this type is constrained.
        #(0, "integer overflow",)
        Overflow,
    }
}

impl Parse for IntegerLiteral {
    type Output = Self;

    fn parse(
        tokens: &mut cryo_lexer::stream::TokenStreamGuard,
    ) -> crate::parser::ParseResult<Self::Output> {
        let token = tokens.advance_require::<IToken>()?;
        let mut int = 0i32;
        let mut sgn = 1;

        for c in token.0.chars() {
            if let '_' = c {
                continue;
            } else if let '-' = c {
                sgn *= -1;
                continue;
            }

            let digit = (c as u32).cast_signed() - 0x30;
            int = int
                .checked_mul(10)
                .ok_or(Spanned::new(IntegerLiteralError::Overflow, token.span))?
                .checked_add(digit)
                .ok_or(Spanned::new(IntegerLiteralError::Overflow, token.span))?;
        }

        Ok(Spanned::new(Self(int * sgn), token.span))
    }
}

/// An unescaped string literal.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringLiteral(pub String);

parse_error! {
    /// Errors that may occur when parsing a string literal.
    #(concrete, "string literal parse error", 2)
    pub enum StringLiteralError {
        /// An invalid escape character was encountered.
        #(0, "invalid escape {c}",)
        InvalidEscape(c: char),
    }
}

impl Parse for StringLiteral {
    type Output = Self;

    // TODO: add support for unicode escapes
    fn parse(
        tokens: &mut cryo_lexer::stream::TokenStreamGuard,
    ) -> crate::parser::ParseResult<Self::Output> {
        let token = tokens.advance_require::<SToken>()?;
        let mut buffer = String::with_capacity(token.0.len());

        let mut iter = token.0.chars().tuple_windows();

        while let Some((a, b)) = iter.next() {
            if let '\\' = a {
                buffer.push(match b {
                    'n' => '\n',
                    't' => '\t',
                    '0' => '\0',
                    '"' => '"',
                    esc => {
                        return Err(Box::new(Spanned::new(
                            StringLiteralError::InvalidEscape(esc),
                            token.span,
                        )));
                    }
                });

                iter.next();
            } else {
                // a because the last b is guaranteed to be '"'
                buffer.push(a)
            }
        }
        buffer.shrink_to_fit();

        Ok(Spanned::new(Self(buffer), token.span))
    }
}
