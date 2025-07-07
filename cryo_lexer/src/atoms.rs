//! Atoms.
//!
//! Atoms are tokens which are constant and only may be parsed from one specific input.

use cryo_span::Span;

use crate::{Lex, LexicalError, Token, TokenType, atom, find_token_end};

/// A semicolon token (`;`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Semi;

/// An assign token (`=`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Assign;

/// The left curly brace (`{`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct LCurly;

/// The right curly brace (`}`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct RCurly;

atom!(LCurly, "{");
atom!(RCurly, "}");
atom!(Semi, ";");
atom!(Assign, "=");
atom! {
    /// A keyword.
    ///
    /// Keywords are reserved tokens used to indicate behaviour or declare constructs.
    pub enum Keyword {
        /// The `let` keyword. Used to declare bindings.
        #("let")
        Let,
        /// The `mut` keyword. Used to declare mutability on bindings.
        #("mut")
        Mut,
        /// The `if` keyword. Used to indicate a conditional block.
        #("if")
        If,
        /// The `else` keyword. Used to define the operation to be executed should an if-statement not be valid.
        #("else")
        Else,
        /// The `struct` keyword. Used to define structs.
        #("struct")
        Struct
    }
}

atom! {
    /// Operators for binary expressions.
    pub enum Operators {
        /// The addition operator (`+`).
        #("+")
        Add,
        /// The subtraction operator (`-`).
        #("-")
        Sub,
        /// The multiplication operator (`*`).
        #("*")
        Mul,
        /// The division operator (`/`).
        #("/")
        Div,
        /// The remainder operator (`%`).
        #("%")
        Rem,
        /// The equality operator (`==`).
        #("==")
        Eq,
        /// The inequality operator (`!=`).
        #("!=")
        NotEq
    }
}

/// Item visibility token.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Visibility {
    /// Private visibility.
    Private,
    /// Public visibility.
    Public,
}

impl Lex for Visibility {
    fn lex(s: &str) -> Result<(crate::Token, &str), crate::Error> {
        let (token, rest) = find_token_end(s);

        if let Ok((
            Token {
                t: TokenType::Keyword(Keyword::Struct),
                ..
            },
            ..,
        )) = Keyword::lex(rest)
        {
            return match token {
                "" => Ok((
                    Token::new(TokenType::Visibility(Visibility::Private), Span::new(0, 0)),
                    rest,
                )),
                "pub" => Ok((
                    Token::new(TokenType::Visibility(Visibility::Public), Span::new(0, 3)),
                    rest,
                )),
                _ => Err(crate::Error::new(
                    LexicalError::SequenceNotFound("pub | \"\""),
                    Span::new(0, token.len()),
                )),
            };
        }

        return Err(crate::Error::new(
            LexicalError::SequenceNotFound("item keyword"),
            Span::new(0, token.len()),
        ));
    }
}

token_marker!(Visibility);
