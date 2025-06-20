//! Tokens created by the lexer.

use std::any::Any;

use cryo_span::{Span, impl_get_span};

use super::{
    Lex,
    identifier::Identifier,
    keyword::Keyword,
    literal::Literal,
    operation::Operation,
    single::{Assign, Semicolon},
};

type LexFunction = fn(&str) -> Result<(Token, &str), Span>;

/// The different types a token can be.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u16)]
pub enum TokenType {
    /// The [`Keyword`] token.
    Keyword(Keyword),
    /// The [`Identifier`] token.
    Identifier(Identifier),
    /// The [`Literal`] token.
    Literal(Literal),
    /// The [`Assign`] token.
    Assign(Assign),
    /// The arithmetic operator ([`Operation`]) token.
    Operation(Operation),
    /// The semicolon token.
    Semicolon(Semicolon),
}

impl TokenType {
    const LEX_FUNCTIONS: &[LexFunction] = &[
        Keyword::lex,
        Identifier::lex,
        Literal::lex,
        Assign::lex,
        Operation::lex,
        Semicolon::lex,
    ];

    /// Require the inner value to be of `T`, and return it as a reference or return `None`.
    pub fn require_ref<T: 'static + TokenGroup>(&self) -> Option<&T> {
        self.as_any_ref().downcast_ref()
    }

    /// Require the inner value of the current enum variant to be of `T` and return it, or else return `self`.
    pub fn require<T: 'static + TokenGroup>(self) -> Result<T, Self> {
        dbg!(core::any::type_name::<T>());
        dbg!(&self);
        let any_ref = self.as_any_ref();

        if !any_ref.is::<T>() {
            return Err(self);
        }

        let result = self.as_any().downcast::<T>();

        result
            .map(|v| *v)
            .map_err(|_| unsafe { core::hint::unreachable_unchecked() })
    }

    /// Get the inner value of the current enum variant as [`std::any::Any`].
    pub fn as_any_ref(&self) -> &dyn Any {
        use self::TokenType::*;

        match self {
            Keyword(v) => v,
            Identifier(v) => v,
            Literal(v) => v,
            Assign(v) => v,
            Operation(v) => v,
            Semicolon(v) => v,
        }
    }

    /// Consume `self` and get the inner value of the current enum variant as [`Any`].
    ///
    /// This method is *not* the same as `Box::new(self)`.
    pub fn as_any(self) -> Box<dyn Any> {
        use self::TokenType::*;
        match self {
            Keyword(v) => Box::new(v),
            Identifier(v) => Box::new(v),
            Literal(v) => Box::new(v),
            Assign(v) => Box::new(v),
            Operation(v) => Box::new(v),
            Semicolon(v) => Box::new(v),
        }
    }

    /// Check if the current token is of `T`.
    pub fn is<T: 'static>(&self) -> bool {
        self.as_any_ref().is::<T>()
    }

    /// Retrieve the name of this variant as specified by [`TokenGroup`].
    pub const fn name(&self) -> &'static str {
        match self {
            Self::Keyword(_) => Keyword::NAME,
            Self::Identifier(_) => Identifier::NAME,
            Self::Literal(_) => Literal::NAME,
            Self::Assign(_) => Assign::NAME,
            Self::Operation(_) => Operation::NAME,
            Self::Semicolon(_) => Semicolon::NAME,
        }
    }
}

impl Lex for TokenType {
    fn lex(input: &str) -> Result<(Token, &str), Span> {
        let mut err = None;

        for f in Self::LEX_FUNCTIONS {
            match f(input) {
                Err(e) => {
                    err = Some(e);
                }
                Ok(v) => return Ok(v),
            }
        }

        Err(err.unwrap_or(Span::EMPTY))
    }
}

/// A token, as lexed by the lexer.
///
/// Contains the actual token ([`Token::token`]) and the [`Span`] where this token lies in the input.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Token {
    /// The actual token.
    pub token: TokenType,
    /// The span pointing to where this token originates in the input.
    pub span: Span,
}

impl_get_span!(Token, span);

impl Token {
    /// Create a new token with from a [`TokenType`] and a [`Span`].
    #[must_use]
    pub const fn new(token: TokenType, span: Span) -> Self {
        Self { token, span }
    }

    /// Require the `token` field to have an inner type of `T` and return `&T` or return `None`.
    pub fn require_ref<T: 'static + TokenGroup>(&self) -> Option<&T> {
        self.token.require_ref::<T>()
    }

    /// Require the `token` field to have an inner type of `T` and return and owned value or return `Err(self)`.
    pub fn require<T: 'static + TokenGroup>(self) -> Result<T, Self> {
        match self.token.require() {
            Ok(v) => Ok(v),
            Err(token) => Err(Self {
                token,
                span: self.span,
            }),
        }
    }

    /// Check if the current token is of type `T`.
    pub fn is<T: 'static>(&self) -> bool {
        self.token.is::<T>()
    }

    /// See [`Token::name`].
    pub const fn name(&self) -> &'static str {
        self.token.name()
    }
}

trait Sealed {}
/// A marker trait for all types a [`TokenType`] can contain.
#[allow(private_bounds)]
pub trait TokenGroup: Sealed {
    /// Return the name of this lexeme, used for formatting and errors.
    const NAME: &'static str;
}

macro_rules! impl_tg {
    ($($t:ty),*) => {
        $(
            impl Sealed for $t {}
            impl TokenGroup for $t {
                const NAME: &'static str = stringify!($t);
            }
        )*
    }
}

impl_tg![Keyword, Identifier, Literal, Assign, Operation, Semicolon];
