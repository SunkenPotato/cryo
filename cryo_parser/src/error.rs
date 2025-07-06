//! Parse errors.
//!
//! To accomodate different error types being returned from parsers, rather than returning a concrete type, parsers return a `Result<T, Box<dyn ParseError>>`.
//!
//! A parse error consists of a `code` indicating the code of the error type, a `subcode` which is unique to each variant within the enum, a [`Span`] supplied by `Spanned<ErrorType>` and a name used for display operations.
//!
//! Furthermore, each parse error type implements [`std::fmt::Display`].
//!
//! To declare a parse error type, view [`parse_error`](cryo_parser::parse_error).

#![allow(private_bounds)]

use std::fmt::{Debug, Display};

use cryo_lexer::stream::TokenStreamError;
use cryo_span::{Span, source_map::SourceMap};

/// An abstract representation of a parse error.
pub trait ParseError {
    /// The code of this parse error.
    fn code(&self) -> u32;
    /// The name of this parse error.
    fn name(&self) -> &'static str;
    /// The subcode of this parse error / parse error variant.
    fn subcode(&self) -> u32;
    /// The span this parse error points has.
    fn span(&self) -> &Span;
}

impl ParseError for TokenStreamError {
    fn code(&self) -> u32 {
        0
    }

    fn subcode(&self) -> u32 {
        match self {
            TokenStreamError::EndOfInput => 0,
            TokenStreamError::IncorrectToken(_, _) => 1,
        }
    }

    fn name(&self) -> &'static str {
        "token stream error"
    }

    fn span(&self) -> &Span {
        match self {
            TokenStreamError::EndOfInput => &Span::ZERO,
            TokenStreamError::IncorrectToken(_, s) => s,
        }
    }
}

impl<T: ParseError + ?Sized> PartialEq<T> for dyn ParseError {
    fn eq(&self, other: &T) -> bool {
        (self.code() == other.code()) && (self.subcode() == other.subcode())
    }
}

impl Eq for dyn ParseError {}

impl Debug for dyn ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("dyn ParseError")
            .field("code", &self.code())
            .field("subcode", &self.subcode())
            .field("name", &self.name())
            .field("span", &self.span())
            .finish()
    }
}

impl Display for ParseErrorDisplay<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} ({}/{}): {}",
            self.0.name(),
            self.0.code(),
            self.0.subcode(),
            self.0.span().display(self.1)
        )
    }
}

/// Provides an interface with which [`ParseError`]s can be displayed.
pub struct ParseErrorDisplay<'e, 'map>(&'e dyn ParseError, &'map SourceMap);

impl dyn ParseError {
    /// Obtain an interface with which this error can be displayed.
    pub const fn display<'e, 'map>(&'e self, map: &'map SourceMap) -> ParseErrorDisplay<'e, 'map> {
        ParseErrorDisplay(self, map)
    }
}

impl<T: ParseError + 'static> From<T> for Box<dyn ParseError> {
    fn from(value: T) -> Self {
        Box::new(value)
    }
}

/// Emit only the second token passed.
#[macro_export]
macro_rules! sec {
    ($f:tt | $sec:tt) => {
        $sec
    };
}

/// Define a parse error.
///
/// You can either define a concrete parse error type or a grouping of parse error types, where for the latter, the properties of the variant type are used in the [`ParseError`] implementation.
///
/// Concrete errors must have an `#(concrete, $name, $code)` attribute after rust attributes are defined.
///
/// Each variant must be a unit or tuple variant where for the latter, each field must be given a name so as it may be used in the [`Display`] implementation generated.
/// Furthermore, each variant must also have the attribute of the form `#($subcode, $format_string, $format_args)` after the declaration of rust attributes.
///
/// A grouping parse error is defined as a simple enum with no required attributes and tuple variants with exactly one field that the error type may be. The variant further more has the bound:
/// ```compile_fail
/// Spanned<E>: ParseError
/// ```
/// It also must be annotated with the `#(group)` attribute.
///
/// In both cases, [`ParseError`] is not implemented for the enum, but rather for `Spanned<T>`.
/// In a group enum however, the macro automatically specifies the variant fields to be of `Spanned<E>`, where `E` is the type that the variant was declared with inside the macro.
///
/// ## Examples
/// ```compile_fail
/// use cryo_parser::parse_error;
/// parse_error! {
///     #[repr(C)]
///     /// documentation...
///     #[must_use]
///     #(concrete, "my parse error type", 314)
///     pub enum ParseErrorType {
///         #(1, "variant a error",)
///         VariantA,
///         #(2, "variant b with extra info: {}", info)
///         VariantB(info: String),
///     }
/// }
/// ```
/// ```compile_fail
/// use cryo_parser::parse_error;
/// parse_error! {
///     #[derive(Clone, PartialEq, Debug)]
///     #(group)
///     pub enum Group {
///         ConcreteA(ConcreteA),
///         OtherGroup(OtherGroup)
///     }
/// }
/// ```
#[macro_export]
macro_rules! parse_error {
    (
        $(#[$attr:meta])*
        #(concrete, $name:expr, $code:expr)
        $visibility:vis enum $identifier:ident {
            $(
                $(#[$var_attr:meta])*
                #($var_code:expr, $fmt_str:expr, $($fmt_args:expr),*)
                $variant:ident $(( $($var_field_name:ident: $var_field:ident),+ ))?,
            )+
        }
    ) => {
        $(#[$attr])*
        $visibility enum $identifier {
            $(
                $(#[$var_attr])*
                $variant $(( $($var_field,)+ ))?,
            )+
        }

        impl $crate::error::ParseError for $crate::S<$identifier> {
            fn code(&self) -> u32 {
                $code
            }

            fn name(&self) -> &'static str {
                $name
            }

            fn subcode(&self) -> u32 {
                match self.t {
                    $(
                        $identifier::$variant $(( $( $crate::sec!($var_field_name | _) ,)+ ))? => $var_code,
                    )+
                }
            }

            fn span(&self) -> &cryo_span::Span {
                &self.span
            }
        }

        impl std::fmt::Display for $identifier {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", match self {
                    $(
                        Self::$variant $(( $($var_field_name,)+ ))? => format!($fmt_str, $($fmt_args,)*),
                    )+
                })
            }
        }
    };

    (
        $(#[$attr:meta])*
        #(group)
        $visibility:vis enum $identifier:ident {
            $(
                $(#[$var_attr:meta])*
                $variant:ident($variant_type:ident),
            )+
        }
    ) => {
        const _: () = {
            const fn __a<T: $crate::error::ParseError>() {}
            $(
                __a::<$crate::S<$variant_type>>();
            )*
        };

        $(#[$attr])*
        $visibility enum $identifier {
            $(
                $(#[$var_attr])*
                $variant($crate::S<$variant_type>),
            )+
        }

        ::paste::paste! {
            struct [<$identifier Display>]<'s, 'map>(&'s $identifier, &'map ::cryo_span::source_map::SourceMap);

            impl $identifier {
                #[doc = concat!("Provide an interface for displaying ", stringify!($identifier), ".")]
                $visibility const fn display<'s, 'map>(&'s self, map: &'map ::cryo_span::source_map::SourceMap) -> [<$identifier Display>]<'s, 'map> {
                    [<$identifier Display>](self, map)
                }
            }
        }

        impl $crate::error::ParseError for $identifier {
            fn code(&self) -> u32 {
                match self {
                    $(
                        Self::$variant(v) => v.code(),
                    )*
                }
            }

            fn subcode(&self) -> u32 {
                match self {
                    $(
                        Self::$variant(v) => v.subcode(),
                    )*
                }
            }

            fn name(&self) -> &'static str {
                match self {
                    $(
                        Self::$variant(v) => v.name(),
                    )*
                }
            }

            fn span(&self) -> &cryo_span::Span {
                match self {
                    $(
                        Self::$variant(v) => v.span()
                    ),*
                }
            }
        }

        ::paste::paste! {
            impl std::fmt::Display for [<$identifier Display>]<'_, '_> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self.0 {
                        $(
                            $identifier::$variant(v) => write!(f, "{}", v.display(self.1)),
                        )*
                    }
                }
            }
        }
    }
}
