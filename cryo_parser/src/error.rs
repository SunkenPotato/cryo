//! Parse errors for the compiler.
//!
//! Top-level error codes:
//! | Error name | Error code |
//! |------------|------------|
//! | [`GenericError`] | 0 |
//! | [`NumberLiteralError`](super::expr::literal::NumberLiteralError) | 1 |
//! | [`StringLiteralError`](super::expr::literal::StringLiteralError) | 2 |

use std::{borrow::Cow, fmt::Display, sync::OnceLock};

use cryo_span::Span;

use crate::Spanned;

/// Information a parse error contains.
pub trait ParseError {
    /// The span at which the error occurred.
    fn span(&self) -> Span;
    /// The top-level error code. (The top-level error is the type for which this is implemented for).
    fn code(&self) -> u32;
    /// The suberror code.
    fn subcode(&self) -> u32;
    /// The error message.
    fn message(&self) -> String;
    /// The name of the error. Derived from [`core::any::type_name`] by default.
    fn name(&self) -> &'static str {
        static NAME: OnceLock<String> = OnceLock::new();

        NAME.get_or_init(|| {
            core::any::type_name_of_val(self)
                .split("::")
                .last()
                .unwrap()
                .to_owned()
        })
    }
}

impl<E: ParseError + 'static> From<E> for Box<dyn ParseError> {
    fn from(value: E) -> Self {
        Box::new(value)
    }
}

impl PartialEq for dyn ParseError {
    fn eq(&self, other: &Self) -> bool {
        self.code() == other.code() && self.subcode() == other.subcode()
    }
}

impl<E> PartialEq<E> for dyn ParseError
where
    E: ParseError,
{
    fn eq(&self, other: &E) -> bool {
        self.code() == other.code() && self.subcode() == other.subcode()
    }
}

impl Eq for dyn ParseError {}

impl std::fmt::Debug for dyn ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParseError")
            .field("span", &self.span())
            .field("code", &self.code())
            .field("subcode", &self.subcode())
            .field("message", &self.message())
            .finish()
    }
}

impl std::fmt::Display for dyn ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "error({}) {}@{}: {}",
            self.name(),
            self.code(),
            self.subcode(),
            self.message()
        )?;
        write!(f, "{}", self.span())?;

        Ok(())
    }
}

/// Convenience function for generating a [`Box<dyn ParseError>`] from an error and a span.
pub fn err<T>(err: T, span: Span) -> Box<dyn ParseError>
where
    Spanned<T>: ParseError,
    T: 'static,
{
    Box::new(Spanned::new(err, span))
}

/// Emits only the second input passed. Used for macro repetitions.
#[macro_export]
macro_rules! only_second {
    ($first:tt | $second:tt) => {
        $second
    };
}

/// Implement the [`ParseError`] trait for a type and define it.
#[macro_export]
macro_rules! parse_error {
    (
        $(#[$attr:meta])*
        #(concrete, $code:literal)
        $visibility:vis enum $name:ident {
            $(
                $(#[$variant_attr:meta])*
                #($message:expr, $subcode:expr)
                $variant_name:ident $( ( $($variant_field_name:ident: $variant_type:ty),+ ) )?
            ),*
        }
    ) => {
        $(#[$attr])*
        $visibility enum $name {
            $(
                $(#[$variant_attr])*
                $variant_name $((
                    $($variant_type),*
                ))?,
            )*
        }

        ::paste::paste! {
            impl ParseError for crate::Spanned<$name> {
                fn span(&self) -> cryo_span::Span {
                    self.span
                }

                fn subcode(&self) -> u32 {
                    self.t.get_code()
                }

                fn code(&self) -> u32 {
                    $code
                }

                fn message(&self) -> String {
                    self.t.get_message()
                }
            }
        }

        impl $name {
            #[allow(unused, missing_docs)]
            $visibility fn get_message(&self) -> String {
                match self {
                    $(
                        Self::$variant_name $( ( $( $variant_field_name ,)* ) )? => format!($message, $($($variant_field_name,)*)?),
                    )*
                    _ => unreachable!()
                }
            }

            #[allow(unused, missing_docs)]
            $visibility fn get_code(&self) -> u32 {
                match self {
                    $(
                        Self::$variant_name $( ( $( $crate::only_second!($variant_type | _) ,)* ) )? => $subcode,
                    )*
                    _ => unreachable!()
                }
            }
        }
    };

    (
        $(#[$attr:meta])*
        #(group)
        $visibility:vis enum $group:ident {
            $(
                $(#[$variant_attr:meta])*
                $variant:ident($inner:ty),
            )*
        }
    ) => {
        const _: () = {
            const fn assert_parse<T: ParseError>() {}

            $(
                assert_parse::<$inner>();
            )*
        };

        $(
            #[$attr]
        )*
        $visibility enum $group {
            $(
                $(#[$variant_attr])*
                $variant($inner),
            )*
        }

        #[allow(unreachable_patterns)]
        impl ParseError for $group {
            fn subcode(&self) -> u32 {
                match self {
                    $(
                        Self::$variant(v) => ParseError::subcode(v),
                    )*
                    _ => unreachable!()
                }
            }

            fn code(&self) -> u32 {
                match self {
                    $(
                        Self::$variant(v) => ParseError::code(v),
                    )*
                    _ => unreachable!()
                }
            }

            fn span(&self) -> cryo_span::Span {
                match self {
                    $(
                        Self::$variant(v) => ParseError::span(v),
                    )*
                    _ => unreachable!()
                }
            }

            fn message(&self) -> String {
                match self {
                    $(
                        Self::$variant(v) => ParseError::message(v),
                    )*
                    _ => unreachable!()
                }
            }
        }
    }
}

parse_error! {
    /// Errors that can occur while parsing anything.
    #(concrete, 0)
    pub enum GenericError {
        /// An unexpected end of input
        #("unexpected end of input", 0)
        EndOfInput,
        /// An incorrect token was encountered.
        ///
        /// The first field represents the expected token, the second the received token.
        #("incorrect token: expected `{}`, got `{}`", 1)
        IncorrectToken(expected: &'static str, recieved: &'static str)
    }
}

/// Represents a parse error.
///
/// Contains a span pointing to where the problematic tokens are in the input,
/// an error message, and a code.
#[derive(Debug, Clone)]
pub struct Error {
    span: Span,
    message: Cow<'static, str>,
    code: u32,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "error[E{:0<5}]: {}", self.code, self.message)?;
        write!(f, "--> {}", self.span)?;

        Ok(())
    }
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        (self.code == other.code) && (self.span == other.span)
    }
}

impl Eq for Error {}

#[cfg(test)]
/// Provides an error for comparing errors in tests.
pub mod test_error {
    use crate::error::ParseError;

    /// A struct used for comparing test errors.
    #[derive(Debug)]
    pub struct TestError {
        /// The top-level error code.
        pub code: u32,
        /// The subcode.
        pub subcode: u32,
    }

    impl TestError {
        /// Create a new [`TestError`].
        pub const fn new(code: u32, subcode: u32) -> Self {
            Self { code, subcode }
        }
    }

    impl ParseError for TestError {
        fn code(&self) -> u32 {
            self.code
        }

        fn subcode(&self) -> u32 {
            self.subcode
        }

        fn span(&self) -> cryo_span::Span {
            unreachable!()
        }

        fn message(&self) -> String {
            unreachable!()
        }
    }
}
