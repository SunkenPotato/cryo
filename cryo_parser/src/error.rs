#![allow(private_bounds)]

use std::fmt::{Debug, Display};

use cryo_span::Span;

pub(crate) trait ParseError {
    fn code(&self) -> u32;
    fn name(&self) -> &'static str;
    fn subcode(&self) -> u32;
    fn span(&self) -> &Span;
}

impl PartialEq for dyn ParseError {
    fn eq(&self, other: &Self) -> bool {
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

impl Display for dyn ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} ({}/{}): {}",
            self.name(),
            self.code(),
            self.subcode(),
            self.span()
        )
    }
}

#[macro_export]
macro_rules! sec {
    ($f:tt | $sec:tt) => {
        $sec
    };
}

#[macro_export]
macro_rules! parse_error {
    (
        $(#[$attr:meta])*
        #(concrete, $name:expr, $code:expr)
        $visibility:vis enum $identifier:ident {
            $(
                $(#[$var_attr:meta])*
                #($var_code:expr, $fmt_str:expr, $($fmt_args:expr,)*)
                $variant:ident $(( $($var_field_name:ident: $var_field:ident,)+ ))?,
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

        impl $crate::ParseError for $crate::S<$identifier> {
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
                        Self::$variant $(( $($var_field,)+ ))? => format!($fmt_str, $($fmt_args,)*),
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
            const fn __a<T: $crate::ParseError>() {}
            $(
                __a::<$crate::S<$variant_type>>();
            )*
        };

        $visibility enum $identifier {
            $(
                $(#[$var_attr])*
                $variant($crate::S<$variant_type>),
            )+
        }

        impl $crate::ParseError for $identifier {
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
                    )*
                }
            }
        }

        impl std::fmt::Display for $identifier {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", match self {
                    $(
                        Self::$variant(v) => v,
                    )*
                })
            }
        }
    }
}
