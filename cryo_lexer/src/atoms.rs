//! Atoms.
//!
//! Atoms are tokens which are constant and only may be parsed from one specific input.

#[doc(hidden)]
macro_rules! atom {
    (
        $(#[$attr:meta])*
        $identifier:ident  = $atom:expr
    ) => {
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        $(#[$attr])*
        pub struct $identifier;

        impl $crate::Lex for $identifier {
            fn lex(s: &str) -> Result<($crate::Token, &str), $crate::LexicalError> {
                let rest = s
                    .strip_prefix($atom)
                    .ok_or(
                        $crate::LexicalError::new(
                            $crate::LexicalErrorKind::SequenceNotFound($atom), cryo_span::Span::new(0, $atom.len() as u32)
                        ))?;
                Ok(($crate::Token::new($crate::TokenKind::$identifier, $atom.into(), cryo_span::Span::new(0, $atom.len() as u32)), rest))
            }
        }

        #[cfg(test)]
        ::paste::paste! {
            #[test]
            #[expect(non_snake_case)]
            fn [<lex_ $identifier>]() {
                use $crate::Lex;

                assert_eq!(
                    $identifier::lex($atom),
                    Ok((
                        $crate::Token::new(
                            $crate::TokenKind::$identifier,
                            $atom.into(),
                            cryo_span::Span::new(0, $atom.len() as u32)
                        ),
                        ""
                    ))
                )
            }
        }
    };


    (
        $(#[$attr:meta])*
        $visibility:vis enum $identifier:ident {
            $(
                $(#[$variant_attr:meta])*
                #($atom:expr)
                $variant:ident
            ),*
        } with $constructor:path
    ) => {
        $(#[$attr])*
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        $visibility enum $identifier {
            $(
                $(#[$variant_attr])*
                $variant,
            )*
        }

        impl $identifier {
            #[doc = concat!("The variants ", stringify!($identifier), " may have")]
            $visibility const VARIANTS: &[Self] = &[$(
                Self::$variant,
            )*];

            #[doc = concat!("Return the string this variant would be able to be parsed from")]
            $visibility const fn as_str(&self) -> &str {
                match self {
                    $(
                        Self::$variant => $atom,
                    )*
                }
            }
        }

        impl $crate::Lex for $identifier {
            fn lex(s: &str) -> Result<($crate::Token, &str), $crate::Error> {
                for variant in Self::VARIANTS {
                    let atom = variant.as_str();
                    if let Some(v) = s.strip_prefix(variant.as_str()) {
                        return Ok(($crate::Token::new($constructor(*variant), cryo_span::Span::new(0, atom.len() as u32)), v))
                    }
                }

                let (start, _) = $crate::find_token_end(s);
                Err($crate::Error::new($crate::LexicalError::SequenceNotFound(stringify!($identifier)), cryo_span::Span::new(0, start.len() as u32)))
            }
        }

        token_marker!($identifier);

        #[cfg(test)]
        ::paste::paste! {
            #[expect(non_snake_case)]
            mod [<$identifier _tests>] {
                use $crate::Lex;
                $(
                    #[test]
                    #[expect(non_snake_case)]
                    fn [<lex_ $identifier _ $variant>]() {
                        assert_eq!(
                            $crate::TokenType::lex(super::$identifier::$variant.as_str()),
                            Ok((
                                $crate::Token::new(
                                    $constructor(
                                        super::$identifier::$variant
                                    ),
                                    cryo_span::Span::new(0, super::$identifier::$variant.as_str().len() as u32)
                                ),
                                ""
                            ))
                        );
                    }
                )*
            }
        }
    };

    (
        $(#[$attr:meta])*
        $visibility:vis enum $identifier:ident {
            $(
                $(#[$variant_attr:meta])*
                #($atom:expr)
                $variant:ident
            ),*
        }
    ) => {
        atom!(
            $(#[$attr])*
            $visibility enum $identifier {
                $(
                    $(#[$variant_attr])*
                    #($atom)
                    $variant
                ),*
            } with $crate::TokenType::$identifier
        );
    };
}

atom!(
    /// The left curly parenthesis.
    LCurly = "{"
);
atom!(
    /// The right curly parenthesis.
    RCurly = "}"
);
atom!(
    /// The left parenthesis.
    LParen = "("
);
atom!(
    /// The right parenthesis.
    RParen = ")"
);
atom!(
    /// A semicolon.
    Semi = ";"
);
atom!(
    /// A comma.
    Comma = ","
);
atom!(
    /// A colon.
    Colon = ":"
);

atom!(
    /// A dot.
    Dot = "."
);

atom!(
    /// `+`.
    Plus = "+"
);

atom!(
    /// `-`.
    Minus = "-"
);

atom!(
    /// `*`.
    Star = "*"
);

atom!(
    /// `/`.
    Slash = "/"
);

atom!(
    /// `%`.
    Percent = "%"
);

atom!(
    /// `=`.
    Equal = "="
);

atom!(
    /// `!`.
    Bang = "!"
);

atom!(
    /// `@`.
    At = "@"
);
