//! Tokens consisting of single characters, such as ';' (semicolon).

macro_rules! single_token {
    ($name:ident, $c:literal, $doc:expr) => {
        #[derive(Clone, Copy, Default, Debug, PartialEq, Eq)]
        #[doc = $doc]
        pub struct $name;

        impl $name {
            /// The string representation of this struct.
            pub const TOKEN: &str = $c;
        }

        impl $crate::Lex for $name {
            fn lex(input: &str) -> Result<($crate::tokens::Token, &str), cryo_span::Span> {
                let span = cryo_span::Span::ONE;

                $crate::tag(Self::TOKEN, input).ok_or(span).map(|s| {
                    (
                        $crate::tokens::Token::new($crate::tokens::TokenType::$name(Self), span),
                        s,
                    )
                })
            }
        }
    };
}

single_token!(
    Assign,
    "=",
    "The assign token. Used in bindings or reassignments."
);
single_token!(
    Semicolon,
    ";",
    "The semicolon token. Used to end statements."
);
