macro_rules! single_token {
    ($name:ident, $c:literal) => {
        #[derive(Clone, Copy, Default, Debug, PartialEq, Eq)]
        pub struct $name;

        impl $name {
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

single_token!(Assign, "=");
single_token!(Semicolon, ";");
