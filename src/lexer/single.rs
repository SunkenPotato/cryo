macro_rules! single_token {
    ($name:ident, $c:literal) => {
        #[derive(Clone, Copy, Default, Debug, PartialEq, Eq)]
        pub struct $name;

        impl $name {
            pub const TOKEN: &str = $c;
        }

        impl $crate::lexer::Lex for $name {
            fn lex(
                input: &str,
            ) -> Result<($crate::lexer::tokens::Token, &str), $crate::span::Span> {
                let span = $crate::span::Span::ONE;

                $crate::lexer::tag(Self::TOKEN, input).ok_or(span).map(|s| {
                    (
                        $crate::lexer::tokens::Token::new(
                            $crate::lexer::tokens::TokenType::$name(Self),
                            span,
                        ),
                        s,
                    )
                })
            }
        }
    };
}

single_token!(Assign, "=");
single_token!(Semicolon, ";");
