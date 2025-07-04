use cryo_lexer::stream::TokenStreamGuard;
use cryo_span::{Span, Spanned};

#[allow(unused_imports)]
pub use self::{
    and::{And, AndOutput},
    either::{Either, EitherOutput},
};
use crate::{parser::Parse, parser::ParseResult};

macro_rules! variadic {
    ($macro:ident $(, $($bounds:ty),*)?) => {
        $macro!(A, B);
        $macro!(A, B, C);
        $macro!(A, B, C, D);
        $macro!(A, B, C, D, E);
        $macro!(A, B, C, D, E, F);
        $macro!(A, B, C, D, E, F, G);
        $macro!(A, B, C, D, E, F, G, H);
        $macro!(A, B, C, D, E, F, G, H, I);
        $macro!(A, B, C, D, E, F, G, H, I, J);
        $macro!(A, B, C, D, E, F, G, H, I, J, K);
        $macro!(A, B, C, D, E, F, G, H, I, J, K, L);
    };
}

macro_rules! sealed {
    ($($t:ident),+) => {
        impl<$($t,)+> Sealed for ($($t,)+) {}
    };
}

struct Sealer;
pub struct Never(Sealer);

impl Parse for Never {
    type Output = Self;
    fn parse(_: &mut TokenStreamGuard) -> ParseResult<Self::Output> {
        Ok(Spanned::new(Self(Sealer), Span::ZERO))
    }
}

variadic!(sealed);

trait Sealed {}

mod either {
    use std::marker::PhantomData;

    use cryo_lexer::stream::TokenStreamGuard;

    use crate::{
        parser::ParseResult,
        parser::combinators::{Never, Sealed},
    };

    use super::super::Parse;

    macro_rules! impl_parse_either {
        ($a:ident, $b:ident $(,)? $($t:ident),*) => {
            impl<$a, $b, $($t,)*> ParseEither for ($a, $b, $($t,)*)
            where
                $a: Parse,
                $b: Parse,
                $($t: Parse,)*
            {
                type Output = EitherOutput<$a, $b, $($t,)*>;

                fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self::Output> {
                    tokens.with($a::parse).map(|v| v.map(EitherOutput::$a)).or_else(|_| tokens.with($b::parse).map(|v| v.map(EitherOutput::$b)))
                    $(
                        .or_else(|_| tokens.with($t::parse).map(|v| v.map(EitherOutput::$t)))
                    )*
                }
            }
        }
    }

    variadic!(impl_parse_either);

    pub trait ParseEither: Sealed {
        type Output;

        fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self::Output>;
    }

    pub struct Either<A: ParseEither> {
        _p: PhantomData<A>,
    }

    pub enum EitherOutput<
        A,
        B,
        C: Parse = Never,
        D: Parse = Never,
        E: Parse = Never,
        F: Parse = Never,
        G: Parse = Never,
        H: Parse = Never,
        I: Parse = Never,
        J: Parse = Never,
        K: Parse = Never,
        L: Parse = Never,
    >
    where
        A: Parse,
        B: Parse,
    {
        A(A::Output),
        B(B::Output),
        C(C::Output),
        D(D::Output),
        E(E::Output),
        F(F::Output),
        G(G::Output),
        H(H::Output),
        I(I::Output),
        J(J::Output),
        K(K::Output),
        L(L::Output),
    }

    impl<A> Parse for Either<A>
    where
        A: ParseEither,
    {
        type Output = A::Output;
        fn parse(
            tokens: &mut cryo_lexer::stream::TokenStreamGuard,
        ) -> crate::parser::ParseResult<Self::Output> {
            tokens.with(A::parse)
        }
    }
}

mod and {
    use std::marker::PhantomData;

    use crate::{Spanned, parser::combinators::Never};
    use cryo_lexer::stream::TokenStreamGuard;
    use cryo_parser_proc_macro::impl_parse_and;
    use cryo_span::Span;

    use crate::{
        parser::ParseResult,
        parser::{Parse, combinators::Sealed},
    };

    pub trait ParseAnd: Sealed {
        type Output;
        fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self::Output>;
    }

    variadic!(impl_parse_and);

    pub struct And<A: ParseAnd> {
        _p: PhantomData<A>,
    }

    impl<A: ParseAnd> Parse for And<A> {
        type Output = A::Output;

        fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self::Output> {
            tokens.with(A::parse)
        }
    }

    #[allow(unused)]
    pub struct AndOutput<
        A: Parse,
        B: Parse,
        C: Parse = Never,
        D: Parse = Never,
        E: Parse = Never,
        F: Parse = Never,
        G: Parse = Never,
        H: Parse = Never,
        I: Parse = Never,
        J: Parse = Never,
        K: Parse = Never,
        L: Parse = Never,
    > {
        pub a: A::Output,
        pub b: B::Output,
        pub c: C::Output,
        pub d: D::Output,
        pub e: E::Output,
        pub f: F::Output,
        pub g: G::Output,
        pub h: H::Output,
        pub i: I::Output,
        pub j: J::Output,
        pub k: K::Output,
        pub l: L::Output,
    }
}
