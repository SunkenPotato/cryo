//! Type-level parser combinators.

use cryo_lexer::stream::TokenStreamGuard;
use cryo_span::{Span, Spanned};

#[allow(unused_imports)]
pub use self::{
    and::{And, AndOutput},
    either::{Either, EitherOutput},
};
use crate::{parser::Parse, parser::ParseResult};

macro_rules! variadic {
    ($macro:ident) => {
        $macro!(A);
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
#[doc(hidden)]
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
        ($a:ident $(,$($t:ident),+)?) => {
            impl<$a $(,$($t),+)?> ParseEither for ($a, $($($t),+)?)
            where
                $a: Parse
                $(,$($t: Parse),+)?
            {
                type Output = EitherOutput<$a $(,$($t),+)?>;

                fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self::Output> {
                    tokens.with($a::parse).map(|v| v.map(EitherOutput::$a))
                    $($(
                        .or_else(|_| tokens.with($t::parse).map(|v| v.map(EitherOutput::$t)))
                    )+)?
                }
            }
        }
    }

    variadic!(impl_parse_either);

    pub trait ParseEither: Sealed {
        type Output;

        fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self::Output>;
    }

    /// The "or" parser combinator.
    ///
    /// `A` must be a tuple of types implementing `Parse`. Due to type system restrictions, the tuple must have 1 to 12 elements. To specify more, use nested [`Either`]s.
    ///
    /// This type will try to parse the types in the tuple in the order that they were specified and then return that type in the form of `EitherOutput`.
    pub struct Either<A: ParseEither> {
        _p: PhantomData<A>,
    }

    /// The output of a [`Either`] combinator.
    pub enum EitherOutput<
        A: Parse,
        B: Parse = Never,
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
        #[expect(missing_docs)]
        A(A::Output),
        #[expect(missing_docs)]
        B(B::Output),
        #[expect(missing_docs)]
        C(C::Output),
        #[expect(missing_docs)]
        D(D::Output),
        #[expect(missing_docs)]
        E(E::Output),
        #[expect(missing_docs)]
        F(F::Output),
        #[expect(missing_docs)]
        G(G::Output),
        #[expect(missing_docs)]
        H(H::Output),
        #[expect(missing_docs)]
        I(I::Output),
        #[expect(missing_docs)]
        J(J::Output),
        #[expect(missing_docs)]
        K(K::Output),
        #[expect(missing_docs)]
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

    /// The "and" parser combinator.
    ///
    /// `A` must be a tuple of length 1 to 12 where each element implements [`Parse`].
    ///
    /// This combinator will try to parse each type one after another until it is done and then return it in the form of [`AndOutput`].
    pub struct And<A: ParseAnd> {
        _p: PhantomData<A>,
    }

    impl<A: ParseAnd> Parse for And<A> {
        type Output = A::Output;

        fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self::Output> {
            tokens.with(A::parse)
        }
    }

    /// The output of an [`And`] combinator.
    #[allow(unused)]
    pub struct AndOutput<
        A: Parse,
        B: Parse = Never,
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
        #[expect(missing_docs)]
        pub a: A::Output,
        #[expect(missing_docs)]
        pub b: B::Output,
        #[expect(missing_docs)]
        pub c: C::Output,
        #[expect(missing_docs)]
        pub d: D::Output,
        #[expect(missing_docs)]
        pub e: E::Output,
        #[expect(missing_docs)]
        pub f: F::Output,
        #[expect(missing_docs)]
        pub g: G::Output,
        #[expect(missing_docs)]
        pub h: H::Output,
        #[expect(missing_docs)]
        pub i: I::Output,
        #[expect(missing_docs)]
        pub j: J::Output,
        #[expect(missing_docs)]
        pub k: K::Output,
        #[expect(missing_docs)]
        pub l: L::Output,
    }
}
