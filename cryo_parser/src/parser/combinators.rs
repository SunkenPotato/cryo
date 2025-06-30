use std::marker::PhantomData;

use cryo_span::Spanned;

use crate::Parse;

pub struct Either<A, B>
where
    A: Parse,
    B: Parse,
{
    _p: PhantomData<(A, B)>,
}

pub enum EitherOutput<A, B>
where
    A: Parse,
    B: Parse,
{
    A(A::Output),
    B(B::Output),
}

impl<A, B> Parse for Either<A, B>
where
    A: Parse,
    B: Parse,
{
    type Output = EitherOutput<A, B>;
    fn parse(
        tokens: &mut cryo_lexer::stream::TokenStreamGuard,
    ) -> crate::ParseResult<Self::Output> {
        tokens
            .with(A::parse)
            .map(|v| v.map(EitherOutput::A))
            .or_else(|_| tokens.with(B::parse).map(|v| v.map(EitherOutput::B)))
    }
}

pub struct And<A, B> {
    _p: PhantomData<(A, B)>,
}

pub struct AndOutput<A, B>
where
    A: Parse,
    B: Parse,
{
    pub a: A::Output,
    pub b: B::Output,
}

impl<A, B> Parse for AndOutput<A, B>
where
    A: Parse,
    B: Parse,
{
    type Output = AndOutput<A, B>;

    fn parse(
        tokens: &mut cryo_lexer::stream::TokenStreamGuard,
    ) -> crate::ParseResult<Self::Output> {
        let Spanned { t: a, span: span_a } = tokens.with(A::parse)?;
        let Spanned { t: b, span: span_b } = tokens.with(B::parse)?;

        Ok(Spanned::new(AndOutput { a, b }, span_a + span_b))
    }
}
