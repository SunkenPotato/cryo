use proc_macro::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{Ident, Token, parse_macro_input, punctuated::Punctuated};

#[proc_macro]
pub fn impl_parse_and(tokens: TokenStream) -> TokenStream {
    impl_parse_and_inner(tokens)
}

#[allow(unused_variables)]
fn err(s: &str) -> TokenStream {
    quote! {
        compile_error!(s)
    }
    .into()
}

// this macro is only a step stone of a much darker path, hence, it should not be changed or even observed.
//
// traveller, collapse this function and be on your way
fn impl_parse_and_inner(tokens: TokenStream) -> TokenStream {
    let idents: Punctuated<Ident, Token![,]> =
        parse_macro_input!(tokens with Punctuated::parse_separated_nonempty);
    if idents.len() < 2 {
        return err("at least two generics are required");
    }

    let other_idents = {
        let last_ident = idents[idents.len() - 1].to_string();
        (char::from_u32(last_ident.chars().next().unwrap() as u32 + 1).unwrap()..='L').map(|v| {
            Ident::new(
                &v.to_ascii_lowercase().to_string(),
                Span::call_site().into(),
            )
        })
    };

    let iter = idents.iter();
    let iter2 = idents.iter();
    let iter3 = idents.iter();
    let iter4 = idents
        .iter()
        .map(|v| Ident::new(&v.to_string().to_ascii_lowercase(), v.span()));
    let iter5 = idents.iter();
    let iter6 = idents.iter();
    let span_idents = idents.iter().map(|v| format_ident!("{}_span", v));

    let output = quote! {
        impl<#(#iter),*> ParseAnd for (#(#iter2),*)
        where #(#iter6: Parse),*
        {
            type Output = AndOutput<#(#iter3),*>;

            #[allow(non_snake_case)]
            fn parse(tokens: &mut TokenStreamGuard) -> ParseResult<Self::Output> {
                let mut span = Span::ZERO;
                #(
                    let Spanned { t: #iter4, span: #span_idents } = tokens.with(#iter5::parse)?;
                    span += #span_idents;
                )*
                #(
                    let Spanned { t: #other_idents, span: _ } = tokens.with(<() as Parse>::parse)?;
                )*

                Ok(Spanned::new(AndOutput { a, b, c, d, e, f, g, h, i, j, k, l }, span))
            }
        }
    };

    output.into()
}
