//! Derive and procedural macros for the `cryo_parser` crate.

mod derive_parse;

use derive_parse::derive_parse_inner;
use proc_macro::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{Ident, Token, parse_macro_input, punctuated::Punctuated};

/// Implement `ParseAnd` for a given tuple. This is constrained to tuples with the length 12.
#[proc_macro]
pub fn impl_parse_and(tokens: TokenStream) -> TokenStream {
    impl_parse_and_inner(tokens)
}

/// Derive the `Parse` trait for a type.
///
/// If an `enum` is annotated with `derive(Parse)`, the generated implementation of `Parse` will try to parse the variants of the enum in the order that they are defined with their implementation of the `Parse` trait.
/// The first operation to succeed will be used.
///
/// If a `struct` is annotated with `derive(Parse)`, the generated implementation of `Parse` will try to parse the individual fields sequentially in the order that they are defined with their implentation of the `Parse` trait.
/// Therefore, parsing will only succeed if all provided components can be parsed one after another.
#[proc_macro_derive(Parse)]
pub fn derive_parse(tokens: TokenStream) -> TokenStream {
    derive_parse_inner(tokens).unwrap().into()
}

#[allow(unused_variables)]
fn err(s: &str) -> proc_macro2::TokenStream {
    quote! {
        compile_error!(#s)
    }
}

// this macro is only a step stone of a much darker path, hence, it should not be changed or even observed.
//
// traveller, collapse this function and be on your way
fn impl_parse_and_inner(tokens: TokenStream) -> TokenStream {
    let idents: Punctuated<Ident, Token![,]> =
        parse_macro_input!(tokens with Punctuated::parse_separated_nonempty);

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
        impl<#(#iter,)*> ParseAnd for (#(#iter2,)*)
        where #(#iter6: Parse,)*
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
                    let Spanned { t: #other_idents, span: _ } = tokens.with(<Never as Parse>::parse)?;
                )*

                Ok(Spanned::new(AndOutput { a, b, c, d, e, f, g, h, i, j, k, l }, span))
            }
        }
    };

    output.into()
}
