use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{Data, DeriveInput, Ident, parse};

use crate::err;

pub(crate) fn derive_parse_inner(tokens: TokenStream) -> syn::Result<TokenStream2> {
    let input = parse::<DeriveInput>(tokens)?;
    match input.data {
        Data::Enum(_) => derive_parse_inner_enum(input),
        Data::Struct(_) => derive_parse_inner_struct(input),
        _ => Ok(err("#[derive(Parse)] cannot be applied to enums")),
    }
}

fn derive_parse_inner_enum(input: DeriveInput) -> syn::Result<TokenStream2> {
    let Data::Enum(input_enum) = input.data else {
        unreachable!()
    };

    if let Err(v) = input_enum.variants.iter().try_for_each(|v| {
        if v.fields.len() == 1 {
            Ok(())
        } else {
            Err(err("must have only one type"))
        }
    }) {
        return Ok(v);
    }

    let types = input_enum
        .variants
        .iter()
        .map(|v| &v.fields.iter().next().unwrap().ty)
        .collect::<Vec<_>>();
    let variant_names = input_enum.variants.iter().map(|v| &v.ident);
    let types0 = types.iter();
    let types1 = types.iter();
    let types2 = types.iter();
    let associated_eo_variant = types.iter().enumerate().map(|v| {
        Ident::new(
            &char::from_u32(v.0 as u32 + 0x41).unwrap().to_string(),
            Span::call_site(),
        )
    });
    let identifier = input.ident;

    let out = quote! {
        impl crate::parser::Parse for #identifier {
            type Output = Self;

            fn parse(tokens: &mut ::cryo_lexer::stream::TokenStreamGuard) -> crate::parser::ParseResult<Self::Output> {
                crate::parser::combinators::Either::<(#(#types0),*)>::parse(tokens).map(|v| v.map(Into::into))
            }
        }

        impl From<crate::parser::combinators::EitherOutput::<#(#types1),*>> for #identifier {
            fn from(value: crate::parser::combinators::EitherOutput<#(#types2),*>) -> Self {
                match value {
                    #(
                        crate::parser::combinators::EitherOutput::#associated_eo_variant(v) => Self::#variant_names(v),
                    )*
                    _ => unreachable!()
                }
            }
        }
    };

    Ok(out)
}
fn derive_parse_inner_struct(input: DeriveInput) -> syn::Result<TokenStream2> {
    let Data::Struct(input_struct) = input.data else {
        unreachable!()
    };

    if input_struct.fields.len() < 2 {
        return Ok(err("must have at least two fields"));
    }

    let identifier = input.ident;
    let types = input_struct
        .fields
        .iter()
        .map(|v| &v.ty)
        .collect::<Vec<_>>();
    let types0 = types.iter();
    let types1 = types.iter();
    let types2 = types.iter();

    let assoc_id = (0..input_struct.fields.len()).map(|v| {
        Ident::new(
            &char::from_u32(v as u32 + 0x61).unwrap().to_string(),
            Span::call_site(),
        )
    });
    // could be rearranged to use some kind of try iterator
    let into_constructor = match input_struct.fields.iter().next().unwrap().ident.is_some() {
        true => {
            let identifier = input_struct
                .fields
                .iter()
                .map(|v| v.ident.as_ref().unwrap());

            quote! {
                Self {
                    #(
                        #identifier: value.#assoc_id
                    ),*
                }
            }
        }
        false => quote! {
            Self(#(value.#assoc_id),*)
        },
    };

    let out = quote! {
        impl crate::parser::Parse for #identifier {
            type Output = Self;

            fn parse(tokens: &mut ::cryo_lexer::stream::TokenStreamGuard) -> crate::parser::ParseResult<Self::Output> {
                <crate::parser::combinators::And::<(#(#types0),*)> as crate::parser::Parse>::parse(tokens).map(|v| v.map(Into::into))
            }
        }

        impl From<crate::parser::combinators::AndOutput<#(#types1),*>> for #identifier {
            fn from(value: crate::parser::combinators::AndOutput<#(#types2),*>) -> Self {
                #into_constructor
            }
        }
    };

    Ok(out)
}
