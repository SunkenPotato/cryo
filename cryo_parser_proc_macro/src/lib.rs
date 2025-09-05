//! Procedural macros for the `cryo_parser` crate.
#![feature(proc_macro_diagnostic)]

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::{Data, DeriveInput, Ident, parse_macro_input, spanned::Spanned};

/// Derive the `IsFail` trait for this struct or enum.
///
/// If this is an enum, the variant marked with `#[fail]` will be used when checking
#[proc_macro_derive(IsFail, attributes(fail))]
pub fn derive_is_fail(tokens: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(tokens as DeriveInput);

    derive_is_fail_inner(derive_input).unwrap().into()
}

fn cmp_err(s: &'static str) -> TokenStream2 {
    quote::quote! {
        compile_error!(#s)
    }
}

fn derive_is_fail_inner(input: DeriveInput) -> syn::Result<TokenStream2> {
    if let Some(attr) = input.attrs.iter().find(|v| v.meta.path().is_ident("fail"))
        && let Ok(name_val) = attr.meta.require_name_value()
    {
        let (impl_g, ty_g, where_cl) = input.generics.split_for_impl();
        let ident = input.ident;
        let expr = &name_val.value;

        return Ok(::quote::quote! {
            impl #impl_g IsFail for #ident #ty_g #where_cl {
                fn is_fail(&self) -> bool {
                    #expr
                }
            }
        });
    }

    match input.data {
        Data::Enum(_) => derive_is_fail_enum(input),
        Data::Struct(_) => derive_is_fail_struct(input),
        Data::Union(_) => derive_is_fail_union(),
    }
}

fn derive_is_fail_enum(input: DeriveInput) -> syn::Result<TokenStream2> {
    let Data::Enum(data) = input.data else {
        unreachable!();
    };

    let ident = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    // #[fail(bubble)]
    if let Some(v) = input.attrs.iter().find(|v| v.meta.path().is_ident("fail"))
        && let Ok(tokens) = v.meta.require_list()
    {
        let bubble = tokens.parse_args::<Ident>();

        if let Ok(bubble) = bubble
            && bubble == "bubble"
        {
            let variants = data.variants;
            let idents = variants.iter().map(|v| &v.ident);

            return Ok(::quote::quote! {
                impl #impl_generics IsFail for #ident #ty_generics #where_clause {
                    fn is_fail(&self) -> bool {
                        match self {
                            #(
                                Self::#idents(v) => IsFail::is_fail(v)
                            ),*
                        }
                    }
                }
            });
        } else {
            return Ok(cmp_err("expected `bubble`"));
        }
    }

    let variants = data.variants.iter().filter_map(|v| {
        v.attrs
            .iter()
            .any(|v| v.meta.path().is_ident("fail"))
            .then_some(&v.ident)
    });

    Ok(::quote::quote! {
        impl #impl_generics IsFail for #ident #ty_generics #where_clause {
            fn is_fail(&self) -> bool {
                matches!(self, #(Self::#variants {..})|*)
            }
        }
    })
}

fn derive_is_fail_struct(input: DeriveInput) -> syn::Result<TokenStream2> {
    let Data::Struct(data) = input.data else {
        unreachable!();
    };

    let ident = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let Some((idx, field)) = data
        .fields
        .iter()
        .enumerate()
        .find(|(_, v)| v.attrs.iter().any(|v| v.meta.path().is_ident("fail")))
    else {
        return Ok(cmp_err(
            "a field with the #[fail] attribute is required, or mark the struct with #[fail = bool]",
        ));
    };

    let field_ident = field
        .ident
        .clone()
        .unwrap_or_else(|| Ident::new(&idx.to_string(), field.ty.span()));

    Ok(::quote::quote! {
        impl #impl_generics IsFail for #ident #ty_generics #where_clause {
            fn is_fail(&self) -> bool {
                self.#field_ident
            }
        }
    })
}

fn derive_is_fail_union() -> syn::Result<TokenStream2> {
    Ok(quote::quote! {
        compile_error!("derive(IsFail) is not applicable on unions")
    })
}
