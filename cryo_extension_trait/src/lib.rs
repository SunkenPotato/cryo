//! Attribute for defining extension traits.

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::{Attribute, Ident, ItemImpl, parse_macro_input};

/// The `#[extension_trait]` attribute.
#[proc_macro_attribute]
pub fn extension_trait(attrs: TokenStream, tokens: TokenStream) -> TokenStream {
    extension_trait_inner(attrs, tokens)
}

fn extension_trait_inner(attrs: TokenStream, tokens: TokenStream) -> TokenStream {
    let attrs = parse_macro_input!(attrs with Attribute::parse_outer);
    let input = parse_macro_input!(tokens as ItemImpl);

    let attribute = attrs
        .iter()
        .find(|attr| attr.meta.path().is_ident("extension_trait"))
        .unwrap();

    let attr_name_value =
        syn::parse::<Ident>(attribute.meta.require_list().unwrap().tokens.clone().into()).unwrap();

    input.

    todo!()
}
