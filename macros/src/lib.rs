use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{parse::Parse, parse_macro_input, FnArg, ItemFn, Pat, PatIdent, PatType, Signature};

struct TreeAttr {}

impl Parse for TreeAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(TreeAttr {})
    }
}

// TODO generate types for each TreeKind
#[proc_macro_derive(ParseTree, attributes(tree))]
pub fn parse_tree_derive(item: TokenStream) -> TokenStream {
    let item: syn::ItemEnum = parse_macro_input!(item);

    for variant in item.variants.iter() {
        for attr in variant.attrs.iter() {
            if attr.path().is_ident("tree") {
            }
        }
    }

    TokenStream::new()
}
