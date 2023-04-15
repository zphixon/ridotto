use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{token::Mut, FnArg, ItemFn, Pat, PatIdent, PatType, Type, TypePath, TypeReference};

#[proc_macro_attribute]
pub fn parser_traced(_: TokenStream, item: TokenStream) -> TokenStream {
    let input_fn: ItemFn = syn::parse(item).expect("Not a function");

    if !input_fn.sig.inputs.iter().any(|arg| match arg {
        FnArg::Typed(arg) => {
            let name_matches = match &*arg.pat {
                Pat::Ident(PatIdent { ident, .. }) => ident == "depth",
                _ => false,
            };

            let type_matches = match &*arg.ty {
                Type::Path(TypePath { path, .. }) => {
                    path.segments.len() == 1 && path.segments[0].ident == "usize"
                }
                _ => false,
            };

            name_matches && type_matches
        }

        _ => false,
    }) {
        panic!("Expected an argument called `depth` with type `usize`");
    }

    if !input_fn.sig.inputs.iter().any(|arg| match arg {
        FnArg::Typed(arg) => {
            let name_matches = match &*arg.pat {
                Pat::Ident(PatIdent { ident, .. }) => ident == "scanner",
                _ => false,
            };

            let type_matches = match &*arg.ty {
                Type::Reference(TypeReference {
                    mutability: Some(_),
                    elem,
                    ..
                }) => match &**elem {
                    Type::Path(TypePath { path, .. }) => {
                        path.segments.len() == 1 && path.segments[0].ident == "Scanner"
                    }
                    _ => false,
                },

                _ => false,
            };

            name_matches && type_matches
        }

        _ => false,
    }) {
        panic!("Expected an argument called `scanner` with type `&mut Scanner`");
    }

    let fn_sig = input_fn.sig.clone();
    let fn_name = input_fn.sig.ident.to_string();
    let fn_body = input_fn.block.clone();

    let output_fn = quote::quote! {
        #fn_sig {
            trace(depth, "> ", #fn_name, scanner);
            let depth = depth + 1;
            if depth > 50 {
                tracing::error!("recursion limit reached");
                return Err(RidottoError::RecursionLimitReached {
                    pos: scanner.peek_token().pos
                });
            }
            let result = #fn_body;
            let depth = depth - 1;
            trace(depth, "< ", #fn_name, scanner);
            result
        }
    };

    output_fn.into_token_stream().into()
}
