use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{FnArg, ItemFn, LitStr, Pat, PatIdent, ReturnType, Type, TypePath, TypeReference};

#[proc_macro]
pub fn node_kind(input: TokenStream) -> TokenStream {
    let input_str: LitStr = syn::parse(input).expect("Not a string");
    let value = input_str.value();

    let language = tree_sitter_ridotto::language();

    if language.id_for_node_kind(&value, true) == 0 && language.id_for_node_kind(&value, false) == 0
    {
        let error_message = format!("Unknown node kind {:?}", value);
        quote::quote_spanned! {
            input_str.span() => compile_error!(#error_message)
        }
        .into_token_stream()
        .into()
    } else {
        input_str.into_token_stream().into()
    }
}

#[proc_macro]
pub fn field_name(input: TokenStream) -> TokenStream {
    let input_str: LitStr = syn::parse(input).expect("Not a string");
    let value = input_str.value();

    let language = tree_sitter_ridotto::language();

    if language.field_id_for_name(&value).is_none() {
        let error_message = format!("Unknown field name {:?}", value);
        quote::quote_spanned! {
            input_str.span() => compile_error!(#error_message)
        }
        .into_token_stream()
        .into()
    } else {
        input_str.into_token_stream().into()
    }
}

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

    if !match &input_fn.sig.output {
        ReturnType::Type(_, type_) => match type_.as_ref() {
            Type::Path(TypePath { path, .. }) => {
                path.segments.len() == 1 && path.segments[0].ident == "Result"
            }
            _ => false,
        },
        _ => false,
    } {
        panic!("Expected to return a Result");
    }

    let fn_sig = input_fn.sig.clone();
    let fn_name = input_fn.sig.ident.to_string();
    let fn_body = input_fn.block.clone();

    let output_fn = quote::quote! {
        #fn_sig {
            trace(depth, "> ", #fn_name, scanner);
            let depth = depth + 1;
            if depth > 600 {
                tracing::error!("recursion limit reached");
                return Err(RidottoError::RecursionLimitReached {
                    pos: scanner.peek_token().pos
                });
            }
            let mut inner = || #fn_body;
            let result = inner();
            let depth = depth - 1;
            if result.is_ok() {
                trace(depth, "< ", #fn_name, scanner);
            } else {
                trace(depth, "! ", #fn_name, scanner);
            }
            result
        }
    };

    output_fn.into_token_stream().into()
}
