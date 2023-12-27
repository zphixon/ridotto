use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{
    parse_macro_input, FnArg, ItemFn, Pat, PatIdent, PatType, Path, Signature, Type, TypePath,
    TypeReference,
};

#[proc_macro]
pub fn setup_trace(_: TokenStream) -> TokenStream {
    quote::quote! {
        static INDENT: ::std::sync::atomic::AtomicUsize = ::std::sync::atomic::AtomicUsize::new(0);
    }
    .into_token_stream()
    .into()
}

#[proc_macro_attribute]
pub fn parser_traced(_args: TokenStream, input: TokenStream) -> TokenStream {
    let ItemFn {
        vis, sig, block, ..
    } = parse_macro_input!(input);

    let Signature {
        ident,
        inputs,
        output,
        ..
    } = sig;
    let name = ident.to_string();

    let has_parser = inputs.iter().any(|input| match input {
        FnArg::Typed(PatType { pat, ty, .. }) => match (pat.as_ref(), ty.as_ref()) {
            (
                Pat::Ident(PatIdent { ident, .. }),
                Type::Reference(TypeReference {
                    mutability: Some(_),
                    elem,
                    ..
                }),
            ) if ident.to_string() == "parser" => match elem.as_ref() {
                Type::Path(TypePath {
                    path: Path { segments, .. },
                    ..
                }) => {
                    segments
                        .iter()
                        .map(|segment| segment.ident.to_string())
                        .collect::<Vec<_>>()
                        == vec![String::from("Parser")]
                }
                _ => false,
            },
            _ => false,
        },
        _ => false,
    });

    let (entry, exit) = if has_parser {
        (
            quote::quote! { ::tracing::trace!("{}> {} {:?}", indent, #name, parser.nth(0).lexeme); },
            quote::quote! { ::tracing::trace!("{}< {} {:?} {:?}", indent, #name, parser.nth(0).lexeme, result); },
        )
    } else {
        (
            quote::quote! { ::tracing::trace!("{}> {}", indent, #name); },
            quote::quote! { ::tracing::trace!("{}< {} {:?}", indent, #name, result); },
        )
    };

    quote::quote! {
        #vis fn #ident ( #inputs ) #output {
            let level = INDENT.fetch_add(1, std::sync::atomic::Ordering::AcqRel);
            let indent = "|  ".repeat(level);

            #entry
            let mut inner = || #block;
            let result = inner();
            #exit

            let _ = INDENT.fetch_sub(1, std::sync::atomic::Ordering::AcqRel);
            result
        }
    }
    .into_token_stream()
    .into()
}
