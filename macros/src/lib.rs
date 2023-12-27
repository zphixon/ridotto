use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{parse_macro_input, FnArg, ItemFn, Pat, PatIdent, PatType, Signature};

#[proc_macro]
pub fn setup_trace(_: TokenStream) -> TokenStream {
    quote::quote! {
        static INDENT: ::std::sync::atomic::AtomicUsize = ::std::sync::atomic::AtomicUsize::new(0);
    }
    .into_token_stream()
    .into()
}

#[proc_macro_attribute]
pub fn parser_function(_args: TokenStream, input: TokenStream) -> TokenStream {
    let ItemFn {
        vis, sig, block, ..
    } = parse_macro_input!(input);

    let Signature {
        ident,
        inputs,
        output,
        generics,
        ..
    } = sig;
    let name = ident.to_string();

    let has_lexer = inputs.iter().any(|input| match input {
        FnArg::Typed(PatType { pat, .. }) => match pat.as_ref() {
            Pat::Ident(PatIdent { ident, .. }) => ident.to_string() == "lexer",
            _ => false,
        },
        _ => false,
    });

    let (entry, exit, exit_error) = if has_lexer {
        if name == "consume" {
            (
                quote::quote! { ::tracing::trace!(" {} -> {} {:?} {:?}", indent, #name, token, lexer.peek().copied().lexeme()); },
                quote::quote! { ::tracing::trace!(" {} <- {} {:?} {:?}", indent, #name, token, lexer.peek().copied().lexeme()); },
                quote::quote! { ::tracing::trace!("*{} <- {} {:?} {:?}", indent, #name, token, lexer.peek().copied().lexeme()); },
            )
        } else {
            (
                quote::quote! { ::tracing::trace!(" {} -> {} {:?}", indent, #name, lexer.peek().copied().lexeme()); },
                quote::quote! { ::tracing::trace!(" {} <- {} {:?}", indent, #name, lexer.peek().copied().lexeme()); },
                quote::quote! { ::tracing::trace!("*{} <- {} {:?}", indent, #name, lexer.peek().copied().lexeme()); },
            )
        }
    } else {
        (
            quote::quote! { ::tracing::trace!(" {} -> {}", indent, #name); },
            quote::quote! { ::tracing::trace!(" {} <- {}", indent, #name); },
            quote::quote! { ::tracing::trace!("*{} <- {}", indent, #name); },
        )
    };

    quote::quote! {
        #vis fn #ident #generics ( #inputs ) #output {
            let level = INDENT.fetch_add(1, std::sync::atomic::Ordering::AcqRel);
            let indent = "  ".repeat(level);

            #entry
            let mut inner = || #block;
            let result = inner();
            if !result.is_error() {
                #exit
            } else {
                #exit_error
            }

            let _ = INDENT.fetch_sub(1, std::sync::atomic::Ordering::AcqRel);
            result
        }
    }
    .into_token_stream()
    .into()
}
