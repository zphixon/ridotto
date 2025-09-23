use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{parenthesized, parse::Parse, parse_macro_input, punctuated::Punctuated, Ident, Token};

#[derive(Debug)]
enum TreeAttrFieldKind {
    Token,
    Tokens,
    Tree,
    Trees,
}

impl Parse for TreeAttrFieldKind {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let kind: Ident = input.parse()?;
        match kind.to_string().as_str() {
            "token" => Ok(TreeAttrFieldKind::Token),
            "tokens" => Ok(TreeAttrFieldKind::Tokens),
            "tree" => Ok(TreeAttrFieldKind::Tree),
            "trees" => Ok(TreeAttrFieldKind::Trees),
            _ => Err(input.error("Expected one of token/tokens/tree/trees")),
        }
    }
}

#[derive(Debug)]
struct TreeAttrField {
    name: Ident,
    kind: TreeAttrFieldKind,
    options: Vec<Ident>,
}

impl Parse for TreeAttrField {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name = input.parse::<Ident>()?;
        input.parse::<Token![=]>()?;
        let kind: TreeAttrFieldKind = input.parse()?;

        let content;
        let _paren = parenthesized!(content in input);
        let options = content
            .parse_terminated(Ident::parse, Token![,])?
            .into_iter()
            .collect::<Vec<_>>();

        if options.is_empty() {
            return Err(input.error("need at least one option"));
        }

        Ok(TreeAttrField {
            name,
            kind,
            options,
        })
    }
}

#[derive(Debug)]
struct TreeAttr {
    fields: Vec<TreeAttrField>,
}

impl Parse for TreeAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(TreeAttr {
            fields: Punctuated::<TreeAttrField, Token![,]>::parse_terminated(input)?
                .into_iter()
                .collect(),
        })
    }
}

#[proc_macro_derive(ParseTree, attributes(tree))]
pub fn parse_tree_derive(item: TokenStream) -> TokenStream {
    let item: syn::ItemEnum = parse_macro_input!(item);

    let mut trees = Vec::new();

    for variant in item.variants.iter() {
        for attr in variant.attrs.iter() {
            if attr.path().is_ident("tree") {
                let variant = variant.ident.clone();
                match attr.parse_args::<TreeAttr>() {
                    Ok(tree) => trees.push((variant, tree)),
                    Err(err) => {
                        return err.to_compile_error().into_token_stream().into();
                    }
                }
            }
        }
    }

    let mut tys = Vec::new();
    for (variant, tree) in trees {
        let mut accessors = Vec::new();
        for field in tree.fields {
            let name = field.name;

            match field.kind {
                TreeAttrFieldKind::Token => {
                    let options = field.options;
                    let option_strs = options.iter().map(Ident::to_string).collect::<Vec<_>>();

                    let comma_sep = format!("`TokenKind::{}`", option_strs.join("`, `TokenKind::"));
                    let doc = format!(
                        "Interpret tree as `TreeKind::{}`, return first child token of kind {}",
                        variant, comma_sep
                    );

                    accessors.push(quote::quote! {
                        #[doc = #doc]
                        pub fn #name<'src>(tree: &Tree<'src>) -> Token<'src> {
                            for child in tree.children.iter() {
                                if let Child::Token(token) = child {
                                    if [#(TokenKind::#options,)*].contains(&token.kind) {
                                        return *token;
                                    }
                                }
                            }
                            unreachable!("looking for {} on {}", #comma_sep, stringify!(#variant));
                        }
                    });
                }

                TreeAttrFieldKind::Tree => {
                    let options = field.options;
                    let option_strs = options.iter().map(Ident::to_string).collect::<Vec<_>>();

                    let comma_sep = format!("`TreeKind::{}`", option_strs.join("`, `TreeKind::"));
                    let doc = format!(
                        "Interpret tree as `TreeKind::{}`, return first child tree of kind {}",
                        variant, comma_sep
                    );

                    accessors.push(quote::quote! {
                        #[doc = #doc]
                        pub fn #name<'parent, 'src>(tree: &'parent Tree<'src>) -> &'parent Tree<'src> {
                            for child in tree.children.iter() {
                                if let Child::Tree(child) = child {
                                    if [#(TreeKind::#options,)*].contains(&child.kind) {
                                        return child;
                                    }
                                }
                            }
                            unreachable!("looking for {} on {}", #comma_sep, stringify!(#variant));
                        }
                    })
                }

                TreeAttrFieldKind::Tokens => {
                    let options = field.options;
                    let option_strs = options.iter().map(Ident::to_string).collect::<Vec<_>>();

                    let comma_sep = format!("`TokenKind::{}`", option_strs.join("`, `TokenKind::"));
                    let doc = format!(
                        "Interpret tree as `TreeKind::{}`, return first child token of kind {}",
                        variant, comma_sep
                    );

                    accessors.push(quote::quote! {
                        #[doc = #doc]
                        pub fn #name<'src>(tree: &Tree<'src>) -> Vec<Token<'src>> {
                            let mut tokens = Vec::new();
                            for child in tree.children.iter() {
                                if let Child::Token(token) = child {
                                    if [#(TokenKind::#options,)*].contains(&token.kind) {
                                        tokens.push(*token);
                                    }
                                }
                            }
                            assert!(!tokens.is_empty(), "looking for {} on {}", #comma_sep, stringify!(#variant));
                            tokens
                        }
                    });
                }

                TreeAttrFieldKind::Trees => {
                    let options = field.options;
                    let option_strs = options.iter().map(Ident::to_string).collect::<Vec<_>>();

                    let comma_sep = format!("`TreeKind::{}`", option_strs.join("`, `TreeKind::"));
                    let doc = format!(
                        "Interpret tree as `TreeKind::{}`, return all children trees of kind {}",
                        variant, comma_sep
                    );

                    accessors.push(quote::quote! {
                        #[doc = #doc]
                        pub fn #name<'parent, 'src>(tree: &'parent Tree<'src>) -> Vec<&'parent Tree<'src>> {
                            let mut trees = Vec::new();
                            for child in tree.children.iter() {
                                if let Child::Tree(child) = child {
                                    if [#(TreeKind::#options,)*].contains(&child.kind) {
                                        trees.push(child);
                                    }
                                }
                            }
                            assert!(!trees.is_empty(), "looking for {} on {}", #comma_sep, stringify!(#variant));
                            trees
                        }
                    })
                }
            }
        }

        tys.push(paste::paste!(quote::quote! {
            pub struct #variant;
            impl #variant {
                #(#accessors)*
            }
        }));
    }

    //let err = format!("{:#?}", tys);
    //quote::quote! { compile_error!(#err); }
    //    .into_token_stream()
    //    .into()
    quote::quote! {
        #(#tys)*
    }
    .into_token_stream()
    .into()
}
