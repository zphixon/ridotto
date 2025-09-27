use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::ToTokens;
use syn::{parenthesized, parse::Parse, parse_macro_input, punctuated::Punctuated, Ident, Token};

#[derive(Debug, Clone, Copy)]
enum Number {
    NoneOrOne,  // ?
    OneExactly, //
    OneOrMore,  // +
    NoneOrMore, // *
}

#[derive(Debug, Clone, Copy)]
enum AstAttrFieldKind {
    Token(Number),
    Tree(Number),
}

impl Parse for AstAttrFieldKind {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let kind: Ident = input.parse()?;

        let peek = input.lookahead1();
        let number = if peek.peek(Token![?]) {
            let _: Token![?] = input.parse()?;
            Number::NoneOrOne
        } else if peek.peek(Token![+]) {
            let _: Token![+] = input.parse()?;
            Number::OneOrMore
        } else if peek.peek(Token![*]) {
            let _: Token![*] = input.parse()?;
            Number::NoneOrMore
        } else {
            Number::OneExactly
        };

        match kind.to_string().as_str() {
            "token" => Ok(AstAttrFieldKind::Token(number)),
            "tree" => Ok(AstAttrFieldKind::Tree(number)),
            _ => Err(input.error("Expected one of token/tokens/tree/trees")),
        }
    }
}

#[derive(Debug, Clone)]
enum OptionName {
    Name(Ident),
    Expr(#[allow(dead_code)] Token![$]),
}

impl OptionName {
    fn ident(self) -> Option<Ident> {
        match self {
            OptionName::Name(name) => Some(name),
            OptionName::Expr(_) => None,
        }
    }
}

impl Parse for OptionName {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let peek = input.lookahead1();
        if peek.peek(Token![$]) {
            Ok(OptionName::Expr(input.parse()?))
        } else {
            Ok(OptionName::Name(input.parse()?))
        }
    }
}

#[derive(Debug)]
struct AstAttrField {
    name: Ident,
    kind: AstAttrFieldKind,
    options: Vec<OptionName>,
}

impl Parse for AstAttrField {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name = input.parse::<Ident>()?;
        input.parse::<Token![=]>()?;
        let kind: AstAttrFieldKind = input.parse()?;

        let content;
        let _paren = parenthesized!(content in input);
        let options = content
            .parse_terminated(OptionName::parse, Token![,])?
            .into_iter()
            .collect::<Vec<_>>();

        if options.is_empty() {
            return Err(input.error("need at least one option"));
        }

        Ok(AstAttrField {
            name,
            kind,
            options,
        })
    }
}

#[derive(Debug)]
struct AstAttr {
    fields: Vec<AstAttrField>,
}

impl Parse for AstAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(AstAttr {
            fields: Punctuated::<AstAttrField, Token![,]>::parse_terminated(input)?
                .into_iter()
                .collect(),
        })
    }
}

#[proc_macro_derive(Ast, attributes(ast))]
pub fn ast_derive(item: TokenStream) -> TokenStream {
    let item: syn::ItemEnum = parse_macro_input!(item);

    let mut expr_options: Vec<Ident> = vec![];
    for attr in item.attrs.iter() {
        if attr.path().is_ident("ast") {
            match attr.parse_args_with(Punctuated::<Ident, Token![,]>::parse_terminated) {
                Ok(exp) => expr_options.extend(exp.into_iter()),
                Err(err) => return err.to_compile_error().into_token_stream().into(),
            }
        }
    }

    let mut trees = Vec::new();

    for variant in item.variants.iter() {
        for attr in variant.attrs.iter() {
            if attr.path().is_ident("ast") {
                let variant = variant.ident.clone();
                match attr.parse_args::<AstAttr>() {
                    Ok(tree) => trees.push((variant, tree)),
                    Err(err) => {
                        return err.to_compile_error().into_token_stream().into();
                    }
                }
            }
        }
    }

    let mut tys = Vec::new();

    tys.push(quote::quote! {
        #[derive(Debug)]
        pub struct FromTreeError<'t, 's> {
            pub tree: &'t Tree<'s>,
            pub why: String,
        }
    });

    let expr_ident = Ident::new("Expr", Span::call_site());
    tys.push(quote::quote! {
        #[derive(Debug)]
        pub enum #expr_ident<'s> {
            #(#expr_options(Box<#expr_options<'s>>),)*
        }
        impl<'s> #expr_ident<'s> {
            pub fn from_tree<'t: 's>(tree: &'t Tree<'s>) -> Result<Self, FromTreeError<'t, 's>> {
                match tree.kind {
                    #(TreeKind::#expr_options => Ok(Expr::#expr_options(Box::new(#expr_options::from_tree(tree)?))),)*
                    _ => Err(FromTreeError { tree, why: String::from("$: could not construct expr") }),
                }
            }
        }
    });

    for (variant, tree) in trees {
        let mut field_decls = Vec::new();
        let mut field_ctors = Vec::new();

        for field in tree.fields {
            let name = field.name;
            let field_name = name.to_string();
            let new_name = stringcase::pascal_case(&(variant.to_string() + "_" + &field_name));

            let new_enum = Ident::new(&new_name, name.span());

            let tree_type =
                if field.options.len() == 1 && field.options[0].clone().ident().is_some() {
                    let single_non_expr = field.options[0].clone().ident().unwrap();
                    quote::quote! { #single_non_expr }
                } else if field.options.len() == 1 && field.options[0].clone().ident().is_none() {
                    quote::quote! { #expr_ident }
                } else {
                    quote::quote! { #new_enum }
                };

            field_decls.push(match field.kind {
                AstAttrFieldKind::Token(Number::NoneOrOne) => {
                    quote::quote! { #name: Option<Token<'s>> }
                }
                AstAttrFieldKind::Token(Number::OneExactly) => quote::quote! { #name: Token<'s> },
                AstAttrFieldKind::Token(_) => quote::quote! { #name: Vec<Token<'s>> },

                AstAttrFieldKind::Tree(Number::NoneOrOne) => {
                    quote::quote! { #name: Option<#tree_type<'s>> }
                }
                AstAttrFieldKind::Tree(Number::OneExactly) => {
                    quote::quote! { #name: #tree_type<'s> }
                }
                AstAttrFieldKind::Tree(_) => quote::quote! { #name: Vec<#tree_type<'s>> },
            });

            if field.options.len() > 1 && matches!(field.kind, AstAttrFieldKind::Tree(_)) {
                let mut new_enum_variant_names = field
                    .options
                    .clone()
                    .into_iter()
                    .flat_map(OptionName::ident)
                    .collect::<Vec<_>>();

                let mut new_enum_ctors = new_enum_variant_names.clone().into_iter().map(|variant| {
                    quote::quote! {
                        TreeKind::#variant => Ok(#new_enum::#variant(Box::new(#variant::<'s>::from_tree(tree)?)))
                    }
                }).collect::<Vec<_>>();

                let mut new_enum_variant_decls = new_enum_variant_names
                    .clone()
                    .into_iter()
                    .map(|name| quote::quote! { Box<#name<'s>> })
                    .collect::<Vec<_>>();

                if field
                    .options
                    .clone()
                    .into_iter()
                    .any(|option| option.ident().is_none())
                {
                    new_enum_variant_names.push(expr_ident.clone());
                    new_enum_variant_decls.push(quote::quote! { #expr_ident<'s> });

                    new_enum_ctors.extend(expr_options.clone().into_iter().map(|expr_option| {
                        quote::quote! {
                            TreeKind::#expr_option => Ok(#new_enum::Expr(#expr_ident::#expr_option(Box::new(#expr_option::<'s>::from_tree(tree)?))))
                        }
                    }));
                }

                tys.push(quote::quote! {
                    #[derive(Debug)]
                    pub enum #new_enum<'s> {
                        #(#new_enum_variant_names(#new_enum_variant_decls)),*
                    }
                    impl<'s> #new_enum<'s> {
                        pub fn from_tree<'t: 's>(tree: &'t Tree) -> Result<#new_enum<'s>, FromTreeError<'t, 's>> {
                            match tree.kind {
                                #(#new_enum_ctors,)*
                                _ => Err(FromTreeError {
                                    tree,
                                    why: format!("couldn't construct {} out of {:?}", stringify!(#new_enum), tree.kind)
                                }),
                            }
                        }
                    }
                });
            }

            let mut tree_token_kinds = field
                .options
                .clone()
                .into_iter()
                .flat_map(OptionName::ident)
                .collect::<Vec<_>>();
            if field
                .options
                .iter()
                .cloned()
                .any(|option| option.ident().is_none())
            {
                tree_token_kinds.extend(expr_options.clone());
            }

            field_ctors.push(match field.kind {
                AstAttrFieldKind::Token(Number::NoneOrOne) => quote::quote! {
                    #name: tree
                        .children
                        .iter()
                        .find(|child| {
                            child.is_token()
                            && [#(TokenKind::#tree_token_kinds,)*].contains(&child.as_token().kind)
                        })
                        .map(|child| child.as_token())
                },

                AstAttrFieldKind::Token(Number::OneExactly) => quote::quote! {
                    #name: tree
                        .children
                        .iter()
                        .find(|child| {
                            child.is_token()
                            && [#(TokenKind::#tree_token_kinds,)*].contains(&child.as_token().kind)
                        })
                        .ok_or_else(|| FromTreeError {
                            tree,
                            why: format!(concat!("token: could not construct ", stringify!(#name), " of ", stringify!(#variant), " {:?}"), tree),
                        })?
                        .as_token()
                },

                AstAttrFieldKind::Token(Number::OneOrMore) => quote::quote! {
                    #name: {
                        let tokens = tree
                            .children
                            .iter()
                            .filter(|child| {
                                child.is_token()
                                && [#(TokenKind::#tree_token_kinds,)*].contains(&child.as_token().kind)
                            })
                            .map(|child| child.as_token())
                            .collect::<Vec<_>>();
                        if tokens.is_empty() {
                            return Err(FromTreeError {
                                tree,
                                why: format!("token+: missing item for {} of {} {:?}", stringify!(#name), stringify!(#variant), tree),
                            });
                        }
                        tokens
                    }
                },

                AstAttrFieldKind::Token(Number::NoneOrMore) => quote::quote! {
                    #name: tree
                        .children
                        .iter()
                        .filter(|child| {
                            child.is_token()
                            && [#(TokenKind::#tree_token_kinds,)*].contains(&child.as_token().kind)
                        })
                        .map(|child| child.as_token())
                        .collect()
                },

                AstAttrFieldKind::Tree(Number::NoneOrOne) => quote::quote! {
                    #name: tree
                        .children
                        .iter()
                        .find(|child| {
                            child.is_tree()
                            && [#(TreeKind::#tree_token_kinds,)*].contains(&child.as_tree().kind)
                        })
                        .map(|child| #tree_type::<'s>::from_tree(child.as_tree()))
                        .transpose()?
                },

                AstAttrFieldKind::Tree(Number::OneExactly) => quote::quote! {
                    #name: #tree_type::<'s>::from_tree(tree
                        .children
                        .iter()
                        .find(|child| {
                            child.is_tree()
                            && [#(TreeKind::#tree_token_kinds,)*].contains(&child.as_tree().kind)
                        })
                        .ok_or_else(||
                            FromTreeError {
                                tree,
                                why: format!(concat!("tree: could not construct ", stringify!(#name), " of ", stringify!(#variant))), 
                            }
                        )?
                        .as_tree())?
                },

                AstAttrFieldKind::Tree(Number::OneOrMore) => quote::quote! {
                    #name: {
                        let trees = tree
                            .children
                            .iter()
                            .filter(|child| {
                                child.is_tree()
                                && [#(TreeKind::#tree_token_kinds,)*].contains(&child.as_tree().kind)
                            })
                            .map(|child| #tree_type::<'s>::from_tree(child.as_tree()))
                            .collect::<Result<Vec<_>, _>>()?;
                        if trees.is_empty() {
                            return Err(FromTreeError {
                                tree,
                                why: format!("tree+: missing item for {} of {} {:?}", stringify!(#name), stringify!(#variant), tree),
                            });
                        }
                        trees
                    }
                },

                AstAttrFieldKind::Tree(Number::NoneOrMore) => quote::quote! {
                    #name: tree
                        .children
                        .iter()
                        .filter(|child| {
                            child.is_tree()
                            && [#(TreeKind::#tree_token_kinds,)*].contains(&child.as_tree().kind)
                        })
                        .map(|child| #tree_type::<'s>::from_tree(child.as_tree()))
                        .collect::<Result<Vec<_>, _>>()?
                },
            });
        }

        tys.push(quote::quote! {
            #[derive(Debug)]
            pub struct #variant<'s> {
                #(pub #field_decls,)*
            }
            impl<'s> #variant<'s> {
                pub fn from_tree<'t: 's>(tree: &'t Tree<'s>) -> Result<Self, FromTreeError<'t, 's>> {
                    Ok(Self {
                        #(#field_ctors,)*
                    })
                }
            }
        });
    }

    quote::quote! { #(#tys)* }.into_token_stream().into()
}
