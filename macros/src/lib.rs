use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{
    parenthesized, parse::Parse, parse_macro_input, punctuated::Punctuated, 
    Ident, Token,
};

#[derive(Debug)]
enum AstAttrFieldKind {
    Token,
    Tokens,
    Tree,
    Trees,
}

impl Parse for AstAttrFieldKind {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let kind: Ident = input.parse()?;
        match kind.to_string().as_str() {
            "token" => Ok(AstAttrFieldKind::Token),
            "tokens" => Ok(AstAttrFieldKind::Tokens),
            "tree" => Ok(AstAttrFieldKind::Tree),
            "trees" => Ok(AstAttrFieldKind::Trees),
            _ => Err(input.error("Expected one of token/tokens/tree/trees")),
        }
    }
}

#[derive(Debug)]
struct AstAttrField {
    name: Ident,
    kind: AstAttrFieldKind,
    options: Vec<Ident>,
}

impl Parse for AstAttrField {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name = input.parse::<Ident>()?;
        input.parse::<Token![=]>()?;
        let kind: AstAttrFieldKind = input.parse()?;

        let content;
        let _paren = parenthesized!(content in input);
        let options = content
            .parse_terminated(Ident::parse, Token![,])?
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
    for (variant, tree) in trees {
        if tree.fields.len() > 1 {
            let mut field_defs = Vec::new();
            let mut field_constructors = Vec::new();

            for field in tree.fields {
                let name = field.name;
                let options = field.options;

                match field.kind {
                    AstAttrFieldKind::Token => {
                        field_defs.push(quote::quote! { #name: Token<'s> });
                        field_constructors.push(quote::quote! {
                            #name: tree
                                .children
                                .iter()
                                .find(|child| {
                                    child.is_token()
                                    && [#(TokenKind::#options,)*].contains(&child.as_token().kind)
                                })
                                .expect(concat!("missing item for ", stringify!(#name), " of ", stringify!(#variant)))
                                .as_token()
                        });
                    }

                    AstAttrFieldKind::Tokens => {
                        field_defs.push(quote::quote! { #name: Vec<Token<'s>> });
                        field_constructors.push(quote::quote! {
                            #name: tree
                                .children
                                .iter()
                                .filter(|child| {
                                    child.is_token()
                                    && [#(TokenKind::#options,)*].contains(&child.as_token().kind)
                                })
                                .map(|child| child.as_token())
                                .collect()
                        });
                    }

                    AstAttrFieldKind::Tree => {
                        if options.len() > 1 {
                            let field_name = name.to_string();
                            let mut new_name = variant.to_string();
                            new_name.push((field_name.as_bytes()[0] as char).to_uppercase().next().unwrap());
                            new_name.push_str(str::from_utf8(&field_name.as_bytes()[1..]).unwrap());
                            let new_enum = Ident::new(&new_name, name.span());
                            tys.push(quote::quote! {
                                #[derive(Debug)]
                                pub enum #new_enum<'s> {
                                    #(#options(#options<'s>)),*
                                }
                                impl<'s> #new_enum<'s> {
                                    pub fn from<'t: 's>(tree: &'t Tree) -> #new_enum<'s> {
                                        match tree.kind {
                                            #(TreeKind::#options => #new_enum::#options(#options::<'s>::from(tree)),)*
                                            _ => unreachable!("couldn't construct {} out of {:?}", stringify!(#new_enum), tree.kind),
                                        }
                                    }
                                }
                            });
                            field_defs.push(quote::quote! { #name: #new_enum<'s> });
                            field_constructors.push(quote::quote! {
                                #name: #new_enum::<'s>::from(tree
                                    .children
                                    .iter()
                                    .find(|child| {
                                        child.is_tree()
                                        && [#(TreeKind::#options,)*].contains(&child.as_tree().kind)
                                    })
                                        .unwrap_or_else(|| {
                                            panic!(concat!("could not construct ", stringify!(#name), " of ", stringify!(#variant), " {:?}"), tree);
                                        })
                                    .as_tree())
                            })
                        } else {
                            let option = options[0].clone();
                            field_defs.push(quote::quote! { #name: #option<'s> });
                            field_constructors.push(quote::quote! {
                                #name: #option::<'s>::from(
                                    tree
                                        .children
                                        .iter()
                                        .find(|child| {
                                            child.is_tree()
                                            && child.as_tree().kind == TreeKind::#option
                                        })
                                        .unwrap_or_else(|| {
                                            panic!(concat!("could not construct ", stringify!(#name), " of ", stringify!(#variant), " {:?}"), tree);
                                        })
                                        .as_tree()
                                )
                            })
                        }
                    }

                    AstAttrFieldKind::Trees => {
                        if options.len() > 1 {
                            let field_name = name.to_string();
                            let mut new_name = variant.to_string();
                            new_name.push((field_name.as_bytes()[0] as char).to_uppercase().next().unwrap());
                            new_name.push_str(str::from_utf8(&field_name.as_bytes()[1..]).unwrap());
                            let new_enum = Ident::new(&new_name, name.span());
                            tys.push(quote::quote! {
                                #[derive(Debug)]
                                pub enum #new_enum<'s> {
                                    #(#options(#options<'s>)),*
                                }
                                impl<'s> #new_enum<'s> {
                                    pub fn from<'t: 's>(tree: &'t Tree) -> #new_enum<'s> {
                                        match tree.kind {
                                            #(TreeKind::#options => #new_enum::#options(#options::<'s>::from(tree)),)*
                                            _ => unreachable!("couldn't construct {} out of {:?}", stringify!(#new_enum), tree.kind),
                                        }
                                    }
                                }
                            });
                            field_defs.push(quote::quote! { #name: Vec<#new_enum<'s>> });
                            field_constructors.push(quote::quote! {
                                #name: tree
                                    .children
                                    .iter()
                                    .filter(|child| {
                                        child.is_tree()
                                        && [#(TreeKind::#options,)*].contains(&child.as_tree().kind)
                                    })
                                    .map(|child| #new_enum::<'s>::from(child.as_tree()))
                                    .collect()
                            })
                        } else {
                            let option = options[0].clone();
                            field_defs.push(quote::quote! { #name: Vec<#option<'s>> });
                            field_constructors.push(quote::quote! {
                                #name: tree
                                    .children
                                    .iter()
                                    .filter(|child| {
                                        child.is_tree()
                                        && child.as_tree().kind == TreeKind::#option
                                    })
                                    .map(|child| #option::<'s>::from(child.as_tree()))
                                    .collect()
                            })
                        }
                    }
                }
            }

            tys.push(quote::quote! {
                #[derive(Debug)]
                pub struct #variant<'s> {
                    #(pub #field_defs),*
                }
                impl<'s> #variant<'s> {
                    pub fn from<'t: 's>(tree: &'t Tree<'s>) -> #variant<'s> {
                        Self {
                            #(#field_constructors),*
                        }
                    }
                }
            });
        } else {
            let AstAttr { mut fields } = tree;
            let AstAttrField {
                name,
                kind,
                options,
            } = fields.pop().unwrap();

            let (decl, ctor) = match kind {
                AstAttrFieldKind::Token => (
                    quote::quote! { Token<'s> },
                    quote::quote! {
                        tree
                            .children
                            .iter()
                            .find(|child| {
                                child.is_token()
                                && [#(TokenKind::#options,)*].contains(&child.as_token().kind)
                            })
                            .unwrap_or_else(|| {
                                panic!(concat!("could not construct ", stringify!(#name), " of ", stringify!(#variant), " {:?}"), tree);
                            })
                            .as_token()
                    },
                ),

                AstAttrFieldKind::Tokens => (
                    quote::quote! { Vec<Token<'s>> },
                    quote::quote! {
                        tree
                            .children
                            .iter()
                            .filter(|child| {
                                child.is_token()
                                && [#(TokenKind::#options,)*].contains(&child.as_token().kind)
                            })
                            .map(|child| child.as_token())
                            .collect()
                    },
                ),

                AstAttrFieldKind::Tree => {
                    if options.len() > 1 {
                        let field_name = name.to_string();
                        let mut new_name = variant.to_string();
                        new_name.push((field_name.as_bytes()[0] as char).to_uppercase().next().unwrap());
                        new_name.push_str(str::from_utf8(&field_name.as_bytes()[1..]).unwrap());
                        let new_enum = Ident::new(&new_name, name.span());
                        tys.push(quote::quote! {
                                #[derive(Debug)]
                                pub enum #new_enum<'s> {
                                    #(#options(#options<'s>)),*
                                }
                                impl<'s> #new_enum<'s> {
                                    pub fn from<'t: 's>(tree: &'t Tree) -> #new_enum<'s> {
                                        match tree.kind {
                                            #(TreeKind::#options => #new_enum::#options(#options::<'s>::from(tree)),)*
                                            _ => unreachable!("couldn't construct {} out of {:?}", stringify!(#new_enum), tree.kind),
                                        }
                                    }
                                }
                            });
                        (
                            quote::quote! { Box<#new_enum<'s>> },
                            quote::quote! {
                                Box::new(#new_enum::<'s>::from(tree
                                    .children
                                    .iter()
                                    .find(|child| {
                                        child.is_tree()
                                        && [#(TreeKind::#options,)*].contains(&child.as_tree().kind)
                                    })
                                    .unwrap_or_else(|| {
                                        panic!(concat!("could not construct ", stringify!(#name), " of ", stringify!(#variant), " {:?}"), tree);
                                    })
                                    .as_tree()))
                            },
                        )
                    } else {
                        let option = options[0].clone();
                        (
                            quote::quote! { #option<'s> },
                            quote::quote! {
                                #option::<'s>::from(
                                    tree
                                        .children
                                        .iter()
                                        .find(|child| {
                                            child.is_tree()
                                            && child.as_tree().kind == TreeKind::#option
                                        })
                                        .unwrap_or_else(|| {
                                            panic!(concat!("could not construct ", stringify!(#name), " of ", stringify!(#variant), " {:?}"), tree);
                                        })
                                        .as_tree()
                                )
                            },
                        )
                    }
                }

                AstAttrFieldKind::Trees => {
                    if options.len() > 1 {
                        let field_name = name.to_string();
                        let mut new_name = variant.to_string();
                        new_name.push((field_name.as_bytes()[0] as char).to_uppercase().next().unwrap());
                        new_name.push_str(str::from_utf8(&field_name.as_bytes()[1..]).unwrap());
                        let new_enum = Ident::new(&new_name, name.span());
                        tys.push(quote::quote! {
                                #[derive(Debug)]
                                pub enum #new_enum<'s> {
                                    #(#options(#options<'s>)),*
                                }
                                impl<'s> #new_enum<'s> {
                                    pub fn from<'t: 's>(tree: &'t Tree) -> #new_enum<'s> {
                                        match tree.kind {
                                            #(TreeKind::#options => #new_enum::#options(#options::<'s>::from(tree)),)*
                                            _ => unreachable!("couldn't construct {} out of {:?}", stringify!(#new_enum), tree.kind),
                                        }
                                    }
                                }
                            });
                        (
                            quote::quote! { Vec<#new_enum<'s>> },
                            quote::quote! {
                                tree
                                    .children
                                    .iter()
                                    .filter(|child| {
                                        child.is_tree()
                                        && [#(TreeKind::#options,)*].contains(&child.as_tree().kind)
                                    })
                                    .map(|child| #new_enum::<'s>::from(child.as_tree()))
                                    .collect()
                            },
                        )
                    } else {
                        let option = options[0].clone();
                        (
                            quote::quote! { Vec<#option<'s>> },
                            quote::quote! {
                                tree
                                    .children
                                    .iter()
                                    .filter(|child| {
                                        child.is_tree()
                                        && child.as_tree().kind == TreeKind::#option
                                    })
                                    .map(|child| #option::<'s>::from(child.as_tree()))
                                    .collect()
                            },
                        )
                    }
                }
            };

            tys.push(quote::quote! {
                #[derive(Debug)]
                pub struct #variant<'s>(pub #decl);

                impl<'s> #variant<'s> {
                    pub fn from<'t: 's>(tree: &'t Tree<'s>) -> Self {
                        Self(#ctor)
                    }
                }
            });
        }
    }

    quote::quote! { #(#tys)* }.into_token_stream().into()
}
