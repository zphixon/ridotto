use indexmap::IndexMap;
use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{
    parenthesized,
    parse::{Parse, Parser},
    parse_macro_input,
    punctuated::Punctuated,
    Ident, ItemFn, Token,
};

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

#[derive(Debug)]
struct Multi {
    name: Ident,
    values: Vec<Ident>,
}

impl Parse for Multi {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;

        let _eq: Token![=] = input.parse()?;

        let content;
        let _paren = parenthesized!(content in input);
        let values = content
            .parse_terminated(Ident::parse, Token![,])?
            .into_iter()
            .collect::<Vec<_>>();

        Ok(Multi { name, values })
    }
}

#[derive(Debug)]
struct MultiAstAttr {
    multis: Vec<Multi>,
}

impl Parse for MultiAstAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(MultiAstAttr {
            multis: Punctuated::<Multi, Token![,]>::parse_terminated(input)?
                .into_iter()
                .collect(),
        })
    }
}

#[proc_macro_derive(Ast, attributes(ast))]
pub fn ast_derive(item: TokenStream) -> TokenStream {
    let item: syn::ItemEnum = parse_macro_input!(item);

    let mut aliases = IndexMap::<Ident, Vec<Ident>>::new();
    for attr in item.attrs.iter() {
        if attr.path().is_ident("ast") {
            match attr.parse_args::<MultiAstAttr>() {
                Ok(multis) => {
                    for multi in multis.multis {
                        aliases.insert(multi.name, multi.values);
                    }
                }
                Err(err) => {
                    return err.to_compile_error().into_token_stream().into();
                }
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

    for (name, values) in aliases.iter() {
        tys.push(quote::quote! {
            #[derive(Debug)]
            pub enum #name<'s> {
                #(#values(Box<#values<'s>>),)*
            }
            impl<'s> #name<'s> {
                pub fn from_tree<'t: 's>(tree: &'t Tree<'s>) -> Result<Self, FromTreeError<'t, 's>> {
                    match tree.kind {
                        #(TreeKind::#values => Ok(#name::#values(Box::new(#values::from_tree(tree)?))),)*
                        _ => Err(FromTreeError { tree, why: String::from("$: could not construct expr") }),
                    }
                }
            }
        });
    }

    for (variant, tree) in trees {
        let mut field_decls = Vec::new();
        let mut field_ctors = Vec::new();

        for field in tree.fields {
            let name = field.name;
            let field_name = name.to_string();
            let new_name = stringcase::pascal_case(&(variant.to_string() + "_" + &field_name));

            let new_enum = Ident::new(&new_name, name.span());

            let tree_type =
                if field.options.len() == 1 && !aliases.contains_key(&field.options[0].clone()) {
                    let single_non_expr = field.options[0].clone();
                    quote::quote! { #single_non_expr }
                } else if field.options.len() == 1 {
                    let alias_ident = field.options[0].clone();
                    quote::quote! { #alias_ident }
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
                    .filter(|option| !aliases.contains_key(option))
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

                if let Some(alias_name) = field
                    .options
                    .clone()
                    .into_iter()
                    .find(|option| aliases.contains_key(option))
                {
                    new_enum_variant_names.push(alias_name.clone());
                    new_enum_variant_decls.push(quote::quote! { #alias_name<'s> });

                    new_enum_ctors.extend(aliases[&alias_name].clone().into_iter().map(|alias_option| {
                        quote::quote! {
                            TreeKind::#alias_option => Ok(#new_enum::Expr(#alias_name::#alias_option(Box::new(#alias_option::<'s>::from_tree(tree)?))))
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
                .filter(|option| !aliases.contains_key(option))
                .collect::<Vec<_>>();
            if let Some(alias_name) = field
                .options
                .iter()
                .cloned()
                .find(|option| aliases.contains_key(option))
            {
                tree_token_kinds.extend(aliases[&alias_name].clone());
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

#[proc_macro]
pub fn setup_trace(_: TokenStream) -> TokenStream {
    quote::quote! {
        static INDENT: ::std::sync::atomic::AtomicUsize
            = ::std::sync::atomic::AtomicUsize::new(0);
    }
    .into_token_stream()
    .into()
}

struct CallTreeFlag {
    name: String,
    value: bool,
}

impl Parse for CallTreeFlag {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name = input.parse::<Ident>()?.to_string();
        let _eq: Token![=] = input.parse()?;
        let value: syn::LitBool = input.parse()?;

        Ok(CallTreeFlag {
            name,
            value: value.value,
        })
    }
}

#[proc_macro_attribute]
pub fn call_tree(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut input: ItemFn = parse_macro_input!(input);

    let mut show_exit = true;
    let mut only_exit = false;
    let Ok(args) = Punctuated::<CallTreeFlag, Token![,]>::parse_terminated.parse(args) else {
        return quote::quote! {
            compile_error!("incorrect syntax")
        }
        .into_token_stream()
        .into();
    };
    let args = args.into_iter().collect::<Vec<_>>();
    for arg in args {
        if arg.name == "show_exit" {
            show_exit = arg.value;
        }
        if arg.name == "only_exit" {
            only_exit = true;
        }
    }

    let input_name = input.sig.ident.clone();
    let inner = input.block.clone();

    let Some(parser) = input
        .sig
        .inputs
        .iter()
        .flat_map(|input| match input {
            syn::FnArg::Receiver(syn::Receiver { self_token, .. }) => {
                Some(quote::quote! { #self_token })
            }
            syn::FnArg::Typed(pat_type) => match pat_type.pat.as_ref() {
                syn::Pat::Ident(syn::PatIdent { ident, .. }) if ident.to_string() == "p" => {
                    Some(quote::quote! { #ident })
                }
                _ => None,
            },
        })
        .next()
    else {
        return quote::quote! {
            compile_error!("expected either self: Parser or p: &mut Parser as argument");
        }
        .into_token_stream()
        .into();
    };

    let non_parser_inputs = input
        .sig
        .inputs
        .iter()
        .flat_map(|input| match input {
            syn::FnArg::Receiver(_) => None,
            syn::FnArg::Typed(pat_type) => match pat_type.pat.as_ref() {
                syn::Pat::Ident(syn::PatIdent { ident, .. }) if ident.to_string() != "p" => {
                    Some(ident)
                }
                _ => None,
            },
        })
        .collect::<Vec<_>>();

    let inputs_format_spec = non_parser_inputs
        .clone()
        .into_iter()
        .map(|input| format!(" {}={{:?}}", input))
        .collect::<Vec<_>>()
        .join("");
    let inputs_format_args = quote::quote! { #(#non_parser_inputs,)* };

    input.block = Box::new(
        syn::parse2(
            quote::quote! {
                {
                    let level = INDENT.fetch_add(1, ::std::sync::atomic::Ordering::AcqRel);

                    if !#only_exit {
                        let indent = "  ".repeat(level);
                        trace!(
                            concat!("{}{} {:?} {:?}", #inputs_format_spec),
                            indent,
                            stringify!(#input_name),
                            #parser.nth_exactly(0),
                            #parser.ignore_newline,
                            #inputs_format_args
                        );
                    }

                    let mut inner = || {
                        #inner
                    };
                    let result = inner();

                    let level = INDENT.fetch_sub(1, ::std::sync::atomic::Ordering::AcqRel);

                    if #show_exit || #only_exit {
                        let indent = "  ".repeat(level - 1);
                        trace!(
                            "{}{} {:?} {:?} ret={:?}",
                            indent,
                            stringify!(#input_name),
                            #parser.nth_exactly(0),
                            #parser.ignore_newline,
                            result,
                        );
                    }

                    result
                }
            }
            .into_token_stream(),
        )
        .unwrap(),
    );

    quote::quote! {
        #input
    }
    .into_token_stream()
    .into()
}
