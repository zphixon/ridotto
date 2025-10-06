use ariadne::{Label, Report, Source};

mod tokens;
mod tree;
mod parse;
mod typeck;

fn main() {
    tracing_subscriber::fmt::init();

    let _src = r#"
func parse_primary(scanner: &Scanner, depth: Usize) -> Result[Expr, Error] {
    let type_ = scanner.peek_token().type_
    match type_ {
        TokenType.LowerIdent => Ok(Expr.Variable {
            variable: consume_lower(scanner, depth)
        }),

        TokenType.UpperIdent => Ok(Expr.TypeName {
            type_: consume_upper(scanner, depth),
        }),

        TokenType.Int(_)
        | TokenType.Float(_)
        | TokenType.True
        | TokenType.False
        | TokenType.String => Ok(Expr.Literal {
            literal: consume(scanner, type_, depth),
        }),

        TokenType.LeftBrace => {
            let stmts = parse_brace_delimited_stmts(scanner, depth)
            Ok(Expr.Block { stmts: stmts })
        },

        TokenType.LeftParen => {
            let values = comma_delimited(
                scanner,
                depth,
                TokenType.LeftParen,
                TokenType.RightParen,
                parse_expr,
            )
            if values.len() == 1 {
                Ok(Expr.Paren {
                    expr: Box.new(values.pop().unwrap()),
                })
            } else {
                Ok(Expr.Tuple { values: values })
            }
        },

        _ => Err(RidottoError.expected_expression(scanner.peek_token())),
    }
}
    "#;

    let (tree, errors) = parse::parse(_src);
    println!("{:#?}", tree);

    let file = tree::File::from_tree(&tree);
    println!("{:#?}", file);

    if let Ok(file) = file {
        for item in file.contents {
            match item {
                tree::FileContents::FuncDecl(func_decl) => {
                    println!("cool function named {:?}", func_decl.name);
                    for param in func_decl.params {
                        println!("  with param {:?}", param);
                    }
                }
                tree::FileContents::TypeDecl(type_decl) => match type_decl.def {
                    Some(tree::TypeDeclDef::TypeDeclInner(type_decl_inner)) => {
                        println!("cool type named {:?}", type_decl.name);
                        for field in type_decl_inner.fields.iter() {
                            println!(
                                "  with field {:?} of type {:?}",
                                field.field.name, field.field.ty
                            );
                        }
                        for variant in type_decl_inner.variants.iter() {
                            println!(
                                "  with cool variant {:?} which is {:?}",
                                variant.name, variant.def
                            );
                        }
                        for method in type_decl_inner.methods.iter() {
                            println!("  with method {:?}", method.name);
                        }
                    }
                    Some(tree::TypeDeclDef::TypeDeclTupleVariant(tup)) => {
                        println!("cool tuple type named {:?}", type_decl.name);
                        for member in tup.members {
                            println!("  with member {:?}", member)
                        }
                    }
                    Some(tree::TypeDeclDef::TypeDeclAlias(type_decl_alias)) => {
                        println!(
                            "cool type alias named {:?} equal to {:?}",
                            type_decl.name, type_decl_alias.expr
                        )
                    }
                    None => {
                        println!("idk what you are");
                    }
                },
            }
        }
    };

    for (error, span) in errors {
        Report::build(
            ariadne::ReportKind::Error,
            ("sarce", std::ops::Range::from(span)),
        )
        .with_message(error)
        .with_label(Label::new(("sarce", std::ops::Range::from(span))).with_message("here"))
        .finish()
        .print(("sarce", Source::from(_src)))
        .unwrap();
    }
}
