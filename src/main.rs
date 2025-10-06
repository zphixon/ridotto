mod parse;
mod typeck;

fn main() {
    tracing_subscriber::fmt::init();

    let _src = r#"
    func m() {
        match self.iter.next() {
            Some(some) => Some((self.xform)(some)),
            None => Chungus {
                bobungus: Tungulus {},
            }
        }
    }
    "#;

    let tree = parse::parse(_src);
    println!("{:#?}", tree);

    let file = parse::File::from_tree(&tree);
    println!("{:#?}", file);
    let file = file.unwrap();

    for item in file.contents {
        match item {
            parse::FileContents::FuncDecl(func_decl) => {
                println!("cool function named {:?}", func_decl.name);
                for param in func_decl.params {
                    println!("  with param {:?}", param);
                }
            }
            parse::FileContents::TypeDecl(type_decl) => match type_decl.def {
                Some(parse::TypeDeclDef::TypeDeclInner(type_decl_inner)) => {
                    println!("cool type named {:?}", type_decl.name);
                    for field in type_decl_inner.fields.iter() {
                        println!("  with field {:?} of type {:?}", field.name, field.ty);
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
                Some(parse::TypeDeclDef::TypeDeclTupleVariant(tup)) => {
                    println!("cool tuple type named {:?}", type_decl.name);
                    for member in tup.members {
                        println!("  with member {:?}", member)
                    }
                }
                Some(parse::TypeDeclDef::TypeDeclAlias(type_decl_alias)) => {
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
}
