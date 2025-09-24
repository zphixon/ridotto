mod parse;
mod typeck;

fn main() {
    tracing_subscriber::fmt::init();

    //runtime::example();

    let _src = r#"
type Atype {
    bungus: A,
    lungus: HUEhu
    Michael {
        x: Int
    }
    Tomlinson = Fungus,
    func bingle() {}
}
#
# woweeeeeee
func main(args: List[String], env: Map[String, String]) {
#    #let d = X { y, ..args },
#    let a = Atype.Bjeif.Froosh {
#        y,
#        b: @47
#    }
#    let b = &Atype.Bjeif.Froosh {
#        y: 32,
#        b: 47
#    }
#    $
#    match b {
#        Atype.Bjief.Froosh { y, a, .. } if a == 47 {
#            bff
#        }
#        _ | Maybe { y, .. }  | Bool.False | Bool.True if y == 3 {
#            let x = (y + b)
#            #let (x, y) = (y, x)
#            nopington
#        }
#    }
#    #let x = Z { .. a, b }
    let x = "jiofewaoji"
    print(-x)
}
func main(){
    a * 3 - 4
}
"#;

    //let src = std::fs::read_to_string(std::env::args().nth(1).unwrap_or("sample.ridotto".into()))
    //    .unwrap();

    //let src = "type Asdf";

    let _sarce = r#"

    #func m() {
    #    match m {
    #        Type.V1 { a, .. } {}
    #        _ if true {},
    #    }
    #}

#    async builtin export func main() -> (Void -> &Void) {
#        let x = await a - b
#        let x = a - 2
#        let y = if 3 {
#            let b = c
#            'c\''
#        } else { nope }
#    }
#    
type Atype {
    y: Bob.Frob[I.Do.Not.Like.Green.Eggs.And.Ham]
    Michael {
        x: Int
        JJJJJJjjj
    }
    Tomlinson = Joo
    # a tingus, a tangus
    Boblinson
    func bob(bobarg: Int, bobarg2: Float) {
        #let a = if bobarg == 0 {
        #    return
        #}
        # else {
        #    bob(bobarg - 1, bobarg2)
        #}
    }
}

func main() {
    let x = "fungus"
    print(x.upper(), 3, x.upper()[3])
}

    func asdf(x: int, b: y) -> Asdf.Bsdf[Csdf, Dsdf,] {
        let a = 3
        let b = 0.0e0

        let c = 3 - (a.e)[b]
        let c = 3 -  a.e [b]

        let c = 3 -  (3+4)[b]  * !e
        let c = 3 - ((3+4)[b]) * !e
        2 + 3

        let c = -  a[b]
        let c = - (a[b])
        let c = (-a)[b]
        let a = b + true
        let a = 3
    }
    
    "#;

    let tree = parse::parse(_src);
    println!("{:#?}", tree);

    let file = parse::File::from_tree(&tree);
    println!("{:#?}", file);

    for item in file.contents {
        match item {
            parse::FileContents::FuncDecl(func_decl) => {
                println!("cool function named {:?}", func_decl.name);
                for param in func_decl.params {
                    println!(
                        "  with argument {:?} of type {:?}",
                        param.param.name, param.param.ty
                    );
                }
            }
            parse::FileContents::TypeDecl(type_decl) => match type_decl.inner_alias {
                Some(parse::TypeDeclInnerAlias::TypeDeclInner(type_decl_inner)) => {
                    println!("cool type named {:?}", type_decl.def);
                    for field in type_decl_inner.fields.iter() {
                        println!("  with field {:?} of type {:?}", field.name, field.ty);
                    }
                    for variant in type_decl_inner.variants.iter() {
                        println!(
                            "  with cool variant {:?} which is {:?}",
                            variant.def, variant.inner_alias
                        );
                    }
                    for method in type_decl_inner.methods.iter() {
                        println!("  with method {:?}", method.name);
                    }
                }
                Some(parse::TypeDeclInnerAlias::TypeDeclAlias(type_decl_alias)) => {
                    println!(
                        "cool type alias named {:?} equal to {:?}",
                        type_decl.def, type_decl_alias.expr
                    )
                }
                None => {
                    println!("idk what you are");
                }
            },
        }
    }
}
