mod parse;
mod typeck;

fn main() {
    tracing_subscriber::fmt::init();

    //runtime::example();

    let _src = r#"
type Atype {
    bungus: Tungus
    lungus: HUEhu
    Michael {
        x: Int
    }
    Tomlinson = Fungus,
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
}
#func main(){
#    a * 3 - 4
#}
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
    y: bob.frob[i.do.not.like.green.eggs._and.ham]
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

#    func asdf(x: int, b: y) -> Asdf.Bsdf[Csdf, Dsdf,] {
#        #let a = 3
#        #let b = 0.0e0
#
#        #let c = 3 - (a.e)[b]
#        #let c = 3 -  a.e [b]
#
#        #let c = 3 -  (3+4)[b]  * !e
#        #let c = 3 - ((3+4)[b]) * !e
#        #2 + 3
#
#        #let c = -  a[b]
#        #let c = - (a[b])
#        #let c = (-a)[b]
#        #let a = b + true
#        #let a = 3
#    }
    
    "#;

    let _x = 0.3e0;

    //let ts = parse::lex(src);
    //for token in ts {
    //    //println!("{:?}", token);
    //}

    //let mut bruh = parse::Token::lexer(src);
    //loop {
    //    let token = bruh.next();
    //    if token == parse::Token::Comment {
    //        continue;
    //    }

    //    println!(
    //        "{:?} {:?} {:?}",
    //        parse::span_to_line_col(src, bruh.span()),
    //        bruh.slice(),
    //        token
    //    );

    //    if token == parse::Token::Eof {
    //        break;
    //    }
    //}

    let tree = parse::parse(_src);
    println!("{:#?}", tree);

    let file = parse::File::from(&tree);
    //println!("{:#?}", file);

    for item in file.0 {
        match item {
            parse::FileContents::FuncDecl(func_decl) => {
                println!("cool function named {:?}", func_decl.name);
                for param in func_decl.params {
                    println!(
                        "  with argument {:?} of type {:?}",
                        param.0.name, param.0.ty
                    );
                }
            }
            parse::FileContents::TypeDecl(type_decl) => {
                println!("cool type named {:?}", type_decl.0.name);
                match type_decl.0.fields {
                    parse::TypeDeclInnerFields::TypeDeclFieldList(type_decl_field_list) => {
                        for field in type_decl_field_list.0 {
                            println!("  with field {:?} of type {:?}", field.name, field.ty);
                        }
                    }
                    parse::TypeDeclInnerFields::TypeDeclAlias(type_decl_alias) => {
                        println!("  alias equal to {:?}", type_decl_alias.0);
                    }
                }
            }
        }
    }
}
