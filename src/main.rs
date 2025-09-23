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

    for item in tree.children.iter() {
        let parse::Child::Tree(item) = item else {
            break;
        };
        match item.kind {
            parse::TreeKind::FuncDecl => {
                println!("cool function named {:?}", parse::FuncDecl::name(item));
                for param in parse::FuncDecl::params(item) {
                    let param = parse::FuncParam::param(param);
                    println!(
                        "  with argument {:?} of type {:?}",
                        parse::TypeAnnotated::name(param),
                        parse::TypeAnnotated::ty(param)
                    );
                }
            }

            parse::TreeKind::TypeDecl => {
                let inner = parse::TypeDecl::inner(item);

                println!("cool type named {:?}", parse::TypeDeclInner::name(inner));
                let fields = parse::TypeDeclInner::fields(inner);
                for field in parse::TypeDeclFieldList::fields(fields) {
                    println!(
                        "  with field {:?} of type {:?}",
                        parse::TypeAnnotated::name(field),
                        parse::TypeAnnotated::ty(field)
                    );
                }
            }

            _ => {}
        }
    }
}
