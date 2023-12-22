use ridotto_macros::{field_name, node_kind};

mod typeck;

fn main() {
    tracing_subscriber::fmt::init();

    let src = r#"
type Atype {
    Michael {
        x: Int
    }
    Tomlinson
}

fn main(args: List[&String]) {
    let d = X { y, ..args }
    let a = Atype.Bjeif.Froosh {
        y,
        b: *47
    }
    let mut b = &Atype.Bjeif.Froosh {
        y: 32
        b: 47
    }
    match b {
        Atype.Bjief.Froosh { y, b: a, .. } if a == 47 => {
            bff
        }
        _ | Maybe { y, .. }  | Bool.False | Bool.True if y == 3 => {
            let x = (y + b)
            let (x, y) = (y, x)
            nopington
        }
    }
    //let x = Z { .. a, b }
}
"#;

    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap_or("sample.ridotto".into()))
        .unwrap();

    let src = "type Asdf";

    let sarce = r#"

    async builtin export func main() -> (Void -> &Void) {
        let x = await a - b
        let x = a - 2
        let y = if 3 {
            let b = c
            c
        } else { nope }
    }
    
type Atype {
    y: bob.frob[i.do.not.like.green.eggs.and.ham]
    Michael {
        x: Int
        JJJJJJjjj
    }
    Tomlinson = Joo
    ## a tingus, a tangus
    Boblinson
    func bob(bobarg: int) {
        let a = if bobarg == 0 {
            return
        }
         else {
            bob(bobarg - 1)
        }
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

    let x = 0.3e0;

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_ridotto::language())
        .unwrap();
    let tree = parser.parse(sarce, None).unwrap();
    println!("{:#?}", tree);

    let mut cursor = tree.walk();
    fn walk(sarce: &str, cursor: &mut tree_sitter::TreeCursor, indent: usize) {
        let indent_ = "|  ".repeat(indent);

        if (cursor.node().is_named() || cursor.field_name() == Some(field_name!("op")))
            && cursor.node().kind() != node_kind!("comment")
        {
            println!(
                "{indent_}{}{}{}",
                cursor.node().kind(),
                if matches!(
                    cursor.node().kind(),
                    node_kind!("ident") | node_kind!("docComment")
                ) {
                    format!(" {}", &sarce[cursor.node().byte_range()].trim())
                } else {
                    "".into()
                },
                cursor
                    .field_name()
                    .map(|name| format!(" ({})", name))
                    .unwrap_or("".into()),
            );
        }

        if cursor.node().kind() == node_kind!("ERROR") {
            println!(
                "{indent_}  OH SHITTTTT {:?}",
                &sarce[cursor.node().byte_range()]
            );
        }

        if cursor.goto_first_child() {
            walk(sarce, cursor, indent + 1);
            cursor.goto_parent();
        }

        while cursor.goto_next_sibling() {
            walk(sarce, cursor, indent);
        }
    }
    walk(sarce, &mut cursor, 0);
}
