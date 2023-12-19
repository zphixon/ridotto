use ridotto_macros::{node_kind, field_name};

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
    
    type Asdf
    func asdf() -> Asdf.Bsdf[Csdf] {}
    
    "#;

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_ridotto::language())
        .unwrap();
    let tree = parser.parse(sarce, None).unwrap();
    println!("{:#?}", tree);

    let mut cursor = tree.walk();
    fn walk(sarce: &str, cursor: &mut tree_sitter::TreeCursor, indent: usize) {
        let indent_ = "  ".repeat(indent);
        println!("{indent_}{:#?}", cursor.node());
        if cursor.node().kind() == node_kind!("ident") {
            println!("{indent_}  IDENT {:?}", &sarce[cursor.node().byte_range()]);
        }
        if cursor.field_name() == Some(field_name!("name")) {
            println!("{indent_}  NAME {:?}", &sarce[cursor.node().byte_range()]);
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
