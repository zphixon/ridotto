use crate::scan::{print_tokens, Scanner, TokenType};

mod ast;
mod error;
mod ns;
mod ns2;
mod parse;
mod scan;
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

    let src = std::fs::read_to_string("sample.ridotto").unwrap();

    let mut scanner = Scanner::new(&src);
    let mut tokens = vec![];
    while scanner.peek_token().type_ != TokenType::Eof {
        tokens.push(scanner.next_token());
    }
    print_tokens(&tokens);

    match parse::parse(&src) {
        Ok(ast) => println!("{:#?}", ast),
        Err(err) => {
            println!("{:?}", err);
            err.report().print(ariadne::Source::from(src)).unwrap();
        }
    }
}
