use crate::scan::{print_tokens, Scanner, TokenType};

mod ast;
mod error;
mod ns;
mod ns2;
mod parse;
mod parse2;
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

    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap_or("sample.ridotto".into()))
        .unwrap();

    let src = "type Asdf";

    let mut scanner = Scanner::new(&src);
    let mut tokens = vec![];
    while scanner.peek_token().type_ != TokenType::Eof {
        tokens.push(scanner.next_token());
    }
    print_tokens(&tokens);

    println!("{:#?}", parse2::Parser::parse(src));

    //match parse::parse(&src) {
    //    Ok(ast) => {
    //        println!("{:#?}", ast);

    //        match ns::analyze_ns(&ast) {
    //            Ok(repo) => repo.debug(),
    //            Err(err) => {
    //                println!("{:?}", err);
    //                err.report().print(ariadne::Source::from(&src)).unwrap();
    //            }
    //        }
    //    }

    //    Err(err) => {
    //        println!("{:?}", err);
    //        err.report().print(ariadne::Source::from(src)).unwrap();
    //    }
    //}
}
