use crate::scan::{print_tokens, Scanner, TokenType};

mod ast;
mod error;
mod ns;
mod ns2;
mod parse;
mod scan;
mod typeck;

fn main() {
    tracing::subscriber::set_global_default(
        tracing_subscriber::fmt()
            .with_max_level(tracing::Level::TRACE)
            .finish(),
    )
    .unwrap();

    //let src = std::fs::read_to_string("sample.ridotto").unwrap();

    let src = r#"
type Atype {
    Michael {
        x: Int
    }
    Tomlinson
}

fn main(args: List[String]) {
    Atype.Bjeif.Froosh {
        y: 32
        b: 47
    }
}
"#;

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
