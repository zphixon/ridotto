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
fn main(args: List[String]) {
    Math.sqrt(Math.powi(b.x - a.x, 23.45) + Math.powi(b.y - a.y, -2))
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
