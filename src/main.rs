use crate::{
    compiler::{print_tokens, TokenType},
    scanner::Scanner,
};

mod ast;
mod compiler;
mod error;
mod parser;
mod scanner;
mod typeck;

const SOURCE: &str = r#"
type Math[lhs, rhs=lhs, output=lhs] does {
    fn add(lhs, rhs) -> output
    fn subtract(lhs, rhs) -> output
    fn multiply(lhs, rhs) -> output
    fn divide(lhs, rhs) -> output

    fn sqrt(lhs) -> lhs
    fn pow(mag: Int, lhs: lhs) -> lhs {
        Iter.inclusive(1, mag)
        |> Iter.fold(lhs, fn(acc, _) {
            acc * lhs
        })
    }
}

type Int does Math[Int] {
    builtin fn add(Int, Int) -> Int
    builtin fn subtract(Int, Int) -> Int
    builtin fn multiply(Int, Int) -> Int
    builtin fn divide(Int, Int) -> Int
    builtin fn sqrt(Int) -> Int
    builtin fn pow(mag: Int, Int) -> Int
}

type Float does Math[Float] {
    builtin fn add(Float, Float) -> Float
    builtin fn subtract(Float, Float) -> Float
    builtin fn multiply(Float, Float) -> Float
    builtin fn divide(Float, Float) -> Float
    builtin fn sqrt(Float) -> Float
    builtin fn pow(mag: Float, Float) -> Float
}

type Direction is {
    up
    down
    left
    right
}

type Option[t] is {
    some(t)
    none
}

type Result[t, e] is {
    ok(ok t)
    error(error e)
}

type Point[Math[n]] has {
    x: n
    y: n
} is {
    metric
    customary
} does {
    fn distance(a: Point, b: Point) -> Option[n] {
        if !(a.is_metric && b.is_metric || a.is_customary && b.is_customary) {
            Option.none
        }

        Option.some(Math.sqrt(
            Math.pow(2, a.x + b.x) + Math.pow(2, a.y + b.y)
        ))
    }
}
"#;

fn main() {
    //let src = SOURCE;
    let src = "type Pea[nut] has { a_huge_head: Bool fart: nut }";

    let mut scanner = Scanner::new(src);
    let mut tokens = vec![];
    while scanner.peek_token().type_ != TokenType::Eof {
        tokens.push(scanner.next_token());
    }
    print_tokens(&tokens);

    let ast = parser::parse(src).unwrap();
    println!("{:#?}", ast);
}
