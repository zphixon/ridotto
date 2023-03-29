use crate::parse::{print_tokens, Scanner, TokenType};

mod ast;
mod error;
mod parse;
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
    ok(ok: t)
    error(error: e)
}

type Args {
    dry_run: Bool

    Build {
        clean: Bool
    }

    Copy {
        source: String
        dest: String
    }
}

args = parse_args()
match args to Args.Build {
    args.dry_run
    args.clean
}

type Math[lhs, rhs=lhs, output=lhs] {
    builtin fn add(lhs, rhs): output
    builtin fn multiply(lhs, rhs): output
}

type Point[Math[n]] {
    x: n
    y: n
}

impl Math[Point[n]] {
    fn add(lhs: Point[n], rhs: Point[n]): Point[n] {
        Point {
            x: lhs.x + rhs.x
            y: lhs.y + rhs.y
        }
    }
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
    let src = "type Point[Math[n]] has {
        x: n
        loong: n=Int
    } is {
        metric
        customary
    } does {
        fn distance(a: Point, b: Point) -> Option[n] {
        }
    }";

    let mut scanner = Scanner::new(src);
    let mut tokens = vec![];
    while scanner.peek_token().type_ != TokenType::Eof {
        tokens.push(scanner.next_token());
    }
    print_tokens(&tokens);

    match parse::parse(src) {
        Ok(ast) => println!("{:#?}", ast),
        Err(err) => {
            println!("{:?}", err);
            err.report().print(ariadne::Source::from(src)).unwrap();
        }
    }
}

const X: &str = r#"

// a value is an instance of an element of a set. that set is the type.
// the type should describe valid elements of that set.

// declare a set, named Unit, with a single element, named Unit
type Unit

// delcare a set named Bit, with some elements Zero and One
type Bit {
    Zero
    One
}

// elements of a set may have properties
type Nibble {
    bit1: Bit
    bit2: Bit
    bit3: Bit
    bit4: Bit
}

// elements of a set may themselves be sets
type Shape {
    Oval {
        eccentricity: Int
        Oval { minor_radius: Int }
        Circle { radius: Int }
    }

    Quadrilateral {
        side1: Int
        side2: Int
        side3: Int
        side4: Int

        Rectangle {
            Rectangle
            Square
        }

        Rhombus {
            slant: Int
            RectangleRhombus
            SquareRhombus
        }
    }
}

type Applique {
    shape: Shape
    x: Int
    y: Int
}

type Option[t] {
    Some { value: t }
    None

    fn unwrap()
}


"#;
