use crate::parse::{print_tokens, Scanner, TokenType};

mod ast;
mod error;
mod ns;
mod ns2;
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
    tracing::subscriber::set_global_default(
        tracing_subscriber::fmt()
            .with_max_level(tracing::Level::TRACE)
            .finish(),
    )
    .unwrap();

    let src = X;
    //let src = X;
    //let src = SOURCE;
    //let src = "type Point[Math[n]] has {
    //    x: n
    //    loong: n=Int
    //} is {
    //    metric
    //    customary
    //} does {
    //    fn distance(a: Point, b: Point) -> Option[n] {
    //    }
    //}";

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
    bita: Bit
    bitb: Bit
    bitc: Bit
    bitd: Bit
}

// elements of a set may themselves be sets
type Shape {
    Oval {
        eccentricity: Int
        Oval { minor_radius: Int }
        Circle { radius: Int }
    }

    Quadrilateral {
        sidea: Int
        sideb: Int
        sidec: Int
        sided: Int

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
    point: Point[Int]
}

type Point[Math[n]] {
    x: n
    y: n

    Metric
    Customary

    fn distance(a: Point, b: Point) -> Option[n] {}
}

type Option[t] {
    Some { value: t }
    None

    fn unwrap(self: Self) -> t {
        match self {
            Option.Some { value } => value,
            Option.None => panic("called unwrap on None value")
        }
    }
}

class Monad[a, b=Int[pog]] {
    fn wrap(a: a) -> Monad[a]
    fn map(m: Monad[a], f: Fn[a, Monad[b]]) -> Monad[b]

    fn wowee() -> Wow.Wee[A.B.C] {
        wowee
    }
}

"#;

const WOW: &str = r#"
type A
type B = A
type C {}
type D {
    E
}
"#;

const B: &str = r#"

type Option[t] {
    Some { value: t }
    None

    fn unwrap(self: Self) -> t {
        match self {
            Option.Some { value } => value,
            Option.None => panic("called unwrap on None value")
        }
    }
}

class Monad[a, b] {
    fn wrap(a: a) -> Monad[a]
    fn map(m: Monad[a], fn(a) -> Monad[b]) -> Monad[b]
}

impl Monad[a, b] for Option[a] {
    fn wrap(value: a) -> Option[a] {
        Option.Some { value }
    }

    fn map(self: Option[a], f: fn(a) -> Option[b]) -> Option[b] {
        match self {
            Option.Some { value } => f(value)
            Option.None => Option.None
        }
    }
}

impl Monad[a, b] for Result[a, e] {
    fn wrap(value: a) -> Result[a, e] {
        Result.Ok { value }
    }

    fn map(self: Result[a, e], f: fn(a) -> Result[b, e]) -> Result[b, e] {
        match self {
            Result.Ok { value } => f(value)
            Result.Err { err } => Result.Err { err }
        }
    }
}

class Iterator[t] {
    fn next(self: Iterator[t]) -> Option[t]

    fn map(self: Iterator[t], xform: fn(t) -> u) -> Map[t, u] {
        Map {
            iter: self
            xform
        }
    }
}

type Map[t, u] {
    iter: Iterator[t]
    xform: fn(t) -> u
}

impl Iterator[u] for Map[t, u] {
    fn next(self: Map[t, u]) -> Option[u] {
        match self.iter.next() {
            Some { some } => Some { (self.xform)(some) }
            None => None
        }
    }
}

"#;
