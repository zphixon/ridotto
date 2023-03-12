use std::{collections::HashSet, rc::Rc};

struct TypeVar {
    name: String,
    is_anonymous: bool,
    default: Option<Rc<TypeVar>>,
}

struct Type {
    name: String,
    args: Vec<Rc<TypeVar>>,
    where_: Vec<Constraint>,
    does: Vec<Rc<Function>>,
    is: Option<Is>,
    has: Option<Has>,
}

enum Constraint {
    GenericTypeExistsForTypeVar {
        type_var: Rc<TypeVar>,
        generic_type: Rc<Type>,
    },
}

struct Function {
    name: String,
    keywords: HashSet<FunctionKeyword>,
    type_args: Vec<i32>,
    // fn divide(n, rhs) -> output
}

enum FunctionKeyword {
    Async,
    Builtin,
    Export,
    Const,
}

struct Is;

struct Has;

#[test]
fn x() {
    let tv_n = Rc::new(TypeVar {
        name: "n".into(),
        is_anonymous: true,
        default: None,
    });

    let tv_rhs = Rc::new(TypeVar {
        name: "rhs".into(),
        is_anonymous: false,
        default: Some(tv_n.clone()),
    });

    let tv_lhs = Rc::new(TypeVar {
        name: "lhs".into(),
        is_anonymous: false,
        default: Some(tv_n.clone()),
    });

    let math = Type {
        name: "Math".into(),
        args: vec![tv_n.clone(), tv_rhs, tv_lhs],
        where_: vec![],
        does: todo!(),
        is: None,
        has: None,
    };
}

/*
// Macros require a dependency on the Ast module.
// sorta like, in-crate proc macros, with a versioned ast

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
    some(_ t)
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
*/
