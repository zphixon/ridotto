use crate::parse::Token;

#[derive(Debug)]
pub enum InnerTypeSpec<'src> {
    Type {
        name: Token<'src>,
        inner: Vec<InnerTypeSpec<'src>>,
    },

    TypeVar {
        name: Token<'src>,
    },
}

#[derive(Debug)]
pub enum TypeSpec<'src> {
    Type {
        name: Token<'src>,
        inner: Vec<TypeSpec<'src>>,
    },

    TypeVar {
        name: Token<'src>,
        default: Option<InnerTypeSpec<'src>>,
    },
}

#[derive(Debug)]
pub struct Type<'src> {
    pub type_spec: TypeSpec<'src>,
    pub inner: TypeInner<'src>,
}

#[derive(Debug)]
pub enum TypeInner<'src> {
    Alias { type_spec: TypeSpec<'src> },
    Regular { regular: Regular<'src> },
}

#[derive(Debug)]
pub struct Regular<'src> {
    pub fields: Vec<Annotated<'src>>,
    pub variants: Vec<Type<'src>>,
    pub behaviors: Vec<Function<'src>>,
}

#[derive(Debug)]
pub enum MaybeAbstractFunction<'src> {
    AbstractFunction(FunctionHead<'src>),
    Function(Function<'src>),
}

#[derive(Debug)]
pub struct FunctionHead<'src> {
    pub builtin: bool,
    pub async_: bool,
    pub const_: bool,
    pub export: bool,
    pub name: Token<'src>,
    pub args: Vec<Annotated<'src>>,
    pub return_: Option<TypeSpec<'src>>,
}

#[derive(Debug)]
pub struct Function<'src> {
    pub head: FunctionHead<'src>,
    pub body: Vec<()>,
}

#[derive(Debug)]
pub struct Annotated<'src> {
    pub name: Token<'src>,
    pub type_spec: TypeSpec<'src>,
}

#[derive(Debug)]
pub struct Class<'src> {
    pub type_spec: TypeSpec<'src>,
    pub behaviors: Vec<MaybeAbstractFunction<'src>>,
}

#[derive(Debug)]
pub struct Impl<'src> {
    lt: std::marker::PhantomData<&'src ()>,
}

#[derive(Debug)]
pub enum Ast<'src> {
    Type(Type<'src>),
    Class(Class<'src>),
    Impl(Impl<'src>),
    Function(Function<'src>),
}

pub enum Point<N> {
    Metric { x: N, y: N },
    Customary { x: N, y: N },
}
