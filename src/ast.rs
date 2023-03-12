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
    Alias {
        type_spec: TypeSpec<'src>,
    },

    Regular {
        has: Option<Has<'src>>,
        is: Option<Is<'src>>,
        does: Option<Does<'src>>,
    },
}

#[derive(Debug)]
pub struct Has<'src> {
    pub annotated: Vec<Annotated<'src>>,
}

#[derive(Debug)]
pub struct Is<'src> {
    pub variants: Vec<Variant<'src>>,
}

#[derive(Debug)]
pub struct Variant<'src> {
    pub name: Token<'src>,
    pub inner: Vec<Annotated<'src>>,
}

#[derive(Debug)]
pub struct Does<'src> {
    functions: Vec<MaybeAbstractFunction<'src>>,
}

#[derive(Debug)]
pub enum MaybeAbstractFunction<'src> {
    AbstractFunction(AbstractFunction<'src>),
    Function(Function<'src>),
}

#[derive(Debug)]
pub struct AbstractFunction<'src> {
    pub builtin: bool,
    pub async_: bool,
    pub const_: bool,
    pub export: bool,
    pub name: Token<'src>,
    pub args: Vec<Annotated<'src>>,
}

#[derive(Debug)]
pub struct Function<'src> {
    pub builtin: bool,
    pub async_: bool,
    pub const_: bool,
    pub export: bool,
    pub name: Token<'src>,
    pub args: Vec<Annotated<'src>>,
    pub body: Vec<()>,
}

#[derive(Debug)]
pub struct Annotated<'src> {
    pub name: Token<'src>,
    pub type_spec: TypeSpec<'src>,
}

#[derive(Debug)]
pub enum Ast<'src> {
    Type(Type<'src>),
    Function(Function<'src>),
}
