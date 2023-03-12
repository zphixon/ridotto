use crate::compiler::Token;

#[derive(Debug)]
pub enum TypeSpec<'src> {
    Type {
        name: Token<'src>,
        inner: Vec<TypeSpec<'src>>,
    },

    TypeVar {
        name: Token<'src>,
    },
}

#[derive(Debug)]
pub enum DeclTypeSpec<'src> {
    Type {
        name: Token<'src>,
        inner: Vec<DeclTypeSpec<'src>>,
    },

    TypeVar {
        name: Token<'src>,
        default: Option<TypeSpec<'src>>,
    },
}

#[derive(Debug)]
pub struct Type<'src> {
    pub decl_type_spec: DeclTypeSpec<'src>,
    pub inner: TypeInner<'src>,
}

#[derive(Debug)]
pub enum TypeInner<'src> {
    Alias {
        decl_type_spec: DeclTypeSpec<'src>,
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
    pub variants: Vec<Token<'src>>,
}

#[derive(Debug)]
pub struct Does<'src> {
    todo: Token<'src>,
}

#[derive(Debug)]
pub struct Annotated<'src> {
    pub name: Token<'src>,
    pub type_spec: TypeSpec<'src>,
}

#[derive(Debug)]
pub enum Ast<'src> {
    Type(Type<'src>),
}
