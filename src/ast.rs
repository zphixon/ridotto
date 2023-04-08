use crate::parse::Token;

/// top level item
#[derive(Debug)]
pub enum Item<'src> {
    ItemTypeDecl(TypeDecl<'src>),
    ItemClass(Class<'src>),
    ItemImpl(Impl<'src>),
    ItemFunction(Function<'src>),
}

/// type declaration
#[derive(Debug)]
pub struct TypeDecl<'src> {
    /// name of the type
    pub name: NameUppercase<'src>,
    /// arguments to the type
    pub type_args: Vec<TypeExpr<'src>>,
    /// inner definition of the type
    pub inner: TypeDeclInnerOrAlias<'src>,
}

#[derive(Debug)]
pub enum TypeDeclInnerOrAlias<'src> {
    /// type X = Y
    TypeDeclAlias { alias: TypeExpr<'src> },
    /// type X { ... }
    TypeDeclInner { inner: TypeDeclInner<'src> },
}

#[derive(Debug)]
pub struct TypeDeclInner<'src> {
    /// properties of a type
    pub fields: Vec<TypeAnnotated<'src>>,
    /// subtypes
    pub variants: Vec<TypeDecl<'src>>,
    /// cocnrete behaviors of the type
    pub behaviors: Vec<Function<'src>>,
}

#[derive(Debug)]
pub struct FunctionHead<'src> {
    pub builtin: bool,
    pub async_: bool,
    pub const_: bool,
    pub export: bool,
    pub name: NameLowercase<'src>,
    pub type_args: Vec<TypeExpr<'src>>,
    pub args: Vec<TypeAnnotated<'src>>,
    pub return_: Option<TypeExpr<'src>>,
}

#[derive(Debug)]
pub struct Function<'src> {
    pub head: FunctionHead<'src>,
    pub body: Vec<()>,
}

#[derive(Debug)]
pub struct Class<'src> {
    pub type_args: TypeExpr<'src>,
    pub behaviors: Vec<MaybeAbstractFunction<'src>>,
}

#[derive(Debug)]
pub enum MaybeAbstractFunction<'src> {
    AbstractFunction(FunctionHead<'src>),
    Function(Function<'src>),
}

/// implementation of a class on a type
#[derive(Debug)]
pub struct Impl<'src> {
    lt: std::marker::PhantomData<&'src ()>,
}

#[derive(Debug)]
pub struct TypeAnnotated<'src> {
    pub name: NameLowercase<'src>,
    pub type_: TypeExpr<'src>,
}

/// type expression
#[derive(Debug)]
pub enum TypeExpr<'src> {
    /// Type, n, n=Type
    Concrete {
        name: TypeName<'src>,
        default: Option<TypeExprNoDefault<'src>>,
    },
    /// Type[...], n[...]
    /// maybe n[...]=?
    Instantiated {
        name: TypeName<'src>,
        type_args: Vec<TypeExpr<'src>>,
    },
}

/// type expression without defaults allowed
///
/// inner part of a type variable which has already been defaulted
#[derive(Debug)]
pub enum TypeExprNoDefault<'src> {
    /// Type, n
    Concrete { name: TypeName<'src> },
    /// Type[...], n[...]
    Instantiated {
        name: TypeName<'src>,
        type_args: Vec<TypeExprNoDefault<'src>>,
    },
}

#[derive(Debug)]
pub enum TypeName<'src> {
    /// uppercase type name
    TypeValue(NameUppercase<'src>),
    /// lowercase type var name
    TypeVar(NameLowercase<'src>),
}

#[derive(Debug)]
pub struct NameUppercase<'src> {
    pub uppercase: Token<'src>,
}

#[derive(Debug)]
pub struct NameLowercase<'src> {
    pub lowercase: Token<'src>,
}
