use crate::parse::Token;
use std::fmt::{Debug, Formatter};

/// top level item
#[derive(Debug)]
pub enum Item<'src> {
    ItemTypeDecl(TypeDecl<'src>),
    ItemClass(Class<'src>),
    #[allow(dead_code)]
    ItemImpl(Impl<'src>),
    ItemFunction(Function<'src>),
}

/// type declaration
pub struct TypeDecl<'src> {
    /// name of the type
    pub name: NameUppercase<'src>,
    /// arguments to the type
    pub type_args: Vec<TypeExpr<'src>>,
    /// inner definition of the type
    pub inner: TypeDeclInnerOrAlias<'src>,
}

pub enum TypeDeclInnerOrAlias<'src> {
    /// type X = Y
    TypeDeclAlias { alias: TypeExpr<'src> },
    /// type X { ... }
    TypeDeclInner { inner: TypeDeclInner<'src> },
}

pub struct TypeDeclInner<'src> {
    /// properties of a type
    pub fields: Vec<TypeAnnotated<'src>>,
    /// subtypes
    pub variants: Vec<TypeVariant<'src>>,
    /// cocnrete behaviors of the type
    pub behaviors: Vec<Function<'src>>,
}

pub struct TypeVariant<'src> {
    pub name: NameUppercase<'src>,
    pub inner: TypeDeclInnerOrAlias<'src>,
}

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

pub struct Function<'src> {
    pub head: FunctionHead<'src>,
    pub body: Vec<Stmt<'src>>,
}

pub struct Class<'src> {
    pub name: NameUppercase<'src>,
    pub type_args: Vec<TypeExpr<'src>>,
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
pub enum Stmt<'src> {
    #[allow(dead_code)]
    V(std::marker::PhantomData<&'src ()>),
}

pub struct TypeAnnotated<'src> {
    pub name: NameLowercase<'src>,
    pub type_: TypeExpr<'src>,
}

/// type expression
pub enum TypeExpr<'src> {
    /// Type
    Concrete { name: TypeName<'src> },
    /// n, n=Type
    TypeVar {
        name: TypeName<'src>,
        default: Option<TypeExprNoDefault<'src>>,
    },
    /// Type[...]
    /// maybe n[...]=?
    Instantiated {
        name: TypeName<'src>,
        type_args: Vec<TypeExpr<'src>>,
    },
    FnType {
        type_args: Vec<TypeExpr<'src>>,
        args: Vec<TypeExpr<'src>>,
        return_: Option<Box<TypeExpr<'src>>>,
    },
}

/// type expression without defaults allowed
///
/// inner part of a type variable which has already been defaulted
pub enum TypeExprNoDefault<'src> {
    /// Type
    Concrete { name: TypeName<'src> },
    /// n
    TypeVar { name: TypeName<'src> },
    /// Type[...], (n[...]?)
    Instantiated {
        name: TypeName<'src>,
        type_args: Vec<TypeExprNoDefault<'src>>,
    },
}

#[derive(Debug)]
pub enum TypeName<'src> {
    /// namespaced
    Namespace(Vec<NameUppercase<'src>>),
    /// uppercase type name
    TypeValue(NameUppercase<'src>),
    /// lowercase type var name
    TypeVar(NameLowercase<'src>),
}

impl TypeName<'_> {
    fn name(&self) -> String {
        match self {
            TypeName::Namespace(names) => {
                let mut s = String::new();
                for (i, name) in names.iter().enumerate() {
                    s += name.uppercase.lexeme;
                    if i + 1 != names.len() {
                        s += ".";
                    }
                }
                s
            }
            TypeName::TypeValue(upper) => upper.uppercase.lexeme.into(),
            TypeName::TypeVar(lower) => lower.lowercase.lexeme.into(),
        }
    }
}

#[derive(Debug)]
pub struct NameUppercase<'src> {
    pub uppercase: Token<'src>,
}

#[derive(Debug)]
pub struct NameLowercase<'src> {
    pub lowercase: Token<'src>,
}

impl Debug for TypeDecl<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut dbg = f.debug_struct("TypeDecl");
        dbg.field("name", &self.name.uppercase.lexeme);
        dbg.field("type_args", &self.type_args);
        match &self.inner {
            TypeDeclInnerOrAlias::TypeDeclAlias { alias } => {
                dbg.field("alias", alias);
            }
            TypeDeclInnerOrAlias::TypeDeclInner { inner } => {
                dbg.field("fields", &inner.fields);
                dbg.field("variants", &inner.variants);
                dbg.field("behaviors", &inner.behaviors);
            }
        }
        dbg.finish()
    }
}

impl Debug for TypeVariant<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut dbg = f.debug_struct("TypeVariant");
        dbg.field("name", &self.name.uppercase.lexeme);
        match &self.inner {
            TypeDeclInnerOrAlias::TypeDeclAlias { alias } => {
                dbg.field("alias", alias);
            }
            TypeDeclInnerOrAlias::TypeDeclInner { inner } => {
                dbg.field("fields", &inner.fields);
                dbg.field("variants", &inner.variants);
                dbg.field("behaviors", &inner.behaviors);
            }
        }
        dbg.finish()
    }
}

impl Debug for TypeExpr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeExpr::Concrete { name } => {
                let mut dbg = f.debug_struct("Concrete");
                dbg.field("name", &name.name());
                dbg.finish()
            }

            TypeExpr::TypeVar { name, default } => {
                let mut dbg = f.debug_struct("TypeVar");
                dbg.field("name", &name.name());
                if let Some(default) = default.as_ref() {
                    dbg.field("default", default);
                }
                dbg.finish()
            }

            TypeExpr::Instantiated { name, type_args } => f
                .debug_struct("Instantiated")
                .field("name", &name.name())
                .field("type_args", type_args)
                .finish(),

            TypeExpr::FnType {
                type_args,
                args,
                return_,
            } => {
                let mut dbg = f.debug_struct("FnType");
                dbg.field("type_args", type_args);
                dbg.field("args", args);
                if let Some(return_) = return_.as_ref() {
                    dbg.field("return_", return_);
                }
                dbg.finish()
            }
        }
    }
}

impl Debug for TypeExprNoDefault<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeExprNoDefault::Concrete { name } => f
                .debug_struct("Concrete")
                .field("name", &name.name())
                .finish(),

            TypeExprNoDefault::TypeVar { name } => f
                .debug_struct("TypeVar")
                .field("name", &name.name())
                .finish(),

            TypeExprNoDefault::Instantiated { name, type_args } => f
                .debug_struct("Instantiated")
                .field("name", &name.name())
                .field("type_args", type_args)
                .finish(),
        }
    }
}

impl Debug for TypeAnnotated<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypeAnnotated")
            .field("name", &self.name.lowercase.lexeme)
            .field("type_", &self.type_)
            .finish()
    }
}

impl Debug for Function<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("builtin", &self.head.builtin)
            .field("async_", &self.head.async_)
            .field("const_", &self.head.const_)
            .field("export", &self.head.export)
            .field("name", &self.head.name.lowercase.lexeme)
            .field("type_args", &self.head.type_args)
            .field("args", &self.head.args)
            .field("return_", &self.head.return_)
            .field("body", &self.body)
            .finish()
    }
}

impl Debug for FunctionHead<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionHead")
            .field("builtin", &self.builtin)
            .field("async_", &self.async_)
            .field("const_", &self.const_)
            .field("export", &self.export)
            .field("name", &self.name.lowercase.lexeme)
            .field("type_args", &self.type_args)
            .field("args", &self.args)
            .field("return_", &self.return_)
            .finish()
    }
}

impl Debug for Class<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Class")
            .field("name", &self.name.uppercase.lexeme)
            .field("type_args", &self.type_args)
            .field("behaviors", &self.behaviors)
            .finish()
    }
}
