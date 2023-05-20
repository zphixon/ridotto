use crate::{error::RidottoError, scan::Token};
use std::fmt::{Debug, Formatter};

/// top level item
#[derive(Debug)]
pub enum Item<'src> {
    ItemTypeDecl(TypeDecl<'src>),
    ItemClass(Class<'src>),
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
    /// type X(...)
    TypeDeclTuple { fields: Vec<TypeExpr<'src>> },
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
    pub body: Vec<Stmt<'src>>,
}

#[derive(Debug)]
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
    pub class: TypeExpr<'src>,
    pub for_: TypeExpr<'src>,
    pub behaviors: Vec<Function<'src>>,
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Stmt<'src> {
    If {},
    For {},
    Match {
        discriminant: Expr<'src>,
        branches: Vec<MatchBranch<'src>>,
    },
    Expr(Expr<'src>),
    Binding {
        pattern: Pattern<'src>,
        value: Expr<'src>,
    },
}

impl Stmt<'_> {
    fn token(&self) -> Token {
        use Stmt::*;
        match self {
            If {} | For {} => todo!(),
            Match { discriminant, .. } => discriminant.token(),
            Expr(expr) => expr.token(),
            Binding { value, .. } => value.token(), // pattern
        }
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct MatchBranch<'src> {
    pub pattern: Pattern<'src>,
    pub guard: Option<Expr<'src>>,
    pub body: Expr<'src>,
}

#[derive(Debug)]
pub enum Pattern<'src> {
    Any,
    Binding {
        name: NameLowercase<'src>,
    },
    MutableBinding {
        name: NameLowercase<'src>,
    },
    Alternate {
        left: Box<Pattern<'src>>,
        right: Box<Pattern<'src>>,
    },
    StructDestructure {
        type_name: TypeName<'src>,
        partial: bool,
        bindings: Vec<StructFieldPattern<'src>>,
    },
    TupleDestructure {
        type_name: TypeName<'src>,
        partial: bool,
        bindings: Vec<Pattern<'src>>,
    },
    Tuple {
        bindings: Vec<Pattern<'src>>,
    },
    EnumVariant {
        type_name: TypeName<'src>,
    },
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum StructFieldPattern<'src> {
    Named {
        name: NameLowercase<'src>,
        value: Pattern<'src>,
    },
    Shorthand {
        name: NameLowercase<'src>,
    },
    MutableShorthand {
        name: NameLowercase<'src>,
    },
    Rest,
}

#[allow(dead_code)]
pub enum Expr<'src> {
    Block {
        stmts: Vec<Stmt<'src>>,
    },
    Literal {
        literal: Token<'src>,
    },
    Paren {
        expr: Box<Expr<'src>>,
    },
    GetFrom {
        object: Box<Expr<'src>>,
        name: Identifier<'src>,
    },
    //ArrayIndex
    TupleIndex {
        object: Box<Expr<'src>>,
        index: Token<'src>,
    },
    Call {
        callee: Box<Expr<'src>>,
        args: Vec<Expr<'src>>,
    },
    StructInstantiate {
        type_name: TypeName<'src>,
        values: Vec<StructField<'src>>,
    },
    TupleInstantiate {
        type_name: TypeName<'src>,
        values: Vec<Expr<'src>>,
    },
    Tuple {
        values: Vec<Expr<'src>>,
    },
    Unary {
        op: Token<'src>,
        rhs: Box<Expr<'src>>,
    },
    Binary {
        lhs: Box<Expr<'src>>,
        op: Token<'src>,
        rhs: Box<Expr<'src>>,
    },
    Variable {
        variable: NameLowercase<'src>,
    },
    TypeName {
        type_: NameUppercase<'src>,
    },
}

impl Expr<'_> {
    pub(crate) fn token(&self) -> Token {
        match self {
            Expr::Block { stmts } => stmts
                .get(0)
                .map(|stmt| stmt.token())
                .unwrap_or_else(|| Token::default()),
            Expr::Binary { lhs, .. } => lhs.token(),
            Expr::Call { callee, .. } => callee.token(),
            Expr::Literal { literal } => *literal,
            Expr::Paren { expr } => expr.token(),
            Expr::GetFrom { object, .. } => object.token(),
            Expr::TupleIndex { object, .. } => object.token(),
            Expr::StructInstantiate { type_name, .. } => type_name.token(),
            Expr::TupleInstantiate {
                type_name: name, ..
            } => name.token(),
            Expr::Tuple { values } => values
                .get(0)
                .map(|value| value.token())
                .unwrap_or_else(|| Token::default()),
            Expr::Unary { op, .. } => *op,
            Expr::Variable { variable } => variable.lowercase,
            Expr::TypeName { type_ } => type_.uppercase,
        }
    }
}

pub enum StructField<'src> {
    Named {
        name: NameLowercase<'src>,
        value: Expr<'src>,
    },
    Shorthand {
        name: NameLowercase<'src>,
    },
    Spread {
        value: Expr<'src>,
    },
}

pub enum Identifier<'src> {
    NameUppercase(NameUppercase<'src>),
    NameLowercase(NameLowercase<'src>),
}

#[derive(Debug)]
pub struct TypeAnnotated<'src> {
    pub name: NameLowercase<'src>,
    pub type_: TypeExpr<'src>,
}

/// type expression
pub enum TypeExpr<'src> {
    /// Type
    Concrete { name: TypeName<'src> },
    /// *Type
    Ptr { pointee: Box<TypeExpr<'src>> },
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
    /// fn[...](...) -> ...
    FnType {
        type_args: Vec<TypeExpr<'src>>,
        args: Vec<TypeExpr<'src>>,
        return_: Option<Box<TypeExpr<'src>>>,
    },
    /// (..., ..., ...)
    Tuple { inner: Vec<TypeExpr<'src>> },
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

    fn token(&self) -> Token<'_> {
        match self {
            TypeName::Namespace(ns) => ns.first().map(|upper| upper.uppercase).unwrap(),
            TypeName::TypeValue(name) => name.uppercase,
            TypeName::TypeVar(name) => name.lowercase,
        }
    }
}

pub enum ExprToTypeName<'src> {
    TypeName(TypeName<'src>),
    Not(Token<'src>),
}

impl<'src> TryFrom<&Expr<'src>> for ExprToTypeName<'src> {
    type Error = RidottoError;
    fn try_from(expr: &Expr<'src>) -> Result<Self, Self::Error> {
        use crate::scan::TokenType;

        // attempt to turn an Expr into a TypeName
        match expr {
            // the expr is {object}.{name}
            Expr::GetFrom { object, name } => match name {
                // name is Something
                Identifier::NameUppercase(from) => {
                    // attempt to turn object into a TypeName
                    match ExprToTypeName::try_from(object.as_ref())? {
                        // object is Something
                        ExprToTypeName::TypeName(TypeName::TypeValue(prev)) => {
                            Ok(ExprToTypeName::TypeName(TypeName::Namespace(vec![
                                prev,
                                NameUppercase {
                                    uppercase: from.uppercase,
                                },
                            ])))
                        }

                        // object is Something.Otherthing
                        ExprToTypeName::TypeName(TypeName::Namespace(mut names)) => {
                            Ok(ExprToTypeName::TypeName(TypeName::Namespace({
                                names.push(NameUppercase {
                                    uppercase: from.uppercase,
                                });
                                names
                            })))
                        }

                        // object is none of these things
                        ExprToTypeName::TypeName(TypeName::TypeVar(lower)) => {
                            Ok(ExprToTypeName::Not(lower.lowercase))
                        }
                        ExprToTypeName::Not(not) => Ok(ExprToTypeName::Not(not)),
                    }
                }

                // name is otherthing
                Identifier::NameLowercase(lower) => Ok(ExprToTypeName::Not(lower.lowercase)),
            },

            // the expr is Something
            Expr::TypeName { type_ } => Ok(ExprToTypeName::TypeName(TypeName::TypeValue(
                NameUppercase {
                    uppercase: type_.uppercase,
                },
            ))),

            _ => Err(RidottoError::expected_one_of(
                [TokenType::UpperIdent, TokenType::LowerIdent],
                expr.token(),
            )),
        }
    }
}

pub struct NameUppercase<'src> {
    pub uppercase: Token<'src>,
}

pub struct NameLowercase<'src> {
    pub lowercase: Token<'src>,
}

impl Debug for NameUppercase<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.uppercase.lexeme.fmt(f)
    }
}

impl Debug for NameLowercase<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.lowercase.lexeme.fmt(f)
    }
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
            TypeDeclInnerOrAlias::TypeDeclTuple { fields } => {
                dbg.field("fields", &fields);
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
            TypeDeclInnerOrAlias::TypeDeclTuple { fields } => {
                dbg.field("fields", fields);
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

            TypeExpr::Ptr { pointee } => f.debug_tuple("Ptr").field(pointee).finish(),

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

            TypeExpr::Tuple { inner } => {
                let mut dbg = f.debug_tuple("Tuple");
                for field in inner.iter() {
                    dbg.field(field);
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

impl Debug for Expr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary { lhs, op, rhs } => {
                f.debug_tuple(op.lexeme).field(lhs).field(rhs).finish()
            }

            Expr::Unary { op, rhs } => f.debug_tuple(op.lexeme).field(rhs).finish(),

            Expr::Call { callee, args } => f
                .debug_struct("Call")
                .field("callee", callee)
                .field("args", args)
                .finish(),

            Expr::Variable { variable } => f
                .debug_tuple("Variable")
                .field(&variable.lowercase.lexeme)
                .finish(),

            Expr::GetFrom { object, name } => {
                f.debug_tuple("GetFrom").field(object).field(name).finish()
            }

            Expr::TupleIndex { object, index } => f
                .debug_tuple("TupleIndex")
                .field(&index.lexeme)
                .field(object)
                .finish(),

            Expr::TypeName { type_ } => f
                .debug_tuple("TypeName")
                .field(&type_.uppercase.lexeme)
                .finish(),

            Expr::Literal { literal } => literal.lexeme.fmt(f),

            Expr::StructInstantiate { type_name, values } => {
                let mut dbg = f.debug_struct(&format!("Instantiate {}", type_name.name()));
                for field in values.iter() {
                    match field {
                        StructField::Named { name, value } => {
                            dbg.field(name.lowercase.lexeme, value);
                        }
                        StructField::Shorthand { name } => {
                            dbg.field(name.lowercase.lexeme, &name.lowercase.lexeme);
                        }
                        StructField::Spread { value } => {
                            dbg.field("(spread)", value);
                        }
                    }
                }
                dbg.finish()
            }

            Expr::TupleInstantiate { type_name, values } => {
                let mut dbg = f.debug_tuple(&format!("Instantiate {}", type_name.name()));
                for value in values.iter() {
                    dbg.field(value);
                }
                dbg.finish()
            }

            Expr::Block { stmts } => f.debug_struct("Block").field("body", stmts).finish(),

            Expr::Paren { expr } => f.debug_tuple("Paren").field(expr).finish(),

            Expr::Tuple { values } => {
                let mut dbg = f.debug_tuple("Tuple");
                for value in values.iter() {
                    dbg.field(value);
                }
                dbg.finish()
            }
        }
    }
}

impl Debug for Identifier<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::NameLowercase(lowercase) => lowercase.fmt(f),
            Identifier::NameUppercase(uppercase) => uppercase.fmt(f),
        }
    }
}
