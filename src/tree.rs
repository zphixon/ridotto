use crate::tokens::*;
use std::fmt::Debug;

#[derive(Debug)]
pub enum Child<'src> {
    Token(Token<'src>),
    Tree(Tree<'src>),
}

impl Child<'_> {
    pub fn is_token(&self) -> bool {
        matches!(self, Child::Token(_))
    }

    pub fn as_token(&self) -> Token<'_> {
        match self {
            Child::Token(token) => *token,
            Child::Tree(tree) => panic!("called as_token on non-token {:?}", tree.kind),
        }
    }

    pub fn is_tree(&self) -> bool {
        matches!(self, Child::Tree(_))
    }

    pub fn as_tree(&self) -> &Tree<'_> {
        match self {
            Child::Tree(tree) => tree,
            Child::Token(token) => panic!("called as_tree on non-tree {:?}", token),
        }
    }
}

pub struct Tree<'src> {
    pub kind: TreeKind,
    pub children: Vec<Child<'src>>,
}

impl Tree<'_> {
    pub fn errors(&self) -> Vec<&Tree<'_>> {
        let mut errors = vec![];
        if self.kind == TreeKind::Error {
            errors.push(self);
        }
        for child in self.children.iter() {
            match child {
                Child::Tree(tree) => errors.extend(tree.errors()),
                _ => {}
            }
        }
        errors
    }

    pub fn span(&self) -> Span {
        let mut span = Span {
            start: usize::MAX,
            end_excl: 0,
        };

        for child in self.children.iter() {
            let child_span = match child {
                Child::Token(child) => child.span,
                Child::Tree(child) => child.span(),
            };
            if child_span.start < span.start {
                span.start = child_span.start;
            }
            if child_span.end_excl > span.end_excl {
                span.end_excl = child_span.end_excl;
            }
        }

        span
    }
}

impl Debug for Tree<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut dbg = f.debug_tuple(&format!("{:?}", self.kind));

        for child in self.children.iter() {
            match child {
                Child::Token(token) => {
                    dbg.field(token);
                }
                Child::Tree(tree) => {
                    dbg.field(tree);
                }
            }
        }

        dbg.finish()
    }
}

#[derive(PartialEq, Debug, macros::Ast)]
#[ast(
    Expr = (
        Paren, Literal, Variable, Binary, Unary, Call, FieldAccess,
        ArrayAccess, TupleLiteral, ArrayList, If, Match, SelfParam,
        Instantiate,
    ),
    Pattern = (
        PatternWildcard, PatternIdent, PatternVariant, PatternAlternate,
        PatternDestructure, PatternTuple, PatternArray, Literal,
    ),
)]
pub enum TreeKind {
    Error,

    #[ast(contents = tree*(FuncDecl, TypeDecl))]
    File,

    #[ast(text = token+(DocComment))]
    Doc,

    #[ast(name = token(LowerIdent), ty = tree(TypeExpr))]
    TypeAnnotated,

    #[ast(expr = tree(TypeRef, TypeConcrete, TypeVar, TypeName, TypeTuple, TypeFunc, TypeSelf))]
    TypeExpr,

    #[ast(kw = token(UpperSelfKw))]
    TypeSelf,

    #[ast(expr = tree(TypeExpr))]
    TypeRef,

    #[ast(name = tree(TypeName), params = tree?(TypeParamList))]
    TypeConcrete,

    #[ast(name = token(LowerIdent))]
    TypeVar,

    // struct TypeName(Vec<UpperIdent>)
    #[ast(name = token+(UpperIdent))]
    TypeName,

    #[ast(members = tree*(TypeExpr))]
    TypeTuple,

    #[ast(list = tree+(TypeExpr))]
    TypeParamList,

    #[ast(
        doc = tree?(Doc),
        name = tree(TypeName),
        params = tree?(TypeParamList),
        qual = tree?(TypeQualifierList),
        def = tree?(TypeDeclInner, TypeDeclAlias, TypeDeclTupleVariant),
    )]
    TypeDecl,

    #[ast(
        fields = tree*(TypeDeclField),
        methods = tree*(FuncDecl),
        variants = tree*(TypeDecl),
    )]
    TypeDeclInner,

    #[ast(doc = tree?(Doc), field = tree(TypeAnnotated))]
    TypeDeclField,

    #[ast(expr = tree(TypeExpr))]
    TypeDeclAlias,

    #[ast(members = tree*(TypeExpr))]
    TypeDeclTupleVariant,

    #[ast(qualifiers = tree+(TypeQualifier))]
    TypeQualifierList,

    #[ast(name = tree(TypeVar), aspect = tree(TypeExpr))]
    TypeQualifier,

    #[ast(
        params = tree(TypeFuncParamList),
        ret = tree?(TypeExpr),
    )]
    TypeFunc,

    #[ast(params = tree*(TypeExpr))]
    TypeFuncParamList,

    #[ast(
        doc = tree?(Doc),
        modifiers = token*(AsyncKw, ConstKw, ExportKw, BuiltinKw, StaticKw),
        name = token(LowerIdent),
        type_params = tree?(TypeParamList),
        qual = tree?(TypeQualifierList),
        params = tree*(SelfParam, TypeAnnotated),
        ret = tree?(TypeExpr),
        body = tree(Body),
    )]
    FuncDecl,

    #[ast(body = tree*(Statement))]
    Body,

    #[ast(inner = tree(Expr, Binding))]
    Statement,

    #[ast(value = token(Number, WholeNumber, String, TrueKw, FalseKw))]
    Literal,

    #[ast(values = tree*(Expr))]
    TupleLiteral,

    #[ast(value = token(LowerIdent))]
    Variable,

    #[ast(from = tree(Expr), field = token(LowerIdent, WholeNumber))]
    FieldAccess,

    #[ast(from = tree(Expr), access = tree(ArrayList))]
    ArrayAccess,

    #[ast(values = tree+(Expr))]
    ArrayList,

    #[ast(callee = tree(Expr), params = tree(ArgList))]
    Call,

    #[ast(values = tree*(Expr))]
    ArgList,

    #[ast(
        op = token(
            Star, Slash, Percent, Plus, Minus, Caret, Amp, Pipe, DoubleLAngle,
            DoubleRAngle, LAngle, RAngle, LessEqual, GreaterEqual, DoubleEqual,
            ExclamEqual, Diamond, Spaceship, InKw, AndKw, DoubleAmp, OrKw, DoublePipe,
        ),
        sides = tree+(Expr)
    )]
    Binary,

    #[ast(inner = tree(Expr))]
    Paren,

    #[ast(op = token(Minus, Plus, Exclam, Amp, Tilde, At, Caret, AwaitKw, YieldKw), inner = tree(Expr))]
    Unary,

    #[ast(name = tree(TypeName), members = tree*(InstantiateField, InstantiateTupleMember))]
    Instantiate,

    #[ast(name = token(LowerIdent), value = tree(Expr))]
    InstantiateField,

    #[ast(value = tree(Expr))]
    InstantiateTupleMember,

    #[ast(cond = tree(Expr), body = tree(Body), else_if = tree?(If, ElseBody))]
    If,

    #[ast(scrutinee = tree(Expr), arms = tree*(MatchArm))]
    Match,

    #[ast(pat = tree(Pattern), body = tree(Body, Expr))]
    MatchArm,

    #[ast(body = tree(Body))]
    ElseBody,

    #[ast(pat = tree(Pattern), value = tree(Expr))]
    Binding,

    #[ast(token = token(Underscore))]
    PatternWildcard,

    #[ast(ident = token(LowerIdent))]
    PatternIdent,

    #[ast(variant = tree(TypeName), members = tree*(Pattern))]
    PatternVariant,

    #[ast(alt = tree+(Pattern))]
    PatternAlternate,

    #[ast(name = tree(TypeName), fields = tree+(PatternWildcard, PatternDestructureStructField, PatternDestructureTupleMember))]
    PatternDestructure,

    #[ast(field = token(LowerIdent), pat = tree?(Pattern))]
    PatternDestructureStructField,

    #[ast(pat = tree(Pattern))]
    PatternDestructureTupleMember,

    #[ast(members = tree*(Pattern))]
    PatternTuple,

    #[ast(members = tree*(Pattern))]
    PatternArray,

    #[ast(token = token(SelfKw))]
    SelfParam,

    ClassDecl,
    ImplDecl,
}
