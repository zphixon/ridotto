// https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html

use logos::Logos;
use std::{
    cell::Cell,
    fmt::Debug,
    ops::{Bound, RangeBounds},
};
use tracing::trace;

#[rustfmt::skip]
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Logos)]
#[logos(skip r"[\t ]+")]
pub enum TokenKind {
    #[token("[")] LSquare,
    #[token("]")] RSquare,
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("{")] LCurly,
    #[token("}")] RCurly,
    #[token("-")] Minus,
    #[token("->")] Arrow,
    #[token("=>")] FatArrow,
    #[token("&")] Amp,
    #[token("&&")] DoubleAmp,
    #[token("&=")] AmpEqual,
    #[token(".")] Dot,
    #[token("..")] DoubleDot,
    #[token(",")] Comma,
    #[token("=")] Equal,
    #[token("==")] DoubleEqual,
    #[token(":")] Colon,
    #[token(";")] Semicolon,
    #[token("+")] Plus,
    #[token("+=")] PlusEqual,
    #[token("!")] Exclam,
    #[token("!=")] ExclamEqual,
    #[token("~")] Tilde,
    #[token("~=")] TildeEqual,
    #[token("@")] At,
    #[token("^")] Caret,
    #[token("^=")] CaretEqual,
    #[token("*")] Star,
    #[token("*=")] StarEqual,
    #[token("/")] Slash,
    #[token("/=")] SlashEqual,
    #[token("%")] Percent,
    #[token("%=")] PercentEqual,
    #[token("<")] LAngle,
    #[token("<<")] DoubleLAngle,
    #[token("<=")] LessEqual,
    #[token("<>")] Diamond,
    #[token("<=>")] Spaceship,
    #[token(">")] RAngle,
    #[token(">>")] DoubleRAngle,
    #[token(">=")] GreaterEqual,
    #[token("|")] Pipe,
    #[token("|=")] PipeEqual,
    #[token("||")] DoublePipe,
    #[token("_")] Underscore,
    #[token("class")] ClassKw,
    #[token("impl")] ImplKw,
    #[token("async")] AsyncKw,
    #[token("const")] ConstKw,
    #[token("export")] ExportKw,
    #[token("builtin")] BuiltinKw,
    #[token("static")] StaticKw,
    #[token("func")] FuncKw,
    #[token("type")] TypeKw,
    #[token("where")] WhereKw,
    #[token("is")] IsKw,
    #[token("for")] ForKw,
    #[token("in")] InKw,
    #[token("match")] MatchKw,
    #[token("let")] LetKw,
    #[token("if")] IfKw,
    #[token("else")] ElseKw,
    #[token("and")] AndKw,
    #[token("or")] OrKw,
    #[token("await")] AwaitKw,
    #[token("yield")] YieldKw,
    #[token("break")] BreakKw,
    #[token("return")] ReturnKw,
    #[token("continue")] ContinueKw,
    #[token("self")] SelfKw,
    #[token("Self")] UpperSelfKw,

    #[token("true")] TrueKw,
    #[token("false")] FalseKw,
    #[regex(r#"(0[box])?[0-9](\.[0-9])?(e[-+]?[0-9]+)?"#)] Number,
    #[regex("[0-9]+", priority = 3)] WholeNumber,
    #[regex(r#"'(\\.|[^'])*'|"(\\.|[^"])*""#)] String,
    #[regex("#[^#\n]*")] Comment,
    #[regex("##[^\n]*")] DocComment,
    #[regex("_?[A-Z][a-zA-Z0-9_]*")] UpperIdent,
    #[regex("_?[a-z][a-zA-Z0-9_]*")] LowerIdent,

    #[regex("\n+")]
    Newline,
    Error,
    Eof,
}

const FUNC_MODIFIERS: &[TokenKind] = &[
    TokenKind::AsyncKw,
    TokenKind::ConstKw,
    TokenKind::BuiltinKw,
    TokenKind::StaticKw,
    TokenKind::ExportKw,
];

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Pos {
    line: usize,
    col: usize,
}

impl Debug for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

pub fn span_to_line_col(src: &str, span: Span) -> Pos {
    let mut line = 1;
    let mut col = 1;

    for (byte_i, byte) in src.bytes().enumerate() {
        if span.contains(byte_i) {
            break;
        }
        if byte == b'\n' {
            col = 1;
            line += 1;
            continue;
        }
        col += 1;
    }

    Pos { line, col }
}

#[derive(Clone, Copy, Default)]
pub struct Span {
    pub start: usize,
    pub end_excl: usize,
}

impl Span {
    pub fn contains(&self, i: usize) -> bool {
        self.start <= i && i < self.end_excl
    }
}

#[derive(Clone, Copy)]
pub struct Token<'src> {
    pub kind: TokenKind,
    pub span: Span,
    pub lexeme: &'src str,
    pub src: &'src str,
}

impl Default for Token<'_> {
    fn default() -> Self {
        Token {
            kind: TokenKind::Eof,
            span: Span::default(),
            lexeme: "",
            src: "",
        }
    }
}

impl Debug for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} {:?}",
            self.lexeme,
            span_to_line_col(self.src, self.span)
        )
    }
}

impl<'src> From<Option<Token<'src>>> for Token<'src> {
    fn from(value: Option<Token<'src>>) -> Self {
        match value {
            Some(st) => st,
            None => Token::default(),
        }
    }
}

pub mod tree {
    use super::*;

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
}

pub use tree::*;

pub struct Tree<'src> {
    pub kind: TreeKind,
    pub children: Vec<Child<'src>>,
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

pub fn parse(src: &str) -> Tree<'_> {
    let tokens = TokenKind::lexer(src)
        .spanned()
        .map(|(token, span)| Token {
            kind: match token {
                Ok(token) => token,
                _ => TokenKind::Error,
            },
            lexeme: &src[span.clone()],
            span: Span {
                start: match span.start_bound() {
                    Bound::Included(&start) => start,
                    _ => unreachable!(),
                },
                end_excl: match span.end_bound() {
                    Bound::Excluded(&end_excl) => end_excl,
                    _ => unreachable!(),
                },
            },
            src,
        })
        .filter(|token| token.kind != TokenKind::Comment)
        .collect::<Vec<_>>();

    let mut parser = Parser {
        tokens,
        src,
        pos: 0,
        ignore_newline: vec![true],
        fuel: Cell::new(256),
        events: vec![],
    };

    file(&mut parser);
    parser.build_tree()
}

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

#[derive(Debug)]
enum Event {
    Open { kind: TreeKind },
    Close,
    Advance,
}

#[derive(Debug)]
struct MarkOpened {
    index: usize,
}

#[derive(Debug, Clone, Copy)]
struct MarkClosed {
    index: usize,
}

struct Parser<'src> {
    tokens: Vec<Token<'src>>,
    src: &'src str,
    pos: usize,
    ignore_newline: Vec<bool>,
    fuel: Cell<u32>,
    events: Vec<Event>,
}

impl<'src> Parser<'src> {
    #[macros::call_tree(only_exit = true)]
    fn open(&mut self) -> MarkOpened {
        let mark = MarkOpened {
            index: self.events.len(),
        };
        self.events.push(Event::Open {
            kind: TreeKind::Error,
        });
        mark
    }

    #[macros::call_tree(show_exit = false)]
    fn close(&mut self, m: MarkOpened, kind: TreeKind) -> MarkClosed {
        self.events[m.index] = Event::Open { kind };
        self.events.push(Event::Close);
        MarkClosed { index: m.index }
    }

    #[macros::call_tree(show_exit = false)]
    fn open_before(&mut self, m: MarkClosed) -> MarkOpened {
        let new_m = MarkOpened { index: m.index };
        self.events.insert(
            m.index,
            Event::Open {
                kind: TreeKind::Error,
            },
        );
        new_m
    }

    #[macros::call_tree(show_exit = false)]
    fn advance(&mut self) {
        assert!(!self.eof());
        self.fuel.set(256);
        self.events.push(Event::Advance);
        self.pos += 1;
    }

    fn eof(&mut self) -> bool {
        if *self.ignore_newline.last().unwrap() {
            while self.pos != self.tokens.len() && self.tokens[self.pos].kind == TokenKind::Newline
            {
                self.pos += 1;
                self.events.push(Event::Advance); // hmmmmmmmmmmmmmmmmmmmmmmmmm
            }
        }
        self.pos == self.tokens.len()
    }

    fn nth_exactly(&self, lookahead: usize) -> Token<'src> {
        *self.tokens.get(self.pos + lookahead).unwrap_or(&Token {
            kind: TokenKind::Eof,
            span: Span {
                start: 0,
                end_excl: 0,
            },
            lexeme: "",
            src: self.src,
        })
    }

    fn nth(&mut self, lookahead: usize) -> Token<'src> {
        if self.fuel.get() == 0 {
            panic!("stuck");
        }
        if *self.ignore_newline.last().unwrap() {
            while !self.eof()
                && (self.pos + lookahead) < self.tokens.len()
                && self.tokens[self.pos + lookahead].kind == TokenKind::Newline
            {
                self.advance();
            }
        }
        self.fuel.set(self.fuel.get() - 1);
        self.nth_exactly(lookahead)
    }

    fn at(&mut self, kind: TokenKind) -> bool {
        self.nth(0).kind == kind
    }

    fn at_any<'t>(&mut self, any: impl IntoIterator<Item = &'t TokenKind>) -> bool {
        any.into_iter().any(|kind| self.at(*kind))
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    #[macros::call_tree]
    fn expect(&mut self, kind: TokenKind) {
        if self.eat(kind) {
            return;
        }
        eprintln!("expected {:?}, got {:?}", kind, self.nth(0));
    }

    fn advance_error(&mut self, error: &str) {
        let mark = self.open();
        eprintln!("{error}");
        self.advance();
        self.close(mark, TreeKind::Error);
    }

    #[macros::call_tree]
    fn maybe_doc(&mut self) -> Option<MarkClosed> {
        if self.at(TokenKind::DocComment) {
            let m = self.open();
            while self.eat(TokenKind::DocComment) {}
            Some(self.close(m, TreeKind::Doc))
        } else {
            None
        }
    }

    fn build_tree(self) -> Tree<'src> {
        let mut tokens = self.tokens.into_iter();
        let mut events = self.events;
        let mut stack = Vec::new();

        assert!(matches!(events.pop(), Some(Event::Close)));

        for event in events {
            match event {
                Event::Open { kind } => {
                    stack.push(Tree {
                        kind,
                        children: Vec::new(),
                    });
                }

                Event::Close => {
                    let tree = stack.pop().unwrap();
                    stack.last_mut().unwrap().children.push(Child::Tree(tree));
                }

                Event::Advance => {
                    let token = tokens.next().unwrap();
                    stack.last_mut().unwrap().children.push(Child::Token(token));
                }
            }
        }

        assert_eq!(stack.len(), 1);
        assert!(tokens.next().is_none());

        stack.pop().unwrap()
    }
}

#[macros::call_tree]
fn file(p: &mut Parser) {
    let m = p.open();

    while !p.eof() {
        let doc = p.maybe_doc();
        if p.eat(TokenKind::TypeKw) {
            type_decl(p, doc, true);
        } else if p.at(TokenKind::FuncKw) || p.at_any(FUNC_MODIFIERS) {
            func_decl(p, doc);
        } else {
            p.advance_error("Expected type declaration");
        }
    }

    p.close(m, TreeKind::File);
}

#[macros::call_tree]
fn type_decl(p: &mut Parser, doc: Option<MarkClosed>, top: bool) {
    let m = if let Some(doc) = doc {
        p.open_before(doc)
    } else {
        p.open()
    };

    type_name(p);
    if top && p.at(TokenKind::LSquare) {
        type_param_list(p);
    }
    if top && p.eat(TokenKind::WhereKw) {
        type_qual_list(p);
    }

    if p.eat(TokenKind::Equal) {
        let m = p.open();
        type_expr(p);
        p.close(m, TreeKind::TypeDeclAlias);
    } else if p.at(TokenKind::LCurly) {
        type_decl_inner(p);
    } else if p.eat(TokenKind::LParen) {
        let m = p.open();
        while !p.at(TokenKind::RParen) && !p.eof() {
            type_expr(p);
            if !p.at(TokenKind::RParen) {
                p.expect(TokenKind::Comma);
            }
        }
        p.eat(TokenKind::Comma);
        p.expect(TokenKind::RParen);
        p.close(m, TreeKind::TypeDeclTupleVariant);
    }

    p.close(m, TreeKind::TypeDecl);
}

#[macros::call_tree]
fn type_decl_inner(p: &mut Parser) {
    let m = p.open();

    p.expect(TokenKind::LCurly);

    let mut doc = p.maybe_doc();
    while p.at(TokenKind::LowerIdent) {
        let m = if let Some(doc) = doc {
            p.open_before(doc)
        } else {
            p.open()
        };
        type_annotated(p);
        p.close(m, TreeKind::TypeDeclField);
        p.eat(TokenKind::Comma);
        doc = p.maybe_doc();
    }

    while p.at(TokenKind::UpperIdent) {
        type_decl(p, doc, false);
        p.eat(TokenKind::Comma);
        doc = p.maybe_doc();
    }

    while p.at_any(FUNC_MODIFIERS) || p.at(TokenKind::FuncKw) {
        func_decl(p, doc);
    }

    p.expect(TokenKind::RCurly);

    p.close(m, TreeKind::TypeDeclInner);
}

#[macros::call_tree]
fn func_decl(p: &mut Parser, doc: Option<MarkClosed>) {
    let m = if let Some(doc) = doc {
        p.open_before(doc)
    } else {
        p.open()
    };

    while FUNC_MODIFIERS.contains(&p.nth(0).kind) {
        p.advance();
    }
    p.expect(TokenKind::FuncKw);
    p.expect(TokenKind::LowerIdent);

    if p.at(TokenKind::LSquare) {
        type_param_list(p);
    }

    p.expect(TokenKind::LParen);
    while !p.eof() && !p.at(TokenKind::RParen) {
        if p.at(TokenKind::SelfKw) {
            self_param(p);
        } else {
            type_annotated(p);
        }

        if !p.at(TokenKind::RParen) {
            p.expect(TokenKind::Comma);
        }
    }
    p.expect(TokenKind::RParen);

    if p.eat(TokenKind::Arrow) {
        type_expr(p);
    }

    if p.eat(TokenKind::WhereKw) {
        type_qual_list(p);
    }

    body(p);

    p.close(m, TreeKind::FuncDecl);
}

#[macros::call_tree]
fn type_qual_list(p: &mut Parser) {
    let m = p.open();
    while p.at(TokenKind::LowerIdent) {
        let m = p.open();
        type_var(p);
        p.expect(TokenKind::IsKw);
        type_expr(p);
        p.close(m, TreeKind::TypeQualifier);
        if !p.eat(TokenKind::Comma) && !p.at(TokenKind::LowerIdent) {
            break;
        }
    }
    p.close(m, TreeKind::TypeQualifierList);
}

#[macros::call_tree]
fn body(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::LCurly);
    while !p.eof() && !p.at(TokenKind::RCurly) {
        statement(p);
    }
    p.expect(TokenKind::RCurly);
    p.close(m, TreeKind::Body);
}

#[macros::call_tree]
fn statement(p: &mut Parser) {
    let m = p.open();

    if p.eat(TokenKind::LetKw) {
        binding(p);
    } else {
        expr(p, false);
    }

    p.close(m, TreeKind::Statement);
}

#[macros::call_tree]
fn binding(p: &mut Parser) {
    let m = p.open();
    pattern(p);
    p.expect(TokenKind::Equal);
    expr(p, false);
    p.close(m, TreeKind::Binding);
}

#[macros::call_tree]
fn pattern(p: &mut Parser) {
    let m = p.open();

    let _: MarkClosed = match p.nth(0).kind {
        TokenKind::Underscore => {
            p.expect(TokenKind::Underscore);
            p.close(m, TreeKind::PatternWildcard)
        }

        TokenKind::LowerIdent => {
            p.expect(TokenKind::LowerIdent);
            p.close(m, TreeKind::PatternIdent)
        }

        TokenKind::LParen => {
            p.expect(TokenKind::LParen);
            while !p.at(TokenKind::RParen) && !p.eof() {
                pattern(p);
                if !p.at(TokenKind::RParen) {
                    p.expect(TokenKind::Comma);
                }
            }
            p.eat(TokenKind::Comma);
            p.expect(TokenKind::RParen);
            p.close(m, TreeKind::PatternTuple)
        }

        TokenKind::UpperIdent => {
            type_name(p);
            if p.eat(TokenKind::LCurly) {
                while !p.at(TokenKind::RCurly) && !p.eof() {
                    let m = p.open();
                    if p.at(TokenKind::Underscore) {
                        let m2 = p.open();
                        p.advance();
                        p.close(m2, TreeKind::PatternWildcard);
                        p.close(m, TreeKind::PatternDestructureStructField);
                        break;
                    }
                    p.expect(TokenKind::LowerIdent);
                    if p.eat(TokenKind::Colon) {
                        pattern(p);
                    }
                    p.close(m, TreeKind::PatternDestructureStructField);
                    if !p.at(TokenKind::RCurly) {
                        p.expect(TokenKind::Comma);
                    }
                }
                p.eat(TokenKind::Comma);
                p.expect(TokenKind::RCurly);
                p.close(m, TreeKind::PatternDestructure)
            } else if p.eat(TokenKind::LParen) {
                while !p.at(TokenKind::RParen) && !p.eof() {
                    let m = p.open();
                    if p.at(TokenKind::Underscore) {
                        let m2 = p.open();
                        p.advance();
                        p.close(m2, TreeKind::PatternWildcard);
                        p.close(m, TreeKind::PatternDestructureTupleMember);
                        break;
                    }
                    pattern(p);
                    p.close(m, TreeKind::PatternDestructureTupleMember);
                    if !p.at(TokenKind::RParen) {
                        p.expect(TokenKind::Comma);
                    }
                }
                p.eat(TokenKind::Comma);
                p.expect(TokenKind::RParen);
                p.close(m, TreeKind::PatternDestructure)
            } else {
                p.close(m, TreeKind::PatternVariant)
            }
        }

        TokenKind::TrueKw
        | TokenKind::FalseKw
        | TokenKind::Number
        | TokenKind::WholeNumber
        | TokenKind::String => {
            p.advance();
            p.close(m, TreeKind::Literal)
        }

        _ => {
            p.advance_error("expected pattern");
            p.close(m, TreeKind::Error)
        }
    };
}

#[macros::call_tree]
fn expr(p: &mut Parser, ignore_newline: bool) {
    p.ignore_newline.push(ignore_newline);
    expr_rec(p, TokenKind::Newline);
    if !*p.ignore_newline.last().unwrap() {
        p.expect(TokenKind::Newline);
    }
    p.ignore_newline.pop().unwrap();
}

#[macros::call_tree]
fn expr_rec(p: &mut Parser, left: TokenKind) {
    let mut lhs = expr_delimited(p);

    while p.at_any(&[TokenKind::LParen, TokenKind::LSquare, TokenKind::Dot]) {
        let what = p.nth(0);
        let m = p.open_before(lhs);

        match what.kind {
            TokenKind::LParen => {
                arg_list(p);
                lhs = p.close(m, TreeKind::Call);
            }

            TokenKind::LSquare => {
                array_list(p);
                lhs = p.close(m, TreeKind::ArrayAccess);
            }

            TokenKind::Dot => {
                p.expect(TokenKind::Dot);
                if !p.eat(TokenKind::LowerIdent) || !p.eat(TokenKind::WholeNumber) {
                    // huh
                }
                lhs = p.close(m, TreeKind::FieldAccess);
            }

            _ => {}
        }
    }

    loop {
        let right = p.nth(0).kind;
        if right_binds_tighter(left, right) {
            let m = p.open_before(lhs);
            p.advance();
            expr_rec(p, right);
            lhs = p.close(m, TreeKind::Binary)
        } else {
            break;
        }
    }
}

fn right_binds_tighter(left: TokenKind, right: TokenKind) -> bool {
    use TokenKind::*;
    fn tightness(kind: TokenKind) -> Option<usize> {
        [
            [OrKw, DoublePipe].as_slice(),
            &[AndKw, DoubleAmp],
            &[
                LAngle,
                RAngle,
                GreaterEqual,
                LessEqual,
                DoubleEqual,
                ExclamEqual,
                Diamond,
                Spaceship,
                InKw,
            ],
            &[Caret, Amp, Pipe, DoubleLAngle, DoubleRAngle],
            &[Plus, Minus],
            &[Star, Slash, Percent],
        ]
        .iter()
        .position(|level| level.contains(&kind))
    }

    let Some(right_t) = tightness(right) else {
        return false;
    };
    let Some(left_t) = tightness(left) else {
        assert!(matches!(left, Eof | Newline));
        return true;
    };

    right_t > left_t
}

#[macros::call_tree]
fn expr_delimited(p: &mut Parser) -> MarkClosed {
    let m = p.open();

    use TokenKind::*;
    match p.nth(0).kind {
        TrueKw | FalseKw | Number | WholeNumber | String => {
            p.advance();
            p.close(m, TreeKind::Literal)
        }

        LowerIdent => {
            p.advance();
            p.close(m, TreeKind::Variable)
        }

        UpperIdent => {
            type_name(p);

            if p.eat(LParen) {
                while !p.at(RParen) && !p.eof() {
                    let m = p.open();
                    expr(p, true);
                    if !p.at(RParen) {
                        p.expect(Comma);
                    }
                    p.close(m, TreeKind::InstantiateTupleMember);
                }
                p.eat(Comma);
                p.expect(RParen);
            } else if p.eat(LCurly) {
                while !p.at(RCurly) && !p.eof() {
                    let m = p.open();
                    p.expect(LowerIdent);
                    p.expect(Colon);
                    expr(p, true);
                    if !p.at(RCurly) {
                        p.expect(Comma);
                    }
                    p.close(m, TreeKind::InstantiateField);
                }
                p.eat(Comma);
                p.expect(RCurly);
            }

            p.close(m, TreeKind::Instantiate)
        }

        SelfKw => {
            p.advance();
            p.close(m, TreeKind::SelfParam)
        }

        LParen => {
            p.expect(LParen);
            expr(p, true);

            if p.at(Comma) {
                while p.eat(Comma) {
                    if p.at(RParen) {
                        break;
                    }
                    expr(p, true);
                }
                p.expect(RParen);
                p.close(m, TreeKind::TupleLiteral)
            } else {
                p.expect(RParen);
                p.close(m, TreeKind::Paren)
            }
        }

        LSquare => {
            // duplicated hmmm
            p.expect(TokenKind::LSquare);
            while !p.eof() && !p.at(TokenKind::RSquare) {
                expr(p, true);
                if !p.at(TokenKind::RSquare) {
                    p.expect(TokenKind::Comma);
                } else {
                    p.eat(TokenKind::Comma);
                }
            }
            p.expect(TokenKind::RSquare);
            p.close(m, TreeKind::ArrayList)
        }

        Minus | Plus | Exclam | Amp | Tilde | At | Caret | AwaitKw | YieldKw => {
            p.advance();
            // not calling expr(false) here because that would require a
            // newline. just let our caller handle it
            expr_rec(p, TokenKind::Newline);
            p.close(m, TreeKind::Unary)
        }

        IfKw => if_expr(p, m),

        MatchKw => match_expr(p, m),

        _ => {
            if !p.eof() {
                p.advance();
            }
            p.close(m, TreeKind::Error)
        }
    }
}

#[macros::call_tree]
fn if_expr(p: &mut Parser, m: MarkOpened) -> MarkClosed {
    p.expect(TokenKind::IfKw);
    expr(p, true);

    p.ignore_newline.push(true);
    body(p);
    if p.eat(TokenKind::ElseKw) {
        let m = p.open();
        if p.at(TokenKind::IfKw) {
            if_expr(p, m);
        } else {
            body(p);
            p.close(m, TreeKind::ElseBody);
        }
    }
    p.ignore_newline.pop().unwrap();

    p.close(m, TreeKind::If)
}

#[macros::call_tree]
fn match_expr(p: &mut Parser, m: MarkOpened) -> MarkClosed {
    p.expect(TokenKind::MatchKw);
    expr(p, true);

    p.ignore_newline.push(true);
    p.expect(TokenKind::LCurly);

    while !p.at(TokenKind::RCurly) && !p.eof() {
        match_arm(p);
        if !p.at(TokenKind::RCurly) {
            p.expect(TokenKind::Comma);
        }
    }
    p.eat(TokenKind::Comma);

    p.expect(TokenKind::RCurly);
    p.ignore_newline.pop().unwrap();

    p.close(m, TreeKind::Match)
}

#[macros::call_tree]
fn match_arm(p: &mut Parser) {
    let m = p.open();

    pattern(p);
    p.expect(TokenKind::FatArrow);
    if p.at(TokenKind::LCurly) {
        body(p);
    } else {
        // we know to expect a comma (or newline?) above
        expr(p, true);
    }

    p.close(m, TreeKind::MatchArm);
}

#[macros::call_tree]
fn arg_list(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::LParen);
    while !p.eof() && !p.at(TokenKind::RParen) {
        expr(p, true);
        if !p.at(TokenKind::RParen) {
            p.expect(TokenKind::Comma);
        } else {
            p.eat(TokenKind::Comma);
        }
    }
    p.expect(TokenKind::RParen);
    p.close(m, TreeKind::ArgList);
}

#[macros::call_tree]
fn array_list(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::LSquare);
    while !p.eof() && !p.at(TokenKind::RSquare) {
        expr(p, true);
        if !p.at(TokenKind::RSquare) {
            p.expect(TokenKind::Comma);
        } else {
            p.eat(TokenKind::Comma);
        }
    }
    p.expect(TokenKind::RSquare);
    p.close(m, TreeKind::ArrayList);
}

#[macros::call_tree]
fn self_param(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::SelfKw);
    p.close(m, TreeKind::SelfParam);
}

#[macros::call_tree]
fn type_annotated(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::LowerIdent);
    p.expect(TokenKind::Colon);
    type_expr(p);
    p.close(m, TreeKind::TypeAnnotated);
}

#[macros::call_tree]
fn type_param_list(p: &mut Parser) {
    let m = p.open();

    p.expect(TokenKind::LSquare);
    while !p.at(TokenKind::RSquare) && !p.eof() {
        type_expr(p);
        if !p.at(TokenKind::RSquare) {
            p.expect(TokenKind::Comma);
        }
    }
    p.eat(TokenKind::Comma);
    p.expect(TokenKind::RSquare);

    p.close(m, TreeKind::TypeParamList);
}

#[macros::call_tree]
fn type_concrete(p: &mut Parser) {
    let m = p.open();
    type_name(p);
    if p.at(TokenKind::LSquare) {
        type_param_list(p);
    }
    p.close(m, TreeKind::TypeConcrete);
}

#[macros::call_tree]
fn type_var(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::LowerIdent);
    p.close(m, TreeKind::TypeVar);
}

#[macros::call_tree]
fn type_expr(p: &mut Parser) {
    let m = p.open();
    if p.at(TokenKind::UpperIdent) {
        type_concrete(p);
    } else if p.at(TokenKind::LowerIdent) {
        type_var(p);
    } else if p.at(TokenKind::LParen) {
        type_tuple(p);
    } else if p.at(TokenKind::FuncKw) {
        type_func(p);
    } else if p.at(TokenKind::Amp) {
        type_ref(p);
    } else if p.at(TokenKind::UpperSelfKw) {
        let m = p.open();
        p.expect(TokenKind::UpperSelfKw);
        p.close(m, TreeKind::TypeSelf);
    } else {
        p.advance_error("expected type expression");
    }
    p.close(m, TreeKind::TypeExpr);
}

#[macros::call_tree]
fn type_func(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::FuncKw);

    {
        let m = p.open();
        p.expect(TokenKind::LParen);
        while !p.eof() && !p.at(TokenKind::RParen) {
            if p.at(TokenKind::LowerIdent) {
                type_expr(p);
            } else {
                break;
            }
        }
        p.expect(TokenKind::RParen);
        p.close(m, TreeKind::TypeFuncParamList);
    }

    p.expect(TokenKind::Arrow);

    type_expr(p);

    p.close(m, TreeKind::TypeFunc);
}

#[macros::call_tree]
fn type_tuple(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::LParen);
    while !p.eof() && !p.at(TokenKind::RParen) {
        type_expr(p);
        if !p.at(TokenKind::RParen) {
            p.expect(TokenKind::Comma);
        }
    }
    p.eat(TokenKind::Comma);
    p.expect(TokenKind::RParen);
    p.close(m, TreeKind::TypeTuple);
}

#[macros::call_tree]
fn type_ref(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::Amp);
    type_expr(p);
    p.close(m, TreeKind::TypeRef);
}

#[macros::call_tree]
fn type_name(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::UpperIdent);
    while p.eat(TokenKind::Dot) {
        p.expect(TokenKind::UpperIdent);
    }
    p.close(m, TreeKind::TypeName);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn space_between_kws() {
        let src = "breakcontinue";
        assert_eq!(
            TokenKind::lexer(src).next().unwrap().unwrap(),
            TokenKind::LowerIdent
        );

        let src = "break continue";
        assert_eq!(
            TokenKind::lexer(src).next().unwrap().unwrap(),
            TokenKind::BreakKw
        );
    }

    #[test]
    fn single_tokens() {
        let tests = [("1", TokenKind::WholeNumber), ("1.1", TokenKind::Number)];
        for (src, typ) in tests {
            let mut lexer = TokenKind::lexer(src);
            let f = lexer.next().unwrap().unwrap();
            assert_eq!(f, typ);
        }
    }
}

macros::setup_trace!();
