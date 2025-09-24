// https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html

use logos::Logos;
use std::{
    cell::Cell,
    fmt::Debug,
    ops::{Bound, RangeBounds},
};

#[rustfmt::skip]
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Logos)]
#[logos(skip r"\s+")]
pub enum TokenKind {
    #[token("[")] LSquare,
    #[token("]")] RSquare,
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("{")] LCurly,
    #[token("}")] RCurly,
    #[token("-")] Minus,
    #[token("->")] Arrow,
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

    #[token("true")] TrueKw,
    #[token("false")] FalseKw,
    #[regex(r#"(0[box])?[0-9](\.[0-9])?(e[-+]?[0-9]+)?"#)] Number,
    #[regex("[0-9]+", priority = 3)] WholeNumber,
    #[regex(r#"'(\\.|[^'])*'|"(\\.|[^"])*""#)] String,
    #[regex("#[^#\n]*")] Comment,
    #[regex("##[^\n]*")] DocComment,
    #[regex("_?[A-Z][a-zA-Z0-9_]*")] UpperIdent,
    #[regex("_?[a-z][a-zA-Z0-9_]*")] LowerIdent,

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
    #[ast(Paren, Literal, Variable, Binary, Unary, Call)]
    pub enum TreeKind {
        Error,

        #[ast(contents = trees(FuncDecl, TypeDecl))]
        File,

        Doc,

        #[ast(name = token(LowerIdent), ty = tree(TypeExpr))]
        TypeAnnotated,

        #[ast(expr = tree(TypeRef, TypeConcrete, TypeVar, TypeName))]
        TypeExpr,

        #[ast(expr = tree(TypeExpr))]
        TypeRef,

        #[ast(name = tree(TypeName), qual = trees(TypeQualifierList))]
        TypeConcrete,

        #[ast(name = token(LowerIdent))]
        TypeVar,

        // struct TypeName(Vec<UpperIdent>)
        #[ast(name = tokens(UpperIdent))]
        TypeName,

        #[ast(list = trees(TypeExpr))]
        TypeQualifierList,
        FuncType,
        FuncTypeArg,

        #[ast(def = tree(TypeConcrete), inner_alias = tree(TypeDeclInner, TypeDeclAlias))]
        TypeDecl,

        #[ast(
            fields = trees(TypeAnnotated),
            methods = trees(FuncDecl),
            variants = trees(TypeDecl),
        )]
        TypeDeclInner,

        #[ast(expr = tree(TypeExpr))]
        TypeDeclAlias,

        TypeDeclVariant,
        TypeDeclWhereList,
        TypeDeclWhere,

        #[ast(name = token(LowerIdent), qual = trees(TypeQualifierList), params = trees(FuncParam), body = trees(Statement))]
        FuncDecl,

        #[ast(inner = tree($, Binding))]
        Statement,

        #[ast(value = token(Number, WholeNumber, String, TrueKw, FalseKw))]
        Literal,

        #[ast(value = token(LowerIdent))]
        Variable,

        #[ast(callee = tree($), params = tree(ArgList))]
        Call,

        #[ast(values = trees($))]
        ArgList,

        #[ast(
            op = token(
                Star, Slash, Percent, Plus, Minus, Caret, Amp, Pipe, DoubleLAngle,
                DoubleRAngle, LAngle, RAngle, LessEqual, GreaterEqual, DoubleEqual,
                ExclamEqual, Diamond, Spaceship, InKw,
            ),
            sides = trees($)
        )]
        Binary,

        #[ast(inner = tree($))]
        Paren,

        #[ast(op = token(Minus, Plus, Exclam, Amp, Tilde, At, Caret, AwaitKw, YieldKw), inner = tree($))]
        Unary,

        #[ast(pat = tree(Pattern), value = tree($))]
        Binding,

        // TODO make this tree(...)
        #[ast(inner = token(LowerIdent))]
        Pattern,

        #[ast(param = tree(TypeAnnotated))]
        FuncParam,

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

enum Event {
    Open { kind: TreeKind },
    Close,
    Advance,
}

struct MarkOpened {
    index: usize,
}

#[derive(Clone, Copy)]
struct MarkClosed {
    index: usize,
}

struct Parser<'src> {
    tokens: Vec<Token<'src>>,
    src: &'src str,
    pos: usize,
    fuel: Cell<u32>,
    events: Vec<Event>,
}

impl<'src> Parser<'src> {
    fn open(&mut self) -> MarkOpened {
        let mark = MarkOpened {
            index: self.events.len(),
        };
        self.events.push(Event::Open {
            kind: TreeKind::Error,
        });
        mark
    }

    fn close(&mut self, m: MarkOpened, kind: TreeKind) -> MarkClosed {
        self.events[m.index] = Event::Open { kind };
        self.events.push(Event::Close);
        MarkClosed { index: m.index }
    }

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

    fn advance(&mut self) {
        assert!(!self.eof());
        self.fuel.set(256);
        self.events.push(Event::Advance);
        self.pos += 1;
    }

    fn eof(&self) -> bool {
        self.pos == self.tokens.len()
    }

    fn nth(&self, lookeahead: usize) -> Token<'src> {
        if self.fuel.get() == 0 {
            panic!("stuck");
        }
        self.fuel.set(self.fuel.get() - 1);
        *self.tokens.get(self.pos + lookeahead).unwrap_or(&Token {
            kind: TokenKind::Eof,
            span: Span {
                start: 0,
                end_excl: 0,
            },
            lexeme: "",
            src: self.src,
        })
    }

    fn at(&mut self, kind: TokenKind) -> bool {
        self.nth(0).kind == kind
    }

    fn at_any<'t>(&mut self, any: impl IntoIterator<Item = &'t TokenKind>) -> bool {
        any.into_iter().any(|kind| *kind == self.nth(0).kind)
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

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

fn file(p: &mut Parser) {
    let mark = p.open();
    while !p.eof() {
        if p.eat(TokenKind::TypeKw) {
            type_decl(p);
        } else if p.at(TokenKind::FuncKw) || p.at_any(FUNC_MODIFIERS) {
            func_decl(p);
        } else {
            p.advance_error("Expected type declaration");
        }
    }
    p.close(mark, TreeKind::File);
}

fn type_decl(p: &mut Parser) {
    let m = p.open();
    type_concrete(p);
    if p.eat(TokenKind::Equal) {
        let m = p.open();
        type_expr(p);
        p.close(m, TreeKind::TypeDeclAlias);
    } else {
        type_decl_inner(p);
    }
    p.close(m, TreeKind::TypeDecl);
}

fn type_decl_inner(p: &mut Parser) {
    let m = p.open();

    p.expect(TokenKind::LCurly);

    while p.at(TokenKind::LowerIdent) {
        type_annotated(p);
        p.eat(TokenKind::Comma);
    }

    while p.at(TokenKind::UpperIdent) {
        type_decl(p);
        p.eat(TokenKind::Comma);
    }

    while p.at_any(FUNC_MODIFIERS) || p.at(TokenKind::FuncKw) {
        func_decl(p);
    }

    p.expect(TokenKind::RCurly);

    p.close(m, TreeKind::TypeDeclInner);
}

fn func_decl(p: &mut Parser) {
    let m = p.open();

    while FUNC_MODIFIERS.contains(&p.nth(0).kind) {
        p.advance();
    }
    p.expect(TokenKind::FuncKw);
    p.expect(TokenKind::LowerIdent);

    if p.at(TokenKind::LSquare) {
        type_qual(p);
    }

    p.expect(TokenKind::LParen);
    while !p.eof() && !p.at(TokenKind::RParen) {
        if p.at(TokenKind::LowerIdent) {
            func_param(p);
        } else {
            break;
        }
    }
    p.expect(TokenKind::RParen);

    if p.eat(TokenKind::Arrow) {
        type_expr(p);
    }

    p.expect(TokenKind::LCurly);
    while !p.eof() && !p.at(TokenKind::RCurly) {
        statement(p);
    }
    p.expect(TokenKind::RCurly);

    p.close(m, TreeKind::FuncDecl);
}

fn statement(p: &mut Parser) {
    let m = p.open();

    if p.eat(TokenKind::LetKw) {
        binding(p);
    } else {
        expr(p);
    }

    p.close(m, TreeKind::Statement);
}

fn binding(p: &mut Parser) {
    let m = p.open();
    pattern(p);
    p.expect(TokenKind::Equal);
    expr(p);
    p.close(m, TreeKind::Binding);
}

fn pattern(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::LowerIdent);
    p.close(m, TreeKind::Pattern);
}

fn expr(p: &mut Parser) {
    expr_rec(p, TokenKind::Eof);
}

fn expr_rec(p: &mut Parser, left: TokenKind) {
    let mut lhs = expr_delimited(p);

    while p.at_any(&[TokenKind::LParen, TokenKind::Dot]) {
        let what = p.nth(0);
        let m = p.open_before(lhs);

        match what.kind {
            TokenKind::LParen => {
                arg_list(p);
                lhs = p.close(m, TreeKind::Call);
            }

            TokenKind::Dot => {
                // something hmmm
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
            // etc
        ]
        .iter()
        .position(|level| level.contains(&kind))
    }

    let Some(right_t) = tightness(right) else {
        return false;
    };
    let Some(left_t) = tightness(left) else {
        assert_eq!(left, Eof);
        return true;
    };

    right_t > left_t
}

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
            // dot access hmmmmmmmm
            p.close(m, TreeKind::Variable)
        }

        LParen => {
            p.expect(LParen);
            expr(p);
            p.expect(RParen);
            p.close(m, TreeKind::Pattern)
        }

        Minus | Plus | Exclam | Amp | Tilde | At | Caret | AwaitKw | YieldKw => {
            p.advance();
            expr(p);
            p.close(m, TreeKind::Unary)
        }

        _ => {
            if !p.eof() {
                p.advance();
            }
            p.close(m, TreeKind::Error)
        }
    }
}

fn arg_list(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::LParen);
    while !p.eof() && !p.at(TokenKind::RParen) {
        expr(p);
    }
    p.expect(TokenKind::RParen);
    p.close(m, TreeKind::ArgList);
}

fn func_param(p: &mut Parser) {
    let m = p.open();
    type_annotated(p);

    if !p.at(TokenKind::RParen) {
        p.expect(TokenKind::Comma);
    }

    p.close(m, TreeKind::FuncParam);
}

fn type_annotated(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::LowerIdent);
    p.expect(TokenKind::Colon);
    type_expr(p);
    p.close(m, TreeKind::TypeAnnotated);
}

fn type_qual(p: &mut Parser) {
    let m = p.open();

    p.expect(TokenKind::LSquare);

    type_expr(p);
    while p.eat(TokenKind::Comma) {
        type_expr(p);
    }
    p.eat(TokenKind::Comma);

    p.expect(TokenKind::RSquare);

    p.close(m, TreeKind::TypeQualifierList);
}

fn type_concrete(p: &mut Parser) {
    let m = p.open();
    type_name(p);
    if p.at(TokenKind::LSquare) {
        type_qual(p);
    }
    p.close(m, TreeKind::TypeConcrete);
}

fn type_var(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::LowerIdent);
    if p.at(TokenKind::LSquare) {
        type_qual(p);
    }
    p.close(m, TreeKind::TypeVar);
}

fn type_expr(p: &mut Parser) {
    let m = p.open();
    if p.at(TokenKind::UpperIdent) {
        type_concrete(p);
    } else if p.at(TokenKind::LowerIdent) {
        type_var(p);
    } else if p.at(TokenKind::LParen) {
        // func_type_expr or tuple_type_expr
    } else if p.at(TokenKind::Amp) {
        type_ref(p);
    } else if p.at(TokenKind::SelfKw) {
        // ??
        p.expect(TokenKind::SelfKw);
    }
    p.close(m, TreeKind::TypeExpr);
}

fn type_ref(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::Amp);
    type_expr(p);
    p.close(m, TreeKind::TypeRef);
}

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
