// https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html

use logos::{Logos, Span};
use std::{cell::Cell, fmt::Debug};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TreeKind {
    Error,

    File,
    Func,
    FuncHead,
    TypeExpr,
    ParamList,
    TypeAnnotated,
    NormalType,
    FuncType,
    TupleType,
    Block,
    Expr,
    Literal,
    Variable,
    Paren,
    Call,
    ArgList,
    Arg,
    ExprBinary,
    ArrayIndex,
    DotAccess,
    Unary,
    TypeDecl,
    TypeInner,
    TypeAlias,
    TypeProperty,
    TypeVariant,
}

#[rustfmt::skip]
#[derive(Clone, Copy, PartialEq, Eq, Debug, Logos)]
#[logos(skip r"\s+")]
pub enum Token {
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
    #[token(".")] Dot,
    #[token("..")] DoubleDot,
    #[token(",")] Comma,
    #[token("=")] Equal,
    #[token("==")] DoubleEqual,
    #[token(":")] Colon,
    #[token("+")] Plus,
    #[token("!")] Exclam,
    #[token("!=")] ExclamEqual,
    #[token("~")] Tilde,
    #[token("@")] At,
    #[token("^")] Caret,
    #[token("*")] Star,
    #[token("/")] Slash,
    #[token("%")] Percent,
    #[token("<")] LAngle,
    #[token("<<")] DoubleLAngle,
    #[token("<=")] LessEqual,
    #[token("<>")] NotEqual,
    #[token(">")] RAngle,
    #[token(">>")] DoubleRAngle,
    #[token(">=")] GreaterEqual,
    #[token("|")] Pipe,
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
    #[token("true")] TrueKw,
    #[token("false")] FalseKw,
    #[token("await")] AwaitKw,
    #[token("yield")] YieldKw,
    #[token("break")] BreakKw,
    #[token("return")] ReturnKw,
    #[token("continue")] ContinueKw,

    #[regex(r#"(0[box])?[0-9](\.[0-9])?(e[-+]?[0-9]+)?"#)] Number,
    #[regex("[0-9]+", priority = 2)] WholeNumber,
    #[regex(r#"'(\\.|[^'])+'"#)] String,
    #[regex("#[^#\n]*")] Comment,
    #[regex("##[^\n]*")] DocComment,
    #[regex("_?[A-Z][a-zA-Z0-9_]*")] UpperIdent,
    #[regex("_?[a-z][a-zA-Z0-9_]*")] LowerIdent,

    ErrorT,
    Eof,
}

impl<E> PartialEq<Token> for Option<Result<Token, E>> {
    fn eq(&self, other: &Token) -> bool {
        match self {
            Some(Ok(token)) => token == other,
            Some(Err(_)) => *other == Token::ErrorT,
            None => *other == Token::Eof,
        }
    }
}

impl<E> PartialEq<Token> for Option<&Result<Token, E>> {
    // guh
    fn eq(&self, other: &Token) -> bool {
        match self {
            Some(Ok(token)) => token == other,
            Some(Err(_)) => *other == Token::ErrorT,
            None => *other == Token::Eof,
        }
    }
}

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
    let mut line = 0;
    let mut col = 1;

    for (byte_i, byte) in src.bytes().enumerate() {
        if span.contains(&byte_i) {
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

#[derive(Clone)]
pub struct WithSpan<'src> {
    token: Token,
    span: Span,
    lexeme: &'src str,
}

#[derive(Clone)]
pub struct Tree<'src> {
    pub kind: TreeKind,
    pub children: Vec<Child<'src>>,
}

#[derive(Clone)]
pub enum Child<'src> {
    Token(WithSpan<'src>),
    Tree(Tree<'src>),
}

impl Tree<'_> {
    fn debug(&self, level: usize) -> String {
        let indent = "  ".repeat(level);

        let mut result = format!("{}{:?}\n", indent, self.kind);
        for child in self.children.iter() {
            result += &child.debug(level + 1);
        }

        result
    }
}

impl Child<'_> {
    fn debug(&self, level: usize) -> String {
        let indent = "  ".repeat(level);
        match self {
            Child::Token(token) => format!("{}{:?}\n", indent, token.lexeme),
            Child::Tree(tree) => tree.debug(level),
        }
    }
}

impl Debug for Tree<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.debug(0))
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

struct MarkClosed {
    index: usize,
}

impl Debug for MarkClosed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "MarkClosed")
    }
}

struct Parser<'src> {
    tokens: Vec<WithSpan<'src>>,
    src: &'src str,
    pos: usize,
    fuel: Cell<u32>,
    events: Vec<Event>,
}

impl Debug for Parser<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parser {{ .. }}")
    }
}

pub fn parse(src: &str) -> Tree {
    let mut parser = Parser {
        tokens: Token::lexer(src)
            .spanned()
            .filter_map(|(token, span)| match token {
                Ok(Token::Comment) => None,
                Ok(token) => Some(WithSpan {
                    token,
                    lexeme: &src[span.clone()],
                    span,
                }),
                _ => Some(WithSpan {
                    token: Token::ErrorT,
                    lexeme: &src[span.clone()],
                    span,
                }),
            })
            .collect(),
        pos: 0,
        src,
        fuel: Cell::new(256),
        events: vec![],
    };

    inner::file(&mut parser);

    parser.build_tree()
}

impl<'src> Parser<'src> {
    #[must_use]
    fn open(&mut self) -> MarkOpened {
        let mark = MarkOpened {
            index: self.events.len(),
        };
        self.events.push(Event::Open {
            kind: TreeKind::Error,
        });
        mark
    }

    fn open_before(&mut self, mark: MarkClosed) -> MarkOpened {
        let mark_opened = MarkOpened { index: mark.index };
        self.events.insert(
            mark.index,
            Event::Open {
                kind: TreeKind::Error,
            },
        );
        mark_opened
    }

    fn close(&mut self, mark: MarkOpened, kind: TreeKind) -> MarkClosed {
        self.events[mark.index] = Event::Open { kind };
        self.events.push(Event::Close);
        MarkClosed { index: mark.index }
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

    fn nth(&self, lookahead: usize) -> WithSpan<'src> {
        if self.fuel.get() == 0 {
            panic!("parser stuck");
        }
        self.fuel.set(self.fuel.get() - 1);
        self.tokens
            .get(self.pos + lookahead)
            .cloned()
            .unwrap_or(WithSpan {
                token: Token::Eof,
                span: if self.src.is_empty() {
                    0..1
                } else {
                    self.src.len() - 1..self.src.len()
                },
                lexeme: "",
            })
    }

    fn at(&self, token: Token) -> bool {
        self.nth(0).token == token
    }

    fn at_any(&self, tokens: &[Token]) -> bool {
        tokens.iter().any(|&token| self.at(token))
    }

    fn eat(&mut self, token: Token) -> bool {
        if self.at(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, token: Token) {
        if self.eat(token) {
            return;
        } else {
            let mark = self.open();
            self.close(mark, TreeKind::Error);
        }
        println!(
            "expected {:?}, got {} at {:?}",
            token,
            if self.eof() {
                "eof".into()
            } else {
                format!("{:?}", self.nth(0).lexeme)
            },
            span_to_line_col(self.src, self.nth(0).span),
        );
    }

    fn advance_with_error(&mut self, error: &str) {
        let mark = self.open();
        println!("{}", error);
        self.advance();
        self.close(mark, TreeKind::Error);
    }

    fn build_tree(self) -> Tree<'src> {
        let Parser {
            tokens, mut events, ..
        } = self;
        let mut tokens = tokens.into_iter();
        let mut stack = Vec::new();

        assert!(matches!(events.pop(), Some(Event::Close)));

        for event in events {
            match event {
                Event::Open { kind } => stack.push(Tree {
                    kind,
                    children: vec![],
                }),

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

        assert!(stack.len() == 1);
        assert!(tokens.next().is_none());

        stack.pop().unwrap()
    }
}

mod inner {
    macros::setup_trace!();

    use super::*;
    use Token::*;
    use TreeKind::*;

    const STARTS_FUNCTION: &[Token] = &[FuncKw, AsyncKw, ConstKw, ExportKw, BuiltinKw, StaticKw];
    #[macros::parser_traced]
    pub fn file(parser: &mut Parser) {
        let mark = parser.open();
        while !parser.eof() {
            if parser.at_any(STARTS_FUNCTION) {
                func_item(parser);
            } else if parser.at(TypeKw) {
                type_item(parser);
            } else if parser.at(DocComment) {
                parser.advance();
            } else {
                parser.advance_with_error("expected an item");
            }
        }
        parser.close(mark, File);
    }

    #[macros::parser_traced]
    fn type_item(parser: &mut Parser) {
        assert!(parser.at(TypeKw));
        let mark = parser.open();

        parser.expect(TypeKw);
        variant_or_inner_or_unit(parser);

        parser.close(mark, TypeDecl);
    }

    #[macros::parser_traced]
    fn type_variant(parser: &mut Parser) {
        assert!(parser.at(LCurly));
        let mark = parser.open();

        parser.expect(LCurly);
        while !parser.eof() && !parser.at(RCurly) {
            if parser.at(LowerIdent) {
                type_annotated(parser);
            } else if parser.at(UpperIdent) {
                variant_or_inner_or_unit(parser);
            } else if parser.at_any(STARTS_FUNCTION) {
                func_item(parser);
            } else {
                break;
            }
        }
        parser.expect(RCurly);

        parser.close(mark, TypeVariant);
    }

    #[macros::parser_traced]
    fn variant_or_inner_or_unit(parser: &mut Parser) {
        normal_type(parser);
        if parser.at(LCurly) {
            type_variant(parser);
        } else if parser.at(Equal) {
            let mark = parser.open();
            parser.expect(Equal);
            type_expr(parser);
            parser.close(mark, TypeAlias);
        }
    }

    const FUNC_MODS: &[Token] = &[AsyncKw, ConstKw, ExportKw, BuiltinKw, StaticKw];
    #[macros::parser_traced]
    fn func_item(parser: &mut Parser) {
        assert!(parser.at_any(STARTS_FUNCTION));
        let mark = parser.open();

        while !parser.eof() && parser.at_any(FUNC_MODS) {
            parser.advance();
        }
        parser.expect(FuncKw);
        parser.expect(LowerIdent);
        if parser.at(LParen) {
            param_list(parser);
        }
        if parser.eat(Arrow) {
            type_expr(parser);
        }
        if parser.at(LCurly) {
            block(parser);
        }

        parser.close(mark, Func);
    }

    #[macros::parser_traced]
    fn param_list(parser: &mut Parser) {
        assert!(parser.at(LParen));
        let mark = parser.open();

        parser.expect(LParen);
        while !parser.eof() && !parser.at(RParen) {
            if parser.at(LowerIdent) {
                type_annotated(parser);
                if !parser.at(RParen) {
                    parser.expect(Comma);
                }
            } else {
                break;
            }
        }
        parser.expect(RParen);

        parser.close(mark, ParamList);
    }

    #[macros::parser_traced]
    fn type_expr(parser: &mut Parser) {
        let mark = parser.open();

        if parser.at_any(&[UpperIdent, LowerIdent]) {
            normal_type(parser);
        } else if parser.at(LParen) {
            tuple_or_func_type(parser);
        } else if parser.at(Amp) {
            parser.expect(Amp);
            type_expr(parser);
        }

        parser.close(mark, TypeExpr);
    }

    #[macros::parser_traced]
    fn tuple_or_func_type(parser: &mut Parser) {
        assert!(parser.at(LParen));
        let mark = parser.open();

        let mut is_func = false;
        let mut is_tuple = false;

        parser.expect(LParen);
        while !parser.eof() && !parser.at(RParen) {
            type_expr(parser);
            if parser.at(Arrow) {
                parser.expect(Arrow);
                type_expr(parser);
                is_func = true;
                break;
            } else if parser.at(Comma) {
                parser.expect(Comma);
                is_tuple = true;
            }
        }
        parser.expect(RParen);

        if is_func && is_tuple {
            parser.advance_with_error("neither tuple nor function?");
            parser.close(mark, Error);
        } else if is_func {
            parser.close(mark, FuncType);
        } else if is_tuple {
            parser.close(mark, TupleType);
        } else {
            parser.advance_with_error("empty tuple");
            parser.close(mark, Error);
        }
    }

    #[macros::parser_traced]
    fn normal_type(parser: &mut Parser) {
        assert!(parser.at_any(&[UpperIdent, LowerIdent]));
        let mark = parser.open();

        if parser.at(UpperIdent) {
            parser.expect(UpperIdent);
            while !parser.eof() && parser.at(Dot) {
                parser.expect(Dot);
                parser.expect(UpperIdent);
            }
        } else {
            parser.expect(LowerIdent);
            while !parser.eof() && parser.at(Dot) {
                parser.expect(Dot);
                parser.expect(LowerIdent);
            }
        }

        if parser.at(LSquare) {
            parser.expect(LSquare);
            while !parser.eof() && !parser.at(RSquare) {
                type_expr(parser);
                if !parser.at(RSquare) {
                    parser.expect(Comma);
                }
            }
            parser.expect(RSquare);
        }

        parser.close(mark, NormalType);
    }

    #[macros::parser_traced]
    fn block(parser: &mut Parser) {
        assert!(parser.at(LCurly));
        let mark = parser.open();

        parser.expect(LCurly);
        while !parser.eof() && !parser.at(RCurly) {
            match parser.nth(0).token {
                LetKw => todo!(),
                ReturnKw => todo!(),
                BreakKw => todo!(),
                ContinueKw => todo!(),
                _ => {
                    expr(parser);
                }
            }
        }
        parser.expect(RCurly);

        parser.close(mark, Block);
    }

    const EXPR_FIRST: &[Token] = &[
        LowerIdent,
        UpperIdent,
        LParen,
        Number,
        WholeNumber,
        IfKw,
        AwaitKw,
        YieldKw,
        MatchKw,
        Star,
        Minus,
        Caret,
        Amp,
        At,
        Tilde,
        Exclam,
    ];
    #[macros::parser_traced]
    fn expr(parser: &mut Parser) {
        let mark = parser.open();
        expr_rec(parser, Eof);
        parser.close(mark, Expr);
    }

    #[macros::parser_traced]
    fn expr_rec(parser: &mut Parser, left: Token) {
        let Some(mut lhs) = expr_delimited(parser) else {
            return;
        };

        loop {
            match parser.nth(0).token {
                LParen => {
                    let mark = parser.open_before(lhs);
                    arg_list(parser);
                    lhs = parser.close(mark, Call);
                }

                LSquare => {
                    let mark = parser.open_before(lhs);
                    parser.expect(LSquare);
                    expr(parser);
                    parser.expect(RSquare);
                    lhs = parser.close(mark, ArrayIndex);
                }

                _ => break,
            }
        }

        while parser.at(LParen) {
            let mark = parser.open_before(lhs);
            arg_list(parser);
            lhs = parser.close(mark, Call);
        }

        loop {
            let right = parser.nth(0);
            if right_binds_tighter(left, right.token) {
                let mark = parser.open_before(lhs);
                parser.advance();
                expr_rec(parser, right.token);
                lhs = parser.close(mark, ExprBinary);
            } else {
                break;
            }
        }
    }

    #[macros::parser_traced]
    fn right_binds_tighter(left: Token, right: Token) -> bool {
        #[rustfmt::skip]
        fn tightness(kind: Token) -> Option<usize> {
            [
                [DoublePipe, OrKw].as_slice(),
                [OrKw, DoubleAmp].as_slice(),
                [LAngle, RAngle, GreaterEqual, LessEqual, DoubleEqual, ExclamEqual, NotEqual, InKw].as_slice(),
                [Caret, Amp, Pipe, DoubleLAngle, DoubleRAngle].as_slice(),
                [Plus, Minus].as_slice(),
                [Star, Slash, Percent].as_slice(),
            ].iter().position(|level| level.contains(&kind))
        }

        let Some(right_tight) = tightness(right) else {
            return false;
        };
        let Some(left_tight) = tightness(left) else {
            assert!(left == Eof);
            return true;
        };

        right_tight > left_tight
    }

    #[macros::parser_traced]
    fn expr_delimited(parser: &mut Parser) -> Option<MarkClosed> {
        let mark = parser.open();

        match parser.nth(0).token {
            Number | WholeNumber | String | TrueKw | FalseKw => {
                parser.advance();
                Some(parser.close(mark, Literal))
            }

            LowerIdent => {
                parser.advance();

                let mut dot_access = false;
                while !parser.eof() && parser.at(Dot) {
                    dot_access = true;
                    parser.expect(Dot);
                    parser.expect(LowerIdent);
                }

                if dot_access {
                    Some(parser.close(mark, DotAccess))
                } else {
                    Some(parser.close(mark, Variable))
                }
            }

            LParen => {
                parser.expect(LParen);
                expr(parser);
                parser.expect(RParen);
                Some(parser.close(mark, Paren))
            }

            Star | Minus | Caret | Amp | At | Tilde | Exclam => {
                parser.advance();
                expr(parser);
                Some(parser.close(mark, Unary))
            }

            _ => None,
        }
    }

    #[macros::parser_traced]
    fn arg_list(parser: &mut Parser) {
        assert!(parser.at(LParen));
        let mark = parser.open();

        parser.expect(LParen);
        while !parser.eof() && !parser.at(RParen) {
            if parser.at_any(EXPR_FIRST) {
                let arg_mark = parser.open();
                expr(parser);
                if !parser.at(RParen) {
                    parser.expect(Comma);
                }
                parser.close(arg_mark, Arg);
            } else {
                break;
            }
        }
        parser.expect(RParen);

        parser.close(mark, ArgList);
    }

    #[macros::parser_traced]
    fn type_annotated(parser: &mut Parser) {
        assert!(parser.at(LowerIdent));
        let mark = parser.open();

        parser.expect(LowerIdent);
        parser.expect(Colon);
        type_expr(parser);

        parser.close(mark, TypeAnnotated);
    }
}
