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
}

#[rustfmt::skip]
#[derive(Clone, Copy, PartialEq, Eq, Debug, Logos)]
#[logos(skip r"\s+")]
pub enum Token {
    #[regex("_?[A-Z][a-zA-Z0-9_]*")] UpperIdent,
    #[regex("_?[a-z][a-zA-Z0-9_]*")] LowerIdent,
    #[token("[")] LSquare,
    #[token("]")] RSquare,
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("{")] LCurly,
    #[token("}")] RCurly,
    #[token("->")] Arrow,
    #[token("&")] Amp,
    #[token(".")] Dot,
    #[token(",")] Comma,
    #[token("type")] TypeKw,
    #[token("where")] WhereKw,
    #[token("=")] Equal,
    #[token(":")] Colon,
    #[token("is")] IsKw,
    #[token("async")] AsyncKw,
    #[token("const")] ConstKw,
    #[token("export")] ExportKw,
    #[token("builtin")] BuiltinKw,
    #[token("static")] StaticKw,
    #[token("func")] FuncKw,
    #[regex(r#"(0[box])?[0-9](\.[0-9])?(e[-+]?[0-9]+)?"#)] Number,
    #[regex("[0-9]+", priority = 2)] WholeNumber,
    #[token("-")] Minus,
    #[token("+")] Plus,
    #[token("!")] Exclam,
    #[token("~")] Tilde,
    #[token("@")] At,
    #[token("^")] Caret,
    #[token("await")] AwaitKw,
    #[token("yield")] YieldKw,
    #[token("*")] Star,
    #[token("/")] Slash,
    #[token("%")] Percent,
    #[token("|")] Pipe,
    #[token("<<")] DoubleLAngle,
    #[token(">>")] DoubleRAngle,
    #[token(">")] RAngle,
    #[token("<")] LAngle,
    #[token(">=")] GreaterEqual,
    #[token("<=")] LessEqual,
    #[token("==")] DoubleEqual,
    #[token("!=")] ExclamEqual,
    #[token("<>")] NotEqual,
    #[token("in")] InKw,
    #[token("and")] AndKw,
    #[token("&&")] DoubleAmp,
    #[token("or")] OrKw,
    #[token("||")] DoublePipe,
    #[token("true")] TrueKw,
    #[token("false")] FalseKw,
    #[regex(r#"'(\\.|[^'])+'"#)] String,
    #[token("match")] MatchKw,
    #[token("if")] IfKw,
    #[token("let")] LetKw,
    #[token("_")] Underscore,
    #[token("..")] DoubleDot,
    #[token("class")] ClassKw,
    #[token("impl")] ImplKw,
    #[token("for")] ForKw,
    #[token("else")] ElseKw,
    #[regex("#[^#\n]*")] Comment,
    #[regex("##[^\n]*")] DocComment,

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

#[derive(Debug, Clone)]
pub struct WithSpan<'src> {
    token: Token,
    span: Span,
    lexeme: &'src str,
}

#[derive(Debug, Clone)]
pub struct Tree<'src> {
    pub kind: TreeKind,
    pub children: Vec<Child<'src>>,
}

#[derive(Debug, Clone)]
pub enum Child<'src> {
    Token(WithSpan<'src>),
    Tree(Tree<'src>),
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

struct Parser<'src> {
    tokens: Vec<WithSpan<'src>>,
    src: &'src str,
    pos: usize,
    fuel: Cell<u32>,
    events: Vec<Event>,
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

    fn nth(&self, lookahead: usize) -> WithSpan {
        if self.fuel.get() == 0 {
            panic!("parser stuck");
        }
        self.fuel.set(self.fuel.get() - 1);
        self.tokens
            .get(self.pos + lookahead)
            .cloned()
            .unwrap_or(WithSpan {
                token: Token::Eof,
                span: 0..self.src.len(),
                lexeme: "",
            })
    }

    fn at(&self, token: Token) -> bool {
        self.nth(0).token == token
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
        }
        println!("expected {:?}, got {:?}", token, self.nth(0));
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
    use super::*;
    use Token::*;
    use TreeKind::*;

    pub fn file(parser: &mut Parser) {
        let mark = parser.open();
        while !parser.eof() {
            if parser.at(FuncKw) {
                func(parser);
            } else {
                parser.advance_with_error("expected an item");
            }
        }
        parser.close(mark, File);
    }

    fn func(parser: &mut Parser) {
        assert!(parser.at(FuncKw));
        let mark = parser.open();

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

    fn type_expr(parser: &mut Parser) {
        let mark = parser.open();

        if parser.at(UpperIdent) {
            normal_type(parser);
        } else if parser.at(LParen) {
            tuple_or_func_type(parser);
        } else if parser.at(LowerIdent) {
            parser.expect(LowerIdent);
        } else if parser.at(Amp) {
            parser.expect(Amp);
            type_expr(parser);
        }

        parser.close(mark, TypeExpr);
    }

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

    fn normal_type(parser: &mut Parser) {
        assert!(parser.at(UpperIdent));
        let mark = parser.open();

        parser.expect(UpperIdent);
        while !parser.eof() && parser.at(Dot) {
            parser.expect(Dot);
            parser.expect(UpperIdent);
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

    fn block(parser: &mut Parser) {
        parser.expect(LCurly);
        parser.expect(RCurly);
    }

    fn type_annotated(parser: &mut Parser) {
        assert!(parser.at(LowerIdent));
        let mark = parser.open();

        parser.expect(LowerIdent);
        parser.expect(Colon);
        type_expr(parser);

        parser.close(mark, TypeAnnotated);
    }
}
