// https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html

use logos::Logos;
use std::fmt::Debug;

ridotto_macros::tree_kind!();

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
    #[token("type")] Type,
    #[token("where")] Where,
    #[token("=")] Equal,
    #[token(":")] Colon,
    #[token("is")] Is,
    #[token("async")] Async,
    #[token("const")] Const,
    #[token("export")] Export,
    #[token("builtin")] Builtin,
    #[token("static")] Static,
    #[token("func")] Func,
    #[regex(r#"(0[box])?[0-9](\.[0-9])?(e[-+]?[0-9]+)?"#)] Number,
    #[regex("[0-9]+", priority = 2)] WholeNumber,
    #[token("-")] Minus,
    #[token("+")] Plus,
    #[token("!")] Exclam,
    #[token("~")] Tilde,
    #[token("@")] At,
    #[token("^")] Caret,
    #[token("await")] Await,
    #[token("yield")] Yield,
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
    #[token("in")] In,
    #[token("and")] And,
    #[token("&&")] DoubleAmp,
    #[token("or")] Or,
    #[token("||")] DoublePipe,
    #[token("true")] True,
    #[token("false")] False,
    #[regex(r#"'(\\.|[^'])+'"#)] String,
    #[token("match")] Match,
    #[token("if")] If,
    #[token("let")] Let,
    #[token("_")] Underscore,
    #[token("..")] DoubleDot,
    #[token("class")] Class,
    #[token("impl")] Impl,
    #[token("for")] For,
    #[token("else")] Else,
    #[regex("#[^#\n]*")] Comment,
    #[regex("##[^\n]*")] DocComment,

    Error,
    Eof,
}

impl<E> PartialEq<Token> for Option<Result<Token, E>> {
    fn eq(&self, other: &Token) -> bool {
        match self {
            Some(Ok(token)) => token == other,
            Some(Err(_)) => *other == Token::Error,
            None => *other == Token::Eof,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Tree {
    pub kind: TreeKind,
    pub children: Vec<Child>,
}

#[derive(Debug, Clone)]
pub enum Child {
    Token(Token),
    Tree(Tree),
}
