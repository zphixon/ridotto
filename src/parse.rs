// https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html

use logos::Logos;
use std::{
    fmt::{Debug, write},
    iter::Peekable,
    ops::{Bound, Deref, RangeBounds}, error,
};

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
    #[token(";")] Semicolon,
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

trait ToToken {
    fn to_token(&self) -> Token;
}

impl ToToken for Token {
    fn to_token(&self) -> Token {
        *self
    }
}

impl<T: ToToken> ToToken for Option<T> {
    fn to_token(&self) -> Token {
        match self {
            Some(token) => token.to_token(),
            None => Token::Eof,
        }
    }
}

impl<T: ToToken, E> ToToken for Result<T, E> {
    fn to_token(&self) -> Token {
        match self {
            Ok(token) => token.to_token(),
            Err(_) => Token::ErrorT,
        }
    }
}

impl ToToken for &SpannedToken<'_> {
    fn to_token(&self) -> Token {
        self.token
    }
}

impl ToToken for SpannedToken<'_> {
    fn to_token(&self) -> Token {
        self.token
    }
}

trait Lexeme {
    fn lexeme(&self) -> &str;
}

impl Lexeme for SpannedToken<'_> {
    fn lexeme(&self) -> &str {
        self.lexeme
    }
}

impl<'src> Lexeme for Option<SpannedToken<'src>> {
    fn lexeme(&self) -> &'src str {
        self.unwrap_or_default().lexeme
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
pub struct SpannedToken<'src> {
    pub token: Token,
    pub span: Span,
    pub lexeme: &'src str,
}

impl Default for SpannedToken<'_> {
    fn default() -> Self {
        SpannedToken {
            token: Token::Eof,
            span: Span::default(),
            lexeme: "",
        }
    }
}

impl Debug for SpannedToken<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.lexeme)
    }
}

impl<'src> From<Option<SpannedToken<'src>>> for SpannedToken<'src> {
    fn from(value: Option<SpannedToken<'src>>) -> Self {
        match value {
            Some(st) => st,
            None => SpannedToken {
                token: Token::Eof,
                span: Span::default(),
                lexeme: "",
            },
        }
    }
}

pub fn parse(src: &str) {
    let mut lexer: Peekable<_> = Token::lexer(src)
        .spanned()
        .filter(|(token, _)| token.to_token() != Token::Comment)
        .map(|(token, span)| SpannedToken {
            token: match token {
                Ok(token) => token,
                _ => Token::ErrorT,
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
        })
        .peekable();

    let mut treeeee = Vec::new();
    while lexer.peek().is_some() {
        treeeee.push(paren(&mut lexer));
    }
    for tree in treeeee {
        println!("{:?}", tree);
    }
}

// TreeKind approach
// start building a tree of type X
//     parse children
//     if child parse fails,
//         add the error as a child
//         attempt to recover

enum Tree<'src> {
    Error(Box<Tree<'src>>),
    Token(SpannedToken<'src>),
    Paren(Vec<Tree<'src>>),
    Nice(SpannedToken<'src>),
}

impl<'src> Tree<'src> {
    fn is_error(&self) -> bool {
        matches!(self, Tree::Error(_))
    }

    fn error_token(token: SpannedToken<'src>) -> Self {
        Tree::Error(Box::new(Tree::Token(token)))
    }

    fn error_tree(tree: Tree<'src>) -> Self {
        Tree::Error(Box::new(tree))
    }

    fn debug_rec(&self, level: usize) -> String {
        let mut s = "  ".repeat(level);

        match self {
            Tree::Error(error) => {
                s += "error\n";
                s += &error.debug_rec(level+1);
            },
            Tree::Token(token) => {
                s += &format!("{:?}\n", token.lexeme);
            },
            Tree::Paren(paren) => {
                s += "paren\n";
                for child in paren {
                    s += &child.debug_rec(level+1);
                }
            },
            Tree::Nice(_) => s += "nice\n",
        }

        s
    }
}

impl Debug for Tree<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.debug_rec(0))
    }
}

macro_rules! recover {
    ($lexer: ident, $($tok:ident),*) => {
        let _ = $lexer.next();
        while ![Token::Eof, $(Token::$tok,)*].contains(&$lexer.peek().to_token()) {
            let _ = $lexer.next();
        }
    };
}

// (nice, (nice, nice, nice, (nice), nice), nice, (nice))

#[macros::parser_function]
fn paren<'src, I: Iterator<Item = SpannedToken<'src>>>(lexer: &mut Peekable<I>) -> Tree<'src> {
    let mut children = Vec::new();

    let lparen = consume(lexer, Token::LParen);
    if lparen.is_error() {
        recover!(lexer, LParen, Comma);
        return lparen;
    }

    while lexer.peek().is_some() && lexer.peek().to_token() != Token::RParen {
        if lexer.peek().to_token() == Token::LParen {
            children.push(paren(lexer));
        } else if lexer.peek().to_token() == Token::LowerIdent {
            children.push(nice(lexer));
        } else {
            children.push(Tree::error_token(lexer.peek().copied().into()));
            recover!(lexer, RParen, Comma);
        }

        if lexer.peek().to_token() != Token::RParen {
            let comma = consume(lexer, Token::Comma);
            if comma.is_error() {
                children.push(comma);
                recover!(lexer, LowerIdent, LParen, RParen, Comma);
            }
        }
    }

    let rparen = consume(lexer, Token::RParen);
    if rparen.is_error() {
        children.push(rparen);
        recover!(lexer, LParen, Comma);
    }

    Tree::Paren(children)
}

#[macros::parser_function]
fn nice<'src, I: Iterator<Item = SpannedToken<'src>>>(lexer: &mut Peekable<I>) -> Tree<'src> {
    let lower = consume(lexer, Token::LowerIdent);

    if lower.is_error() {
        return lower;
    }

    let Tree::Token(nice) = lower else {
        return Tree::error_tree(lower);
    };

    if nice.lexeme != "nice" {
        return Tree::error_token(nice);
    }

    Tree::Nice(nice)
}

#[macros::parser_function]
fn consume<'src, I: Iterator<Item = SpannedToken<'src>>>(
    lexer: &mut Peekable<I>,
    token: Token,
) -> Tree<'src> {
    assert_ne!(token, Token::Eof);

    let next = lexer.next();
    if next.to_token() == token {
        Tree::Token(next.unwrap())
    } else {
        Tree::error_token(next.into())
    }
}

macros::setup_trace!();

//#[macros::parser_function(recover = [])]
//fn x<'src, I: Iterator<Item = SpannedToken<'src>>>(lexer: &mut Peekable<I>) -> Result<(), ()> {
//    Err(())
//}
