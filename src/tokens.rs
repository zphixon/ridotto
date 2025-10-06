use logos::Logos;
use std::fmt::Debug;

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

impl From<Span> for std::ops::Range<usize> {
    fn from(value: Span) -> Self {
        value.start..value.end_excl
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
