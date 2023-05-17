use std::{
    fmt,
    hash::{Hash, Hasher},
    iter::Peekable,
};
use unicode_segmentation::{Graphemes, UnicodeSegmentation};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    /// type
    Type,
    /// class
    Class,
    /// where
    Where,
    /// async
    Async,
    /// builtin
    Builtin,
    /// export
    Export,
    /// const
    Const,
    /// fn
    Fn,
    /// if
    If,
    /// else
    Else,
    /// while
    While,
    /// for
    For,
    /// in
    In,
    /// break
    Break,
    /// continue
    Continue,
    /// return
    Return,
    /// assert
    Assert,
    /// impl
    Impl,
    /// let
    Let,
    /// var
    Var,

    /// _
    Underscore,
    /// [
    LeftBracket,
    /// ]
    RightBracket,
    /// (
    LeftParen,
    /// )
    RightParen,
    /// ,
    Comma,
    /// .
    Period,
    /// =
    Equal,
    /// {
    LeftBrace,
    /// }
    RightBrace,
    /// :
    Colon,
    /// !
    Exclam,
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Star,
    /// /
    Slash,
    /// %
    Percent,
    /// &&
    DoubleAmp,
    /// ||
    DoubleBar,
    /// &
    Amp,
    /// |
    Bar,
    /// ^
    Caret,
    /// ==
    DoubleEqual,
    /// !=
    ExclamEqual,
    /// <
    LeftAngle,
    /// >
    RightAngle,
    /// <=
    LeftAngleEqual,
    /// >=
    RightAngleEqual,
    /// <<
    DoubleLeftAngle,
    /// >>
    DoubleRightAngle,

    /// Type var or variable, e.g. `hello`
    LowerIdent,
    /// Type name, e.g. `Hello`
    UpperIdent,

    #[allow(dead_code)]
    String,
    True,
    False,
    Float(f64),
    Int(i64),

    Unknown,

    Eof,
}

impl TokenType {
    pub fn is_ident(self) -> bool {
        matches!(self, TokenType::LowerIdent | TokenType::UpperIdent)
    }

    pub fn starts_function(self) -> bool {
        matches!(
            self,
            TokenType::Fn
                | TokenType::Async
                | TokenType::Const
                | TokenType::Export
                | TokenType::Builtin
        )
    }
}

#[derive(Default, Debug, Clone, Copy)]
pub enum Pos {
    Source {
        line: usize,
        col: usize,
        byte: usize,
    },
    #[default]
    Builtin,
}

impl Pos {
    pub fn line(&self) -> usize {
        match self {
            Pos::Source { line, .. } => *line,
            _ => panic!("called line on builtin pos"),
        }
    }

    pub fn byte(&self) -> usize {
        match self {
            Pos::Source { byte, .. } => *byte,
            _ => panic!("called byte on builtin pos"),
        }
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pos::Source { line, col, .. } => write!(f, "{line}:{col}"),
            Pos::Builtin => write!(f, "builtin"),
        }
    }
}

/// Represents a token in source code.
///
/// Maintains a reference to the original source.
#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub type_: TokenType,
    pub lexeme: &'a str,
    pub pos: Pos,
}

impl Default for Token<'_> {
    fn default() -> Self {
        Token {
            type_: TokenType::LowerIdent,
            lexeme: "anon",
            pos: Pos::Builtin,
        }
    }
}

impl Hash for Token<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.lexeme.hash(state);
    }
}

impl Eq for Token<'_> {}

impl PartialEq for Token<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.type_ == other.type_ && self.lexeme == other.lexeme
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.type_ {
            TokenType::LowerIdent => write!(f, "{}", self.lexeme),
            TokenType::UpperIdent => write!(f, "{}", self.lexeme),
            TokenType::String => write!(f, "{}", self.lexeme),
            TokenType::Float(v) => write!(f, "{}", v),
            TokenType::Int(v) => write!(f, "{}", v),
            v => write!(f, "{:?}", v),
        }
    }
}

pub fn print_tokens(tokens: &[Token]) {
    let mut previous_line = 0;
    for token in tokens.iter() {
        println!(
            "{} {:?}{}",
            if token.pos.line() != previous_line {
                previous_line = token.pos.line();
                format!("{:>4}", token.pos.line())
            } else {
                "   |".into()
            },
            token.type_,
            if token.type_.is_ident() {
                format!(" {}", token.lexeme)
            } else {
                "".into()
            }
        );
    }
}

pub struct Scanner<'src> {
    graphemes: Peekable<Graphemes<'src>>,

    current: Option<Token<'src>>,

    line: usize,
    column: usize,

    source: &'src str,
    start_byte: usize,
    len_bytes: usize,
}

impl<'src> Scanner<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            graphemes: source.graphemes(true).peekable(),

            current: None,

            line: 1,
            column: 0,

            source,
            start_byte: 0,
            len_bytes: 0,
        }
    }

    fn next_grapheme(&mut self) -> &'src str {
        let Some(grapheme) = self.graphemes.next() else {
            return "";
        };

        self.column += 1;
        if grapheme == "\n" {
            self.column = 0;
            self.line += 1;
        }
        self.len_bytes += grapheme.as_bytes().len();

        grapheme
    }

    fn peek_grapheme(&mut self) -> &'src str {
        self.graphemes.peek().copied().unwrap_or("")
    }

    pub fn next_token(&mut self) -> Token<'src> {
        if let Some(current) = self.current.take() {
            return current;
        }

        self.next()
    }

    pub fn peek_token(&mut self) -> Token<'src> {
        if let Some(current) = self.current.clone() {
            return current;
        }

        self.current = Some(self.next());
        self.current.clone().unwrap()
    }

    fn slurp_whitespace(&mut self) {
        while let Some(true) = self
            .graphemes
            .peek()
            .map(|s| s.as_bytes().iter().all(u8::is_ascii_whitespace))
        {
            let _ = self.next_grapheme();
        }
    }

    fn next_type(&mut self) -> TokenType {
        self.slurp_whitespace();
        if self.peek_grapheme() == "" {
            return TokenType::Eof;
        }

        self.start_byte += self.len_bytes;
        self.len_bytes = 0;
        match self.next_grapheme() {
            "[" => TokenType::LeftBracket,
            "]" => TokenType::RightBracket,
            "(" => TokenType::LeftParen,
            ")" => TokenType::RightParen,
            "," => TokenType::Comma,
            "+" => TokenType::Plus,
            "-" => TokenType::Minus,
            "*" => TokenType::Star,
            "^" => TokenType::Caret,
            "%" => TokenType::Percent,
            "{" => TokenType::LeftBrace,
            "}" => TokenType::RightBrace,
            ":" => TokenType::Colon,
            "." => TokenType::Period,
            "_" => TokenType::Underscore,

            "/" => {
                if self.peek_grapheme() == "/" {
                    while !matches!(self.peek_grapheme(), "\r\n" | "\n" | "") {
                        self.next_grapheme();
                    }
                    self.next_type()
                } else {
                    TokenType::Slash
                }
            }

            "&" => {
                if self.peek_grapheme() == "&" {
                    self.next_grapheme();
                    TokenType::DoubleAmp
                } else {
                    TokenType::Amp
                }
            }

            "|" => {
                if self.peek_grapheme() == "|" {
                    self.next_grapheme();
                    TokenType::DoubleBar
                } else {
                    TokenType::Bar
                }
            }

            "!" => {
                if self.peek_grapheme() == "=" {
                    self.next_grapheme();
                    TokenType::ExclamEqual
                } else {
                    TokenType::Exclam
                }
            }

            "=" => {
                if self.peek_grapheme() == "=" {
                    self.next_grapheme();
                    TokenType::DoubleEqual
                } else {
                    TokenType::Equal
                }
            }

            ">" => {
                if self.peek_grapheme() == "=" {
                    self.next_grapheme();
                    TokenType::RightAngleEqual
                } else if self.peek_grapheme() == ">" {
                    self.next_grapheme();
                    TokenType::DoubleRightAngle
                } else {
                    TokenType::RightAngle
                }
            }

            "<" => {
                if self.peek_grapheme() == "=" {
                    self.next_grapheme();
                    TokenType::LeftAngleEqual
                } else if self.peek_grapheme() == "<" {
                    self.next_grapheme();
                    TokenType::DoubleLeftAngle
                } else {
                    TokenType::LeftAngle
                }
            }

            "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => self.number(),

            _ => {
                // TODO strings
                self.identifier_or_keyword()
            }
        }
    }

    fn next(&mut self) -> Token<'src> {
        let type_ = self.next_type();

        Token {
            type_,
            lexeme: self.lexeme(),
            pos: Pos::Source {
                line: self.line,
                col: self.column,
                byte: self.start_byte,
            },
        }
    }

    fn lexeme(&self) -> &'src str {
        if self.start_byte >= self.source.len()
            || self.start_byte + self.len_bytes >= self.source.len()
        {
            ""
        } else {
            &self.source[self.start_byte..self.start_byte + self.len_bytes]
        }
    }

    fn identifier_or_keyword(&mut self) -> TokenType {
        while identifier_grapheme(self.peek_grapheme()) {
            self.next_grapheme();
        }

        let lexeme = self.lexeme();
        if let Some(tk) = into_keyword(lexeme) {
            return tk;
        }

        if let Some(true) = lexeme.chars().take(1).next().map(|c| c.is_lowercase()) {
            TokenType::LowerIdent
        } else if let Some(true) = lexeme.chars().take(1).next().map(|c| c.is_uppercase()) {
            TokenType::UpperIdent
        } else {
            TokenType::Unknown
        }
    }

    fn number(&mut self) -> TokenType {
        // TODO handle other bases
        while let Some(true) = self
            .peek_grapheme()
            .chars()
            .take(1)
            .next()
            .map(|c| c.is_numeric())
        {
            self.next_grapheme();
        }

        if let Ok(int) = self.lexeme().parse() {
            if self.peek_grapheme() == "." {
                // TODO handle exponential notation
                self.next_grapheme();

                while let Some(true) = self
                    .peek_grapheme()
                    .chars()
                    .take(1)
                    .next()
                    .map(|c| c.is_numeric())
                {
                    self.next_grapheme();
                }

                if let Ok(float) = self.lexeme().parse() {
                    TokenType::Float(float)
                } else {
                    TokenType::Unknown
                }
            } else {
                TokenType::Int(int)
            }
        } else {
            TokenType::Unknown
        }
    }
}

fn into_keyword(s: &str) -> Option<TokenType> {
    match s {
        "fn" => Some(TokenType::Fn),
        "if" => Some(TokenType::If),
        "else" => Some(TokenType::Else),
        "while" => Some(TokenType::While),
        "for" => Some(TokenType::For),
        "in" => Some(TokenType::In),
        "break" => Some(TokenType::Break),
        "continue" => Some(TokenType::Continue),
        "return" => Some(TokenType::Return),
        "assert" => Some(TokenType::Assert),
        "true" => Some(TokenType::True),
        "false" => Some(TokenType::False),
        "type" => Some(TokenType::Type),
        "where" => Some(TokenType::Where),
        "async" => Some(TokenType::Async),
        "builtin" => Some(TokenType::Builtin),
        "export" => Some(TokenType::Export),
        "const" => Some(TokenType::Const),
        "impl" => Some(TokenType::Impl),
        "class" => Some(TokenType::Class),
        "let" => Some(TokenType::Let),
        "var" => Some(TokenType::Var),
        _ => None,
    }
}

fn lower(g: &str) -> bool {
    matches!(
        g,
        "a" | "b"
            | "c"
            | "d"
            | "e"
            | "f"
            | "g"
            | "h"
            | "i"
            | "j"
            | "k"
            | "l"
            | "m"
            | "n"
            | "o"
            | "p"
            | "q"
            | "r"
            | "s"
            | "t"
            | "u"
            | "v"
            | "w"
            | "x"
            | "y"
            | "z"
    )
}

fn upper(g: &str) -> bool {
    matches!(
        g,
        "A" | "B"
            | "C"
            | "D"
            | "E"
            | "F"
            | "G"
            | "H"
            | "I"
            | "J"
            | "K"
            | "L"
            | "M"
            | "N"
            | "O"
            | "P"
            | "Q"
            | "R"
            | "S"
            | "T"
            | "U"
            | "V"
            | "W"
            | "X"
            | "Y"
            | "Z"
    )
}

fn identifier_grapheme(g: &str) -> bool {
    lower(g) || upper(g) || g == "_"
}
