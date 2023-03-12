//! Modules for compiling Ridotto source code.

use crate::error::RidottoError;

use std::{
    fmt,
    hash::{Hash, Hasher},
};

pub const MAX_DEPTH: usize = 120;

/// Kinds of tokens that may exist in Ridotto code.
///
/// Some of these don't currently have a use, and only exist for the creation of
/// syntax errors :^)
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    /// type
    Type,
    /// does
    Does,
    /// is
    Is,
    /// has
    Has,
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

    String,
    True,
    False,
    Float(f64),
    Int(i64),

    Eof,
}

impl TokenType {
    pub fn is_ident(self) -> bool {
        matches!(self, TokenType::LowerIdent | TokenType::UpperIdent)
    }
}

#[derive(Default, Debug, Clone, Copy)]
pub enum Pos {
    Source {
        line: usize,
        col: usize,
        char: usize,
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

impl Token<'_> {
    pub fn eof() -> Self {
        Token {
            type_: TokenType::Eof,
            lexeme: "",
            pos: Pos::Builtin,
        }
    }
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
