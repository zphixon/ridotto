//! Contains `Scanner`, an on-demand producer of tokens.

use crate::{
    compiler::{Pos, Token, TokenType},
    error::RidottoError,
};
use std::{collections::VecDeque, iter::Peekable};
use unicode_segmentation::{Graphemes, UnicodeSegmentation};

pub struct Scanner<'src> {
    graphemes: Peekable<Graphemes<'src>>,

    current: Option<Token<'src>>,

    line: usize,
    column: usize,

    source: &'src str,
    start: usize,
    len: usize,
}

impl<'src> Scanner<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            graphemes: source.graphemes(true).peekable(),

            current: None,

            line: 1,
            column: 0,

            source,
            start: 0,
            len: 0,
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
        self.len += grapheme.as_bytes().len();

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

    fn next(&mut self) -> Token<'src> {
        self.slurp_whitespace();
        if self.peek_grapheme() == "" {
            return Token::eof();
        }

        self.start += self.len;
        self.len = 0;
        let type_ = match self.next_grapheme() {
            "[" => TokenType::LeftBracket,
            "]" => TokenType::RightBracket,
            "(" => TokenType::LeftParen,
            ")" => TokenType::RightParen,
            "," => TokenType::Comma,
            "+" => TokenType::Plus,
            "-" => TokenType::Minus,
            "*" => TokenType::Star,
            "/" => TokenType::Slash,
            "^" => TokenType::Caret,
            "%" => TokenType::Percent,
            "{" => TokenType::LeftBrace,
            "}" => TokenType::RightBrace,
            ":" => TokenType::Colon,
            "." => TokenType::Period,
            "_" => TokenType::Underscore,

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

            _ => {
                // TODO numbers, strings
                self.identifier_or_keyword()
            }
        };

        Token {
            type_,
            lexeme: self.lexeme(),
            pos: Pos::Source {
                line: self.line,
                col: self.column,
            },
        }
    }

    fn lexeme(&self) -> &'src str {
        if self.start >= self.source.len() || self.start + self.len >= self.source.len() {
            ""
        } else {
            &self.source[self.start..self.start + self.len]
        }
    }

    fn identifier_or_keyword(&mut self) -> TokenType {
        //let mut
        while identifier_grapheme(self.peek_grapheme()) {
            self.next_grapheme();
        }

        let lexeme = self.lexeme();
        if let Some(tk) = into_keyword(lexeme) {
            tk
        } else {
            if lexeme.chars().take(1).next().unwrap().is_lowercase() {
                TokenType::LowerIdent
            } else if lexeme.chars().take(1).next().unwrap().is_uppercase() {
                TokenType::UpperIdent
            } else {
                println!("TODO: '{}'", lexeme);
                TokenType::LowerIdent
            }
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
        "does" => Some(TokenType::Does),
        "is" => Some(TokenType::Is),
        "has" => Some(TokenType::Has),
        "where" => Some(TokenType::Where),
        "async" => Some(TokenType::Async),
        "builtin" => Some(TokenType::Builtin),
        "export" => Some(TokenType::Export),
        "const" => Some(TokenType::Const),
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
