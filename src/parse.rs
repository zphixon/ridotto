use crate::{ast::*, error::RidottoError};
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
                    while !matches!(self.peek_grapheme(), "\n" | "") {
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

            _ => {
                // TODO numbers, strings
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
        "where" => Some(TokenType::Where),
        "async" => Some(TokenType::Async),
        "builtin" => Some(TokenType::Builtin),
        "export" => Some(TokenType::Export),
        "const" => Some(TokenType::Const),
        "impl" => Some(TokenType::Impl),
        "class" => Some(TokenType::Class),
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

pub fn parse(source: &str) -> Result<Vec<Ast<'_>>, RidottoError> {
    tracing::trace!("parse");
    let mut scanner = Scanner::new(source);
    let mut ast = Vec::new();

    while scanner.peek_token().type_ != TokenType::Eof {
        match scanner.peek_token().type_ {
            TokenType::Type => {
                ast.push(Ast::Type(parse_type_with_kw(&mut scanner)?));
            }

            type_ if type_.starts_function() => {
                ast.push(Ast::Function(parse_function(&mut scanner)?));
            }

            _ => return Err(RidottoError::expected_item(scanner.peek_token())),
        }
    }

    Ok(ast)
}

fn parse_type_with_kw<'src>(scanner: &mut Scanner<'src>) -> Result<Type<'src>, RidottoError> {
    tracing::trace!("parse_type_with_kw");
    consume(scanner, TokenType::Type)?;
    parse_type_without_kw(scanner)
}

fn parse_type_without_kw<'src>(scanner: &mut Scanner<'src>) -> Result<Type<'src>, RidottoError> {
    tracing::trace!("parse_type_without_kw");
    let type_spec = parse_type_spec(scanner)?;

    let inner = if scanner.peek_token().type_ == TokenType::Equal {
        TypeInner::Alias {
            type_spec: parse_type_spec(scanner)?,
        }
    } else {
        TypeInner::Regular {
            regular: parse_regular_type(scanner)?,
        }
    };

    Ok(Type { type_spec, inner })
}

fn parse_regular_type<'src>(scanner: &mut Scanner<'src>) -> Result<Regular<'src>, RidottoError> {
    tracing::trace!("parse_regular_type");
    let mut fields = Vec::new();
    let mut variants = Vec::new();
    let mut behaviors = Vec::new();

    if scanner.peek_token().type_ == TokenType::LeftBrace {
        consume(scanner, TokenType::LeftBrace)?;
        while scanner.peek_token().type_ == TokenType::LowerIdent {
            fields.push(parse_annotated(scanner)?);
        }
        while scanner.peek_token().type_ == TokenType::UpperIdent {
            variants.push(parse_type_without_kw(scanner)?);
        }
        while scanner.peek_token().type_.starts_function() {
            behaviors.push(parse_function(scanner)?);
        }
        consume(scanner, TokenType::RightBrace)?;
    }

    Ok(Regular {
        fields,
        variants,
        behaviors,
    })
}

fn parse_function<'src>(scanner: &mut Scanner<'src>) -> Result<Function<'src>, RidottoError> {
    tracing::trace!("parse_function");
    let head = parse_function_head(scanner)?;
    let body = parse_function_body(scanner)?;
    Ok(Function { head, body })
}

fn parse_function_head<'src>(
    scanner: &mut Scanner<'src>,
) -> Result<FunctionHead<'src>, RidottoError> {
    tracing::trace!("parse_function_head");
    let mut builtin = false;
    let mut async_ = false;
    let mut const_ = false;
    let mut export = false;

    while scanner.peek_token().type_.starts_function()
        && scanner.peek_token().type_ != TokenType::Fn
    {
        match scanner.peek_token().type_ {
            TokenType::Builtin => builtin = true,
            TokenType::Async => async_ = true,
            TokenType::Const => const_ = true,
            TokenType::Export => export = true,
            _ => {}
        }

        scanner.next_token();
    }

    let got = scanner.peek_token();
    consume(scanner, TokenType::Fn).map_err(|_| {
        RidottoError::expected_one_of(
            [
                TokenType::Builtin,
                TokenType::Async,
                TokenType::Const,
                TokenType::Export,
                TokenType::Fn,
            ],
            got,
        )
    })?;

    let name = consume_lower(scanner)?;

    let mut args = Vec::new();
    consume(scanner, TokenType::LeftParen)?;
    while scanner.peek_token().type_ != TokenType::RightParen {
        args.push(parse_annotated(scanner)?);
        if scanner.peek_token().type_ != TokenType::RightParen {
            consume(scanner, TokenType::Comma)?;
        }
    }
    if !args.is_empty() && scanner.peek_token().type_ == TokenType::Comma {
        scanner.next_token();
    }
    consume(scanner, TokenType::RightParen)?;

    let return_ = if scanner.peek_token().type_ == TokenType::Minus {
        consume(scanner, TokenType::Minus)?;
        consume(scanner, TokenType::RightAngle)?;
        Some(parse_type_spec(scanner)?)
    } else {
        None
    };

    Ok(FunctionHead {
        builtin,
        async_,
        const_,
        export,
        name,
        args,
        return_,
    })
}

fn parse_function_body<'src>(scanner: &mut Scanner<'src>) -> Result<Vec<()>, RidottoError> {
    tracing::trace!("parse_function_body");
    consume(scanner, TokenType::LeftBrace)?;
    consume(scanner, TokenType::RightBrace)?;
    Ok(Vec::new())
}

fn parse_annotated<'src>(scanner: &mut Scanner<'src>) -> Result<Annotated<'src>, RidottoError> {
    tracing::trace!("parse_annotated");
    let name = consume_lower(scanner)?;
    consume(scanner, TokenType::Colon)?;
    Ok(Annotated {
        name,
        type_spec: parse_type_spec(scanner)?,
    })
}

fn parse_type_spec<'src>(scanner: &mut Scanner<'src>) -> Result<TypeSpec<'src>, RidottoError> {
    tracing::trace!("parse_type_spec");
    let name = consume_ident(scanner)?;
    match name.type_ {
        TokenType::UpperIdent => {
            let mut inner = Vec::new();
            if scanner.peek_token().type_ == TokenType::LeftBracket {
                consume(scanner, TokenType::LeftBracket)?;
                inner.push(parse_type_spec(scanner)?);

                while scanner.peek_token().type_ == TokenType::Comma {
                    consume(scanner, TokenType::Comma)?;
                    inner.push(parse_type_spec(scanner)?);
                }

                consume(scanner, TokenType::RightBracket)?;
            }

            Ok(TypeSpec::Type { name, inner })
        }

        TokenType::LowerIdent => {
            let default = if scanner.peek_token().type_ == TokenType::Equal {
                consume(scanner, TokenType::Equal)?;
                Some(parse_inner_type_spec(scanner)?)
            } else {
                None
            };

            Ok(TypeSpec::TypeVar { name, default })
        }

        _ => unreachable!(),
    }
}

fn parse_inner_type_spec<'src>(
    scanner: &mut Scanner<'src>,
) -> Result<InnerTypeSpec<'src>, RidottoError> {
    tracing::trace!("parse_inner_type_spec");
    let name = consume_ident(scanner)?;
    match name.type_ {
        TokenType::UpperIdent => {
            let mut inner = vec![];
            if scanner.peek_token().type_ == TokenType::LeftBracket {
                consume(scanner, TokenType::LeftBracket)?;
                inner.push(parse_inner_type_spec(scanner)?);

                while scanner.peek_token().type_ == TokenType::Comma {
                    consume(scanner, TokenType::Comma)?;
                    inner.push(parse_inner_type_spec(scanner)?);
                }

                consume(scanner, TokenType::RightBracket)?;
            }

            Ok(InnerTypeSpec::Type { name, inner })
        }

        TokenType::LowerIdent => Ok(InnerTypeSpec::TypeVar { name }),

        _ => unreachable!(),
    }
}

fn consume_ident<'src>(scanner: &mut Scanner<'src>) -> Result<Token<'src>, RidottoError> {
    tracing::trace!("consume_ident");
    let name = scanner.next_token();
    if name.type_.is_ident() {
        Ok(name)
    } else {
        Err(RidottoError::expected_identifier(name))
    }
}

fn consume_lower<'src>(scanner: &mut Scanner<'src>) -> Result<Token<'src>, RidottoError> {
    tracing::trace!("consume_lower");
    let token = scanner.next_token();
    if token.type_ == TokenType::LowerIdent {
        Ok(token)
    } else {
        Err(RidottoError::expected_lower(token))
    }
}

fn consume_upper<'src>(scanner: &mut Scanner<'src>) -> Result<Token<'src>, RidottoError> {
    tracing::trace!("consume_upper");
    let token = scanner.next_token();
    if token.type_ == TokenType::UpperIdent {
        Ok(token)
    } else {
        Err(RidottoError::expected_upper(token))
    }
}

fn consume<'src>(
    scanner: &mut Scanner<'src>,
    type_: TokenType,
) -> Result<Token<'src>, RidottoError> {
    tracing::trace!("consume");
    let token = scanner.next_token();
    if token.type_ == type_ {
        Ok(token)
    } else {
        Err(RidottoError::expected_token(type_, token))
    }
}
