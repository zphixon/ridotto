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

pub fn parse(source: &str) -> Result<Vec<Item<'_>>, RidottoError> {
    tracing::trace!("parse");
    let mut scanner = Scanner::new(source);
    let mut ast = Vec::new();

    while scanner.peek_token().type_ != TokenType::Eof {
        match scanner.peek_token().type_ {
            TokenType::Type => {
                ast.push(Item::ItemTypeDecl(parse_type_decl(&mut scanner)?));
            }

            type_ if type_.starts_function() => {
                ast.push(Item::ItemFunction(parse_function(&mut scanner)?));
            }

            TokenType::Class => {
                ast.push(Item::ItemClass(parse_class(&mut scanner)?));
            }

            _ => return Err(RidottoError::expected_item(scanner.peek_token())),
        }
    }

    Ok(ast)
}

macro_rules! parse_fn {
    ($what: ident -> $returns: ty, $body: expr) => {
        paste::paste! {
            fn [<parse_ $what>]<'src>(
                scanner: &mut Scanner<'src>
            ) -> Result<$returns, RidottoError> {
                tracing::trace!(concat!(stringify!($what), " {:?}"), scanner.peek_token().lexeme);
                ($body)(scanner)
            }
        }
    };
}

parse_fn!(type_decl -> TypeDecl<'src>, |scanner: &mut Scanner<'src>| {
    consume(scanner, TokenType::Type)?;
    let name = consume_upper(scanner)?;

    let type_args = if scanner.peek_token().type_ == TokenType::LeftBracket {
        parse_type_args(scanner)?
    } else {
        Vec::new()
    };

    let inner = if scanner.peek_token().type_ == TokenType::Equal {
        consume(scanner, TokenType::Equal)?;
        TypeDeclInnerOrAlias::TypeDeclAlias {
            alias: parse_type_expr(scanner)?,
        }
    } else {
        TypeDeclInnerOrAlias::TypeDeclInner {
            inner: parse_regular_type(scanner)?,
        }
    };

    Ok(TypeDecl {
        name,
        type_args,
        inner,
    })
});


fn parse_type_variant<'src>(
    scanner: &mut Scanner<'src>,
) -> Result<TypeVariant<'src>, RidottoError> {
    tracing::trace!("parse_type_variant {:?}", scanner.peek_token().lexeme);
    let name = consume_upper(scanner)?;

    let inner = if scanner.peek_token().type_ == TokenType::Equal {
        consume(scanner, TokenType::Equal)?;
        TypeDeclInnerOrAlias::TypeDeclAlias {
            alias: parse_type_expr(scanner)?,
        }
    } else {
        TypeDeclInnerOrAlias::TypeDeclInner {
            inner: parse_regular_type(scanner)?,
        }
    };

    Ok(TypeVariant { name, inner })
}

fn parse_regular_type<'src>(
    scanner: &mut Scanner<'src>,
) -> Result<TypeDeclInner<'src>, RidottoError> {
    tracing::trace!("parse_regular_type {:?}", scanner.peek_token().lexeme);
    let mut fields = Vec::new();
    let mut variants = Vec::new();
    let mut behaviors = Vec::new();

    if scanner.peek_token().type_ == TokenType::LeftBrace {
        consume(scanner, TokenType::LeftBrace)?;
        while scanner.peek_token().type_ == TokenType::LowerIdent {
            fields.push(parse_annotated(scanner)?);
        }
        while scanner.peek_token().type_ == TokenType::UpperIdent {
            variants.push(parse_type_variant(scanner)?);
        }
        while scanner.peek_token().type_.starts_function() {
            behaviors.push(parse_function(scanner)?);
        }
        consume(scanner, TokenType::RightBrace)?;
    }

    Ok(TypeDeclInner {
        fields,
        variants,
        behaviors,
    })
}

fn parse_class<'src>(scanner: &mut Scanner<'src>) -> Result<Class<'src>, RidottoError> {
    tracing::trace!("parse_class {:?}", scanner.peek_token().lexeme);
    consume(scanner, TokenType::Class)?;

    let name = consume_upper(scanner)?;

    let type_args = if scanner.peek_token().type_ == TokenType::LeftBracket {
        parse_type_args(scanner)?
    } else {
        Vec::new()
    };

    consume(scanner, TokenType::LeftBrace)?;

    let mut behaviors = Vec::new();
    while scanner.peek_token().type_.starts_function() {
        let head = parse_function_head(scanner)?;
        if scanner.peek_token().type_ == TokenType::LeftBrace {
            behaviors.push(MaybeAbstractFunction::Function(Function {
                head,
                body: parse_function_body(scanner)?,
            }));
        } else {
            behaviors.push(MaybeAbstractFunction::AbstractFunction(head));
        }
    }

    consume(scanner, TokenType::RightBrace)?;

    Ok(Class {
        name,
        type_args,
        behaviors,
    })
}

fn parse_function<'src>(scanner: &mut Scanner<'src>) -> Result<Function<'src>, RidottoError> {
    tracing::trace!("parse_function {:?}", scanner.peek_token().lexeme);
    let head = parse_function_head(scanner)?;
    let body = parse_function_body(scanner)?;
    Ok(Function { head, body })
}

fn parse_function_head<'src>(
    scanner: &mut Scanner<'src>,
) -> Result<FunctionHead<'src>, RidottoError> {
    tracing::trace!("parse_function_head {:?}", scanner.peek_token().lexeme);
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

    let type_args = if scanner.peek_token().type_ == TokenType::LeftBracket {
        parse_type_args(scanner)?
    } else {
        Vec::new()
    };

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
        Some(parse_type_expr(scanner)?)
    } else {
        None
    };

    Ok(FunctionHead {
        builtin,
        async_,
        const_,
        export,
        name,
        type_args,
        args,
        return_,
    })
}

fn parse_function_body<'src>(scanner: &mut Scanner<'src>) -> Result<Vec<Stmt<'src>>, RidottoError> {
    tracing::trace!("parse_function_body {:?}", scanner.peek_token().lexeme);

    let mut depth = 0;
    while {
        if scanner.peek_token().type_ == TokenType::LeftBrace {
            depth += 1;
        }
        if scanner.peek_token().type_ == TokenType::RightBrace {
            depth -= 1;
        }

        scanner.next_token();
        depth != 0
    } {
        // :)
    }

    Ok(Vec::new())
}

fn parse_annotated<'src>(scanner: &mut Scanner<'src>) -> Result<TypeAnnotated<'src>, RidottoError> {
    tracing::trace!("parse_annotated {:?}", scanner.peek_token().lexeme);
    let name = consume_lower(scanner)?;
    consume(scanner, TokenType::Colon)?;
    Ok(TypeAnnotated {
        name,
        type_: parse_type_expr(scanner)?,
    })
}

fn parse_type_expr<'src>(scanner: &mut Scanner<'src>) -> Result<TypeExpr<'src>, RidottoError> {
    tracing::trace!("parse_type_expr {:?}", scanner.peek_token().lexeme);
    let name = parse_type_name(scanner)?;

    match (&name, scanner.peek_token().type_) {
        (TypeName::Namespace(_) | TypeName::TypeValue(_), TokenType::LeftBracket) => {
            Ok(TypeExpr::Instantiated {
                name,
                type_args: parse_type_args(scanner)?,
            })
        }

        (TypeName::Namespace(_) | TypeName::TypeValue(_), _) => Ok(TypeExpr::Concrete { name }),

        (TypeName::TypeVar(_), TokenType::Equal) => {
            scanner.next_token();
            Ok(TypeExpr::TypeVar {
                name,
                default: Some(parse_type_expr_no_default(scanner)?),
            })
        }

        (TypeName::TypeVar(_), _) => Ok(TypeExpr::TypeVar {
            name,
            default: None,
        }),
    }
}

fn parse_type_args<'src>(scanner: &mut Scanner<'src>) -> Result<Vec<TypeExpr<'src>>, RidottoError> {
    tracing::trace!("parse_type_args {:?}", scanner.peek_token().lexeme);
    let mut type_args = Vec::new();
    consume(scanner, TokenType::LeftBracket)?;
    while scanner.peek_token().type_ != TokenType::RightBracket {
        type_args.push(parse_type_expr(scanner)?);
        if scanner.peek_token().type_ != TokenType::RightBracket {
            consume(scanner, TokenType::Comma)?;
        }
    }
    if !type_args.is_empty() && scanner.peek_token().type_ == TokenType::Comma {
        scanner.next_token();
    }
    consume(scanner, TokenType::RightBracket)?;
    Ok(type_args)
}

fn parse_type_expr_no_default<'src>(
    scanner: &mut Scanner<'src>,
) -> Result<TypeExprNoDefault<'src>, RidottoError> {
    tracing::trace!("parse_type_expr_no_default {:?}", scanner.peek_token().lexeme);
    let name = parse_type_name(scanner)?;

    if scanner.peek_token().type_ == TokenType::LeftBracket {
        let mut type_args = Vec::new();
        consume(scanner, TokenType::LeftBracket)?;
        while scanner.peek_token().type_ != TokenType::RightBracket {
            type_args.push(parse_type_expr_no_default(scanner)?);
            if scanner.peek_token().type_ != TokenType::RightBracket {
                consume(scanner, TokenType::Comma)?;
            }
        }
        if !type_args.is_empty() && scanner.peek_token().type_ == TokenType::Comma {
            scanner.next_token();
        }
        consume(scanner, TokenType::RightBracket)?;

        Ok(TypeExprNoDefault::Instantiated { name, type_args })
    } else {
        Ok(TypeExprNoDefault::Concrete { name })
    }
}

fn parse_type_name<'src>(scanner: &mut Scanner<'src>) -> Result<TypeName<'src>, RidottoError> {
    tracing::trace!("parse_type_name {:?}", scanner.peek_token().lexeme);
    let name = consume_ident(scanner)?;
    if name.type_ == TokenType::UpperIdent {
        let uppercase = NameUppercase { uppercase: name };
        if scanner.peek_token().type_ == TokenType::Period {
            let mut names = vec![uppercase];
            while scanner.peek_token().type_ == TokenType::Period {
                scanner.next_token();
                names.push(consume_upper(scanner)?);
            }
            Ok(TypeName::Namespace(names))
        } else {
            Ok(TypeName::TypeValue(uppercase))
        }
    } else if name.type_ == TokenType::LowerIdent {
        Ok(TypeName::TypeVar(NameLowercase { lowercase: name }))
    } else {
        unreachable!()
    }
}

fn consume_ident<'src>(scanner: &mut Scanner<'src>) -> Result<Token<'src>, RidottoError> {
    tracing::trace!("consume_ident {:?}", scanner.peek_token().lexeme);
    let name = scanner.next_token();
    if name.type_.is_ident() {
        Ok(name)
    } else {
        Err(RidottoError::expected_identifier(name))
    }
}

fn consume_lower<'src>(scanner: &mut Scanner<'src>) -> Result<NameLowercase<'src>, RidottoError> {
    tracing::trace!("consume_lower {:?}", scanner.peek_token().lexeme);
    let token = scanner.next_token();
    if token.type_ == TokenType::LowerIdent {
        Ok(NameLowercase { lowercase: token })
    } else {
        Err(RidottoError::expected_lower(token))
    }
}

fn consume_upper<'src>(scanner: &mut Scanner<'src>) -> Result<NameUppercase<'src>, RidottoError> {
    tracing::trace!("consume_upper {:?}", scanner.peek_token().lexeme);
    let token = scanner.next_token();
    if token.type_ == TokenType::UpperIdent {
        Ok(NameUppercase { uppercase: token })
    } else {
        Err(RidottoError::expected_upper(token))
    }
}

fn consume<'src>(
    scanner: &mut Scanner<'src>,
    type_: TokenType,
) -> Result<Token<'src>, RidottoError> {
    tracing::trace!("consume {:?}", scanner.peek_token().lexeme);
    let token = scanner.next_token();
    if token.type_ == type_ {
        Ok(token)
    } else {
        Err(RidottoError::expected_token(type_, token))
    }
}
