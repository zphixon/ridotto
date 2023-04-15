use crate::scan::{Pos, Token, TokenType};

#[derive(Debug)]
pub enum RidottoError {
    ExpectedIdentifier {
        got: String,
        pos: Pos,
    },

    ExpectedUpperIdent {
        got: String,
        pos: Pos,
    },

    ExpectedLowerIdent {
        got: String,
        pos: Pos,
    },

    ExpectedToken {
        expected: TokenType,
        got: String,
        pos: Pos,
    },

    ExpectedOneOf {
        expected: Vec<TokenType>,
        got: String,
        pos: Pos,
    },

    AlreadySeenFunctionMod {
        got: String,
        pos: Pos,
    },

    ExpectedItem {
        got: String,
        pos: Pos,
    },

    RecursionLimitReached {
        pos: Pos,
    },
}

impl RidottoError {
    pub fn report(&self) -> ariadne::Report {
        use ariadne::{Label, Report, ReportKind};

        let pos = self.pos();
        let got = self.got();
        let len = pos.byte()..(pos.byte() + got.as_bytes().len());

        Report::build(ReportKind::Error, (), pos.byte())
            .with_label(Label::new(len).with_message(self.message()))
            .finish()
    }

    pub fn message(&self) -> String {
        match self {
            Self::ExpectedIdentifier { got, .. } => {
                format!("Expected an identifier, got {:?}", got)
            }
            Self::ExpectedUpperIdent { got, .. } => {
                format!("Expected an uppercase identifier, got {:?}", got)
            }
            Self::ExpectedLowerIdent { got, .. } => {
                format!("Expected a lowercase identifier, got {:?}", got)
            }
            Self::ExpectedToken { expected, got, .. } => {
                format!("Expected {:?}, got {:?}", expected, got)
            }
            Self::ExpectedOneOf { expected, got, .. } => {
                format!("Expected one of {:?}, got {:?}", expected, got)
            }
            Self::AlreadySeenFunctionMod { got, .. } => {
                format!("The function modifier {:?} has already been used", got)
            }
            Self::ExpectedItem { got, .. } => {
                format!("Expected an item (function, type) got {:?}", got)
            }
            Self::RecursionLimitReached { .. } => {
                format!("Recursion limit reached")
            }
        }
    }

    pub fn pos(&self) -> Pos {
        match self {
            Self::ExpectedIdentifier { pos, .. } => *pos,
            Self::ExpectedUpperIdent { pos, .. } => *pos,
            Self::ExpectedLowerIdent { pos, .. } => *pos,
            Self::ExpectedToken { pos, .. } => *pos,
            Self::ExpectedOneOf { pos, .. } => *pos,
            Self::AlreadySeenFunctionMod { pos, .. } => *pos,
            Self::ExpectedItem { pos, .. } => *pos,
            Self::RecursionLimitReached { pos, .. } => *pos,
        }
    }

    pub fn got(&self) -> &str {
        match self {
            Self::ExpectedIdentifier { got, .. } => got,
            Self::ExpectedUpperIdent { got, .. } => got,
            Self::ExpectedLowerIdent { got, .. } => got,
            Self::ExpectedToken { got, .. } => got,
            Self::ExpectedOneOf { got, .. } => got,
            Self::AlreadySeenFunctionMod { got, .. } => got,
            Self::ExpectedItem { got, .. } => got,
            Self::RecursionLimitReached { .. } => "",
        }
    }

    pub fn expected_identifier(got: Token) -> Self {
        Self::ExpectedIdentifier {
            got: got.lexeme.into(),
            pos: got.pos,
        }
    }

    pub fn expected_lower(got: Token) -> Self {
        Self::ExpectedLowerIdent {
            got: got.lexeme.into(),
            pos: got.pos,
        }
    }

    pub fn expected_upper(got: Token) -> Self {
        Self::ExpectedUpperIdent {
            got: got.lexeme.into(),
            pos: got.pos,
        }
    }

    pub fn expected_token(expected: TokenType, got: Token) -> Self {
        Self::ExpectedToken {
            expected,
            got: got.lexeme.into(),
            pos: got.pos,
        }
    }

    pub fn expected_one_of(expected: impl Expected, got: Token) -> Self {
        Self::ExpectedOneOf {
            expected: expected.expected(),
            got: got.lexeme.into(),
            pos: got.pos,
        }
    }

    pub fn already_seen_function_mod(got: Token) -> Self {
        Self::AlreadySeenFunctionMod {
            got: got.lexeme.into(),
            pos: got.pos,
        }
    }

    pub fn expected_item(got: Token) -> Self {
        Self::ExpectedItem {
            got: got.lexeme.into(),
            pos: got.pos,
        }
    }
}

pub trait Expected {
    fn expected(self) -> Vec<TokenType>;
}

impl Expected for TokenType {
    fn expected(self) -> Vec<TokenType> {
        vec![self]
    }
}

impl<T> Expected for T
where
    T: IntoIterator<Item = TokenType>,
{
    fn expected(self) -> Vec<TokenType> {
        self.into_iter().collect()
    }
}
