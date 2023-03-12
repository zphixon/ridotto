use crate::compiler::{Pos, Token, TokenType};

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
}

impl RidottoError {
    pub fn expected_identifier(got: Token) -> Self {
        Self::ExpectedIdentifier {
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
}
