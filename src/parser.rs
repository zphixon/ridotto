use crate::{
    ast::*,
    compiler::{Token, TokenType},
    error::RidottoError,
    scanner::Scanner,
};

pub fn parse(source: &str) -> Result<Ast<'_>, RidottoError> {
    let mut scanner = Scanner::new(source);
    Ok(Ast::Type(parse_type_decl(&mut scanner)?))
}

fn parse_type_decl<'src>(scanner: &mut Scanner<'src>) -> Result<Type<'src>, RidottoError> {
    consume(scanner, TokenType::Type)?;

    let decl_type_spec = parse_decl_type_spec(scanner)?;

    // TODO alias

    let has = if scanner.peek_token().type_ == TokenType::Has {
        Some(parse_has(scanner)?)
    } else {
        None
    };

    Ok(Type {
        decl_type_spec,
        has,
    })
}

fn parse_has<'src>(scanner: &mut Scanner<'src>) -> Result<Has<'src>, RidottoError> {
    scanner.next_token();
    consume(scanner, TokenType::LeftBrace)?;

    let mut annotated = Vec::new();
    while scanner.peek_token().type_ != TokenType::RightBrace {
        annotated.push(parse_annotated(scanner)?);
    }

    consume(scanner, TokenType::RightBrace)?;
    Ok(Has { annotated })
}

fn parse_annotated<'src>(scanner: &mut Scanner<'src>) -> Result<Annotated<'src>, RidottoError> {
    let name = consume(scanner, TokenType::LowerIdent)?;
    consume(scanner, TokenType::Colon)?;
    Ok(Annotated {
        name,
        type_spec: parse_type_spec(scanner)?,
    })
}

fn parse_decl_type_spec<'src>(
    scanner: &mut Scanner<'src>,
) -> Result<DeclTypeSpec<'src>, RidottoError> {
    let name = consume_ident(scanner)?;
    match name.type_ {
        TokenType::UpperIdent => {
            let mut inner = Vec::new();
            if scanner.peek_token().type_ == TokenType::LeftBracket {
                scanner.next_token();
                inner.push(parse_decl_type_spec(scanner)?);

                while scanner.peek_token().type_ == TokenType::Comma {
                    scanner.next_token();
                    inner.push(parse_decl_type_spec(scanner)?);
                }

                consume(scanner, TokenType::RightBracket)?;
            }

            Ok(DeclTypeSpec::Type { name, inner })
        }

        TokenType::LowerIdent => {
            let default = if scanner.peek_token().type_ == TokenType::Equal {
                scanner.next_token();
                Some(parse_type_spec(scanner)?)
            } else {
                None
            };

            Ok(DeclTypeSpec::TypeVar { name, default })
        }

        _ => unreachable!(),
    }
}

fn parse_type_spec<'src>(scanner: &mut Scanner<'src>) -> Result<TypeSpec<'src>, RidottoError> {
    let name = consume_ident(scanner)?;
    match name.type_ {
        TokenType::UpperIdent => {
            let mut inner = vec![];
            if scanner.peek_token().type_ == TokenType::LeftBracket {
                scanner.next_token();
                inner.push(parse_type_spec(scanner)?);

                while scanner.peek_token().type_ == TokenType::Comma {
                    scanner.next_token();
                    inner.push(parse_type_spec(scanner)?);
                }

                consume(scanner, TokenType::RightBracket)?;
            }

            Ok(TypeSpec::Type { name, inner })
        }

        TokenType::LowerIdent => Ok(TypeSpec::TypeVar { name }),

        _ => unreachable!(),
    }
}

fn consume_ident<'src>(scanner: &mut Scanner<'src>) -> Result<Token<'src>, RidottoError> {
    let name = scanner.next_token();
    if name.type_.is_ident() {
        Ok(name)
    } else {
        Err(RidottoError::expected_identifier(name))
    }
}

fn consume<'src>(
    scanner: &mut Scanner<'src>,
    type_: TokenType,
) -> Result<Token<'src>, RidottoError> {
    let token = scanner.next_token();
    if token.type_ == type_ {
        Ok(token)
    } else {
        Err(RidottoError::expected_token(type_, token))
    }
}
