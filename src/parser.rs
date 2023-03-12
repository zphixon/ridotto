use std::{collections::HashSet, ops::Index};

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

    let inner = match scanner.peek_token().type_ {
        TokenType::Equal => {
            scanner.next_token();
            let decl_type_spec = parse_decl_type_spec(scanner)?;
            TypeInner::Alias { decl_type_spec }
        }

        _ => parse_type_decl_inner(scanner)?,
    };

    Ok(Type {
        decl_type_spec,
        inner,
    })
}

fn parse_type_decl_inner<'src>(
    scanner: &mut Scanner<'src>,
) -> Result<TypeInner<'src>, RidottoError> {
    let mut expected = Vec::from_iter([TokenType::Is, TokenType::Has, TokenType::Does]);
    let mut has = None;
    let is = None;
    let does = None;

    match scanner.peek_token().type_ {
        TokenType::Has if expected.contains(&TokenType::Has) => {
            expected.swap_remove(
                expected
                    .iter()
                    .position(|&tt| tt == TokenType::Has)
                    .unwrap(),
            );

            has = Some(parse_has(scanner)?);
        }

        TokenType::Is if expected.contains(&TokenType::Is) => {
            expected.swap_remove(expected.iter().position(|&tt| tt == TokenType::Is).unwrap());
        }

        TokenType::Does if expected.contains(&TokenType::Does) => {
            expected.swap_remove(
                expected
                    .iter()
                    .position(|&tt| tt == TokenType::Does)
                    .unwrap(),
            );
        }

        _ if !expected.is_empty() => {
            return Err(RidottoError::expected_one_of(
                expected,
                scanner.peek_token(),
            ))
        }

        _ if expected.is_empty() => return Err(RidottoError::expected_item(scanner.peek_token())),

        _ => unreachable!(),
    }

    Ok(TypeInner::Regular { has, is, does })
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
    let name = consume_lower(scanner)?;
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

fn consume_lower<'src>(scanner: &mut Scanner<'src>) -> Result<Token<'src>, RidottoError> {
    let token = scanner.next_token();
    if token.type_ == TokenType::LowerIdent {
        Ok(token)
    } else {
        Err(RidottoError::expected_lower(token))
    }
}

fn consume_upper<'src>(scanner: &mut Scanner<'src>) -> Result<Token<'src>, RidottoError> {
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
    let token = scanner.next_token();
    if token.type_ == type_ {
        Ok(token)
    } else {
        Err(RidottoError::expected_token(type_, token))
    }
}
