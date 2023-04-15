use crate::{
    ast::*,
    error::RidottoError,
    scan::{Scanner, Token, TokenType},
};

pub fn parse(source: &str) -> Result<Vec<Item<'_>>, RidottoError> {
    tracing::trace!("parse");
    let mut scanner = Scanner::new(source);
    let mut ast = Vec::new();

    while scanner.peek_token().type_ != TokenType::Eof {
        match scanner.peek_token().type_ {
            TokenType::Type => {
                ast.push(Item::ItemTypeDecl(parse_type_decl(&mut scanner, 0)?));
            }

            type_ if type_.starts_function() => {
                ast.push(Item::ItemFunction(parse_function(&mut scanner, 0)?));
            }

            TokenType::Class => {
                ast.push(Item::ItemClass(parse_class(&mut scanner, 0)?));
            }

            TokenType::Impl => {
                ast.push(Item::ItemImpl(parse_impl(&mut scanner, 0)?));
            }

            _ => return Err(RidottoError::expected_item(scanner.peek_token())),
        }
    }

    Ok(ast)
}

fn trace(depth: usize, prefix: &str, func: &str, scanner: &mut Scanner) {
    tracing::trace!(
        "{}{}{} {:?}",
        std::iter::repeat(".").take(depth).collect::<String>(),
        prefix,
        func,
        scanner.peek_token().lexeme,
    );
}

#[ridotto_macros::parser_traced]
fn parse_type_decl<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<TypeDecl<'src>, RidottoError> {
    consume(scanner, TokenType::Type, depth)?;
    let name = consume_upper(scanner, depth)?;

    let type_args = if scanner.peek_token().type_ == TokenType::LeftBracket {
        parse_type_args(scanner, depth)?
    } else {
        Vec::new()
    };

    let inner = if scanner.peek_token().type_ == TokenType::Equal {
        consume(scanner, TokenType::Equal, depth)?;
        TypeDeclInnerOrAlias::TypeDeclAlias {
            alias: parse_type_expr(scanner, depth)?,
        }
    } else {
        TypeDeclInnerOrAlias::TypeDeclInner {
            inner: parse_type_decl_inner(scanner, depth)?,
        }
    };

    Ok(TypeDecl {
        name,
        type_args,
        inner,
    })
}

#[ridotto_macros::parser_traced]
fn parse_type_variant<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<TypeVariant<'src>, RidottoError> {
    let name = consume_upper(scanner, depth)?;

    let inner = if scanner.peek_token().type_ == TokenType::Equal {
        consume(scanner, TokenType::Equal, depth)?;
        TypeDeclInnerOrAlias::TypeDeclAlias {
            alias: parse_type_expr(scanner, depth)?,
        }
    } else {
        TypeDeclInnerOrAlias::TypeDeclInner {
            inner: parse_type_decl_inner(scanner, depth)?,
        }
    };

    Ok(TypeVariant { name, inner })
}

#[ridotto_macros::parser_traced]
fn parse_type_decl_inner<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<TypeDeclInner<'src>, RidottoError> {
    let mut fields = Vec::new();
    let mut variants = Vec::new();
    let mut behaviors = Vec::new();

    if scanner.peek_token().type_ == TokenType::LeftBrace {
        consume(scanner, TokenType::LeftBrace, depth)?;
        while scanner.peek_token().type_ == TokenType::LowerIdent {
            fields.push(parse_annotated(scanner, depth)?);
        }
        while scanner.peek_token().type_ == TokenType::UpperIdent {
            variants.push(parse_type_variant(scanner, depth)?);
        }
        while scanner.peek_token().type_.starts_function() {
            behaviors.push(parse_function(scanner, depth)?);
        }
        consume(scanner, TokenType::RightBrace, depth)?;
    }

    Ok(TypeDeclInner {
        fields,
        variants,
        behaviors,
    })
}

#[ridotto_macros::parser_traced]
fn parse_class<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Class<'src>, RidottoError> {
    consume(scanner, TokenType::Class, depth)?;

    let name = consume_upper(scanner, depth)?;

    let type_args = if scanner.peek_token().type_ == TokenType::LeftBracket {
        parse_type_args(scanner, depth)?
    } else {
        Vec::new()
    };

    consume(scanner, TokenType::LeftBrace, depth)?;

    let mut behaviors = Vec::new();
    while scanner.peek_token().type_.starts_function() {
        let head = parse_function_head(scanner, depth)?;
        if scanner.peek_token().type_ == TokenType::LeftBrace {
            behaviors.push(MaybeAbstractFunction::Function(Function {
                head,
                body: parse_function_body(scanner, depth)?,
            }));
        } else {
            behaviors.push(MaybeAbstractFunction::AbstractFunction(head));
        }
    }

    consume(scanner, TokenType::RightBrace, depth)?;

    Ok(Class {
        name,
        type_args,
        behaviors,
    })
}

#[ridotto_macros::parser_traced]
fn parse_impl<'src>(scanner: &mut Scanner<'src>, depth: usize) -> Result<Impl<'src>, RidottoError> {
    consume(scanner, TokenType::Impl, depth)?;
    let class = parse_type_expr(scanner, depth)?;
    consume(scanner, TokenType::For, depth)?;
    let for_ = parse_type_expr(scanner, depth)?;

    consume(scanner, TokenType::LeftBrace, depth)?;

    let mut behaviors = Vec::new();
    while scanner.peek_token().type_.starts_function() {
        behaviors.push(parse_function(scanner, depth)?);
    }

    consume(scanner, TokenType::RightBrace, depth)?;

    Ok(Impl {
        class,
        for_,
        behaviors,
    })
}

#[ridotto_macros::parser_traced]
fn parse_function<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Function<'src>, RidottoError> {
    let head = parse_function_head(scanner, depth)?;
    let body = parse_function_body(scanner, depth)?;
    Ok(Function { head, body })
}

#[ridotto_macros::parser_traced]
fn parse_function_head<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<FunctionHead<'src>, RidottoError> {
    let mut builtin = false;
    let mut async_ = false;
    let mut const_ = false;
    let mut export = false;

    while scanner.peek_token().type_.starts_function()
        && scanner.peek_token().type_ != TokenType::Fn
    {
        let peeked = scanner.peek_token();
        match peeked.type_ {
            TokenType::Builtin if !builtin => builtin = true,
            TokenType::Async if !async_ => async_ = true,
            TokenType::Const if !const_ => const_ = true,
            TokenType::Export if !export => export = true,

            TokenType::Builtin | TokenType::Async | TokenType::Const | TokenType::Export => {
                return Err(RidottoError::already_seen_function_mod(peeked));
            }

            _ => {}
        }

        scanner.next_token();
    }

    let got = scanner.peek_token();
    consume(scanner, TokenType::Fn, depth).map_err(|_| {
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

    let name = consume_lower(scanner, depth)?;

    let type_args = if scanner.peek_token().type_ == TokenType::LeftBracket {
        parse_type_args(scanner, depth)?
    } else {
        Vec::new()
    };

    let mut args = Vec::new();
    consume(scanner, TokenType::LeftParen, depth)?;
    while scanner.peek_token().type_ != TokenType::RightParen {
        args.push(parse_annotated(scanner, depth)?);
        if scanner.peek_token().type_ != TokenType::RightParen {
            consume(scanner, TokenType::Comma, depth)?;
        }
    }
    if !args.is_empty() && scanner.peek_token().type_ == TokenType::Comma {
        scanner.next_token();
    }
    consume(scanner, TokenType::RightParen, depth)?;

    let return_ = if scanner.peek_token().type_ == TokenType::Minus {
        consume(scanner, TokenType::Minus, depth)?;
        consume(scanner, TokenType::RightAngle, depth)?;
        Some(parse_type_expr(scanner, depth)?)
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

#[ridotto_macros::parser_traced]
fn parse_function_body<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Vec<Stmt<'src>>, RidottoError> {
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

#[ridotto_macros::parser_traced]
fn parse_annotated<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<TypeAnnotated<'src>, RidottoError> {
    let name = consume_lower(scanner, depth)?;
    consume(scanner, TokenType::Colon, depth)?;
    Ok(TypeAnnotated {
        name,
        type_: parse_type_expr(scanner, depth)?,
    })
}

#[ridotto_macros::parser_traced]
fn parse_type_expr<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<TypeExpr<'src>, RidottoError> {
    if scanner.peek_token().type_ == TokenType::Fn {
        parse_fn_type_expr(scanner, depth)
    } else if scanner.peek_token().type_ == TokenType::LeftParen {
        parse_tuple_type_expr(scanner, depth)
    } else {
        parse_regular_type_expr(scanner, depth)
    }
}

#[ridotto_macros::parser_traced]
fn parse_regular_type_expr<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<TypeExpr<'src>, RidottoError> {
    let name = parse_type_name(scanner, depth)?;

    match (&name, scanner.peek_token().type_) {
        (TypeName::Namespace(_) | TypeName::TypeValue(_), TokenType::LeftBracket) => {
            Ok(TypeExpr::Instantiated {
                name,
                type_args: parse_type_args(scanner, depth)?,
            })
        }

        (TypeName::Namespace(_) | TypeName::TypeValue(_), _) => Ok(TypeExpr::Concrete { name }),

        (TypeName::TypeVar(_), TokenType::Equal) => {
            scanner.next_token();
            Ok(TypeExpr::TypeVar {
                name,
                default: Some(parse_type_expr_no_default(scanner, depth)?),
            })
        }

        (TypeName::TypeVar(_), _) => Ok(TypeExpr::TypeVar {
            name,
            default: None,
        }),
    }
}

#[ridotto_macros::parser_traced]
fn parse_tuple_type_expr<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<TypeExpr<'src>, RidottoError> {
    let mut inner = Vec::new();
    consume(scanner, TokenType::LeftParen, depth)?;
    while scanner.peek_token().type_ != TokenType::RightParen {
        inner.push(parse_type_expr(scanner, depth)?);
        if scanner.peek_token().type_ != TokenType::RightParen {
            consume(scanner, TokenType::Comma, depth)?;
        }
    }
    if !inner.is_empty() && scanner.peek_token().type_ == TokenType::Comma {
        scanner.next_token();
    }
    consume(scanner, TokenType::RightParen, depth)?;
    Ok(TypeExpr::Tuple { inner })
}

#[ridotto_macros::parser_traced]
fn parse_fn_type_expr<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<TypeExpr<'src>, RidottoError> {
    consume(scanner, TokenType::Fn, depth)?;

    let type_args = if scanner.peek_token().type_ == TokenType::LeftBracket {
        parse_type_args(scanner, depth)?
    } else {
        Vec::new()
    };

    let TypeExpr::Tuple {
        inner: args
    } = parse_tuple_type_expr(scanner, depth)? else {
        unreachable!();
    };

    let return_ = if scanner.peek_token().type_ == TokenType::Minus {
        consume(scanner, TokenType::Minus, depth)?;
        consume(scanner, TokenType::RightAngle, depth)?;
        Some(Box::new(parse_type_expr(scanner, depth)?))
    } else {
        None
    };

    Ok(TypeExpr::FnType {
        type_args,
        args,
        return_,
    })
}

#[ridotto_macros::parser_traced]
fn parse_type_args<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Vec<TypeExpr<'src>>, RidottoError> {
    let mut type_args = Vec::new();
    consume(scanner, TokenType::LeftBracket, depth)?;
    while scanner.peek_token().type_ != TokenType::RightBracket {
        type_args.push(parse_type_expr(scanner, depth)?);
        if scanner.peek_token().type_ != TokenType::RightBracket {
            consume(scanner, TokenType::Comma, depth)?;
        }
    }
    if !type_args.is_empty() && scanner.peek_token().type_ == TokenType::Comma {
        scanner.next_token();
    }
    consume(scanner, TokenType::RightBracket, depth)?;
    Ok(type_args)
}

#[ridotto_macros::parser_traced]
fn parse_type_expr_no_default<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<TypeExprNoDefault<'src>, RidottoError> {
    let name = parse_type_name(scanner, depth)?;

    if scanner.peek_token().type_ == TokenType::LeftBracket {
        let mut type_args = Vec::new();
        consume(scanner, TokenType::LeftBracket, depth)?;
        while scanner.peek_token().type_ != TokenType::RightBracket {
            type_args.push(parse_type_expr_no_default(scanner, depth)?);
            if scanner.peek_token().type_ != TokenType::RightBracket {
                consume(scanner, TokenType::Comma, depth)?;
            }
        }
        if !type_args.is_empty() && scanner.peek_token().type_ == TokenType::Comma {
            scanner.next_token();
        }
        consume(scanner, TokenType::RightBracket, depth)?;

        Ok(TypeExprNoDefault::Instantiated { name, type_args })
    } else {
        Ok(match &name {
            TypeName::Namespace(_) | TypeName::TypeValue(_) => TypeExprNoDefault::Concrete { name },
            TypeName::TypeVar(_) => TypeExprNoDefault::TypeVar { name },
        })
    }
}

#[ridotto_macros::parser_traced]
fn parse_type_name<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<TypeName<'src>, RidottoError> {
    let name = consume_ident(scanner, depth)?;
    if name.type_ == TokenType::UpperIdent {
        let uppercase = NameUppercase { uppercase: name };
        if scanner.peek_token().type_ == TokenType::Period {
            let mut names = vec![uppercase];
            while scanner.peek_token().type_ == TokenType::Period {
                scanner.next_token();
                names.push(consume_upper(scanner, depth)?);
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

fn consume_ident<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Token<'src>, RidottoError> {
    trace(depth, "- ", "consume_ident", scanner);
    let name = scanner.next_token();
    if name.type_.is_ident() {
        Ok(name)
    } else {
        Err(RidottoError::expected_identifier(name))
    }
}

fn consume_lower<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<NameLowercase<'src>, RidottoError> {
    trace(depth, "- ", "consume_lower", scanner);
    let token = scanner.next_token();
    if token.type_ == TokenType::LowerIdent {
        Ok(NameLowercase { lowercase: token })
    } else {
        Err(RidottoError::expected_lower(token))
    }
}

fn consume_upper<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<NameUppercase<'src>, RidottoError> {
    trace(depth, "- ", "consume_upper", scanner);
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
    depth: usize,
) -> Result<Token<'src>, RidottoError> {
    trace(depth, "- ", "consume", scanner);
    let token = scanner.next_token();
    if token.type_ == type_ {
        Ok(token)
    } else {
        Err(RidottoError::expected_token(type_, token))
    }
}
