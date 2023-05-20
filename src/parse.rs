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
    } else if scanner.peek_token().type_ == TokenType::LeftParen {
        let TypeExpr::Tuple {
            inner: fields,
            ..
        } = parse_tuple_type_expr(scanner, depth)? else {
            unreachable!()
        };

        TypeDeclInnerOrAlias::TypeDeclTuple { fields }
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
                body: parse_brace_delimited_stmts(scanner, depth)?,
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
    let body = parse_brace_delimited_stmts(scanner, depth)?;
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

    let args = maybe_comma_delimited(
        scanner,
        depth,
        TokenType::LeftParen,
        TokenType::RightParen,
        parse_annotated,
    )?;

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
fn parse_brace_delimited_stmts<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Vec<Stmt<'src>>, RidottoError> {
    let mut stmts = Vec::new();
    consume(scanner, TokenType::LeftBrace, depth)?;
    while scanner.peek_token().type_ != TokenType::RightBrace {
        stmts.push(parse_stmt(scanner, depth)?);
    }
    consume(scanner, TokenType::RightBrace, depth)?;
    Ok(stmts)
}

#[ridotto_macros::parser_traced]
fn parse_stmt<'src>(scanner: &mut Scanner<'src>, depth: usize) -> Result<Stmt<'src>, RidottoError> {
    match scanner.peek_token().type_ {
        TokenType::Let => parse_binding(scanner, depth),
        TokenType::Match => parse_match(scanner, depth),
        _ => Ok(Stmt::Expr(parse_expr(scanner, depth)?)),
    }
}

#[ridotto_macros::parser_traced]
fn parse_binding<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Stmt<'src>, RidottoError> {
    consume(scanner, TokenType::Let, depth)?;
    let pattern = parse_pattern(scanner, depth)?;
    consume(scanner, TokenType::Equal, depth)?;
    let value = parse_expr(scanner, depth)?;
    Ok(Stmt::Binding { pattern, value })
}

#[ridotto_macros::parser_traced]
fn parse_match<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Stmt<'src>, RidottoError> {
    consume(scanner, TokenType::Match, depth)?;
    let discriminant = parse_expr(scanner, depth)?;
    let branches = maybe_comma_delimited(
        scanner,
        depth,
        TokenType::LeftBrace,
        TokenType::RightBrace,
        parse_match_branch,
    )?;
    Ok(Stmt::Match {
        discriminant,
        branches,
    })
}

#[ridotto_macros::parser_traced]
fn parse_match_branch<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<MatchBranch<'src>, RidottoError> {
    let pattern = parse_pattern(scanner, depth)?;
    let guard = if scanner.peek_token().type_ == TokenType::If {
        consume(scanner, TokenType::If, depth)?;
        Some(parse_expr(scanner, depth)?)
    } else {
        None
    };
    consume(scanner, TokenType::Equal, depth)?;
    consume(scanner, TokenType::RightAngle, depth)?;
    let body = parse_expr(scanner, depth)?;
    Ok(MatchBranch {
        pattern,
        guard,
        body,
    })
}

#[ridotto_macros::parser_traced]
fn parse_pattern<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Pattern<'src>, RidottoError> {
    let left = match scanner.peek_token().type_ {
        TokenType::Underscore => {
            consume(scanner, TokenType::Underscore, depth)?;
            Ok(Pattern::Any)
        }

        TokenType::LeftParen => {
            let bindings = comma_delimited(
                scanner,
                depth,
                TokenType::LeftParen,
                TokenType::RightParen,
                parse_pattern,
            )?;
            Ok(Pattern::Tuple { bindings })
        }

        TokenType::LowerIdent => Ok(Pattern::Binding {
            name: consume_lower(scanner, depth)?,
        }),

        TokenType::Mut => {
            consume(scanner, TokenType::Mut, depth)?;
            Ok(Pattern::MutableBinding {
                name: consume_lower(scanner, depth)?,
            })
        }

        TokenType::UpperIdent => {
            let type_name = parse_type_name(scanner, depth)?;
            if scanner.peek_token().type_ == TokenType::LeftBrace {
                let bindings = comma_delimited(
                    scanner,
                    depth,
                    TokenType::LeftBrace,
                    TokenType::RightBrace,
                    parse_struct_field_pattern,
                )?;
                let partial = if scanner.peek_token().type_ == TokenType::Period {
                    consume(scanner, TokenType::Period, depth)?;
                    consume(scanner, TokenType::Period, depth)?;
                    true
                } else {
                    false
                };
                Ok(Pattern::StructDestructure {
                    type_name,
                    partial,
                    bindings,
                })
            } else if scanner.peek_token().type_ == TokenType::LeftParen {
                let bindings = comma_delimited(
                    scanner,
                    depth,
                    TokenType::LeftParen,
                    TokenType::RightParen,
                    parse_pattern,
                )?;
                let partial = if scanner.peek_token().type_ == TokenType::Period {
                    consume(scanner, TokenType::Period, depth)?;
                    consume(scanner, TokenType::Period, depth)?;
                    true
                } else {
                    false
                };
                Ok(Pattern::TupleDestructure {
                    type_name,
                    partial,
                    bindings,
                })
            } else {
                Ok(Pattern::EnumVariant { type_name })
            }
        }

        _ => Err(RidottoError::expected_pattern(scanner.peek_token())),
    }?;

    if scanner.peek_token().type_ == TokenType::Bar {
        consume(scanner, TokenType::Bar, depth)?;
        let left = Box::new(left);
        let right = Box::new(parse_pattern(scanner, depth)?);
        Ok(Pattern::Alternate { left, right })
    } else {
        Ok(left)
    }
}

#[ridotto_macros::parser_traced]
fn parse_struct_field_pattern<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<StructFieldPattern<'src>, RidottoError> {
    if scanner.peek_token().type_ == TokenType::Period {
        consume(scanner, TokenType::Period, depth)?;
        consume(scanner, TokenType::Period, depth)?;
        return Ok(StructFieldPattern::Rest);
    }

    let name = consume_lower(scanner, depth)?;
    if scanner.peek_token().type_ == TokenType::Colon {
        consume(scanner, TokenType::Colon, depth)?;
        let value = parse_pattern(scanner, depth)?;
        Ok(StructFieldPattern::Named { name, value })
    } else {
        Ok(StructFieldPattern::Shorthand { name })
    }
}

#[ridotto_macros::parser_traced]
fn parse_expr<'src>(scanner: &mut Scanner<'src>, depth: usize) -> Result<Expr<'src>, RidottoError> {
    parse_logic_or(scanner, depth)
}

#[ridotto_macros::parser_traced]
fn parse_logic_or<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Expr<'src>, RidottoError> {
    let mut logic_and = parse_logic_and(scanner, depth)?;
    loop {
        if scanner.peek_token().type_ == TokenType::DoubleBar {
            let lhs = Box::new(logic_and);
            let op = scanner.next_token();
            let rhs = Box::new(parse_logic_or(scanner, depth)?);
            logic_and = Expr::Binary { lhs, op, rhs };
        } else {
            break Ok(logic_and);
        }
    }
}

#[ridotto_macros::parser_traced]
fn parse_logic_and<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Expr<'src>, RidottoError> {
    let mut equality = parse_equality(scanner, depth)?;

    loop {
        if scanner.peek_token().type_ == TokenType::DoubleAmp {
            let lhs = Box::new(equality);
            let op = scanner.next_token();
            let rhs = Box::new(parse_logic_and(scanner, depth)?);
            equality = Expr::Binary { lhs, op, rhs };
        } else {
            break Ok(equality);
        }
    }
}

#[ridotto_macros::parser_traced]
fn parse_equality<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Expr<'src>, RidottoError> {
    let mut comparison = parse_comparison(scanner, depth)?;
    loop {
        if matches!(
            scanner.peek_token().type_,
            TokenType::DoubleEqual | TokenType::ExclamEqual
        ) {
            let lhs = Box::new(comparison);
            let op = scanner.next_token();
            let rhs = Box::new(parse_equality(scanner, depth)?);
            comparison = Expr::Binary { lhs, op, rhs };
        } else {
            break Ok(comparison);
        }
    }
}

#[ridotto_macros::parser_traced]
fn parse_comparison<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Expr<'src>, RidottoError> {
    let mut bit_op = parse_bit_op(scanner, depth)?;
    loop {
        if matches!(
            scanner.peek_token().type_,
            TokenType::LeftAngle
                | TokenType::RightAngle
                | TokenType::LeftAngleEqual
                | TokenType::RightAngleEqual
        ) {
            let lhs = Box::new(bit_op);
            let op = scanner.next_token();
            let rhs = Box::new(parse_comparison(scanner, depth)?);
            bit_op = Expr::Binary { lhs, op, rhs };
        } else {
            break Ok(bit_op);
        }
    }
}

#[ridotto_macros::parser_traced]
fn parse_bit_op<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Expr<'src>, RidottoError> {
    let mut add_sub = parse_add_sub(scanner, depth)?;
    loop {
        if matches!(
            scanner.peek_token().type_,
            TokenType::DoubleLeftAngle
                | TokenType::DoubleRightAngle
                | TokenType::Amp
                | TokenType::Bar
                | TokenType::Caret
        ) {
            let lhs = Box::new(add_sub);
            let op = scanner.next_token();
            let rhs = Box::new(parse_bit_op(scanner, depth)?);
            add_sub = Expr::Binary { lhs, op, rhs };
        } else {
            break Ok(add_sub);
        }
    }
}

#[ridotto_macros::parser_traced]
fn parse_add_sub<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Expr<'src>, RidottoError> {
    let mut mul_div_mod = parse_mul_div_mod(scanner, depth)?;
    loop {
        if matches!(
            scanner.peek_token().type_,
            TokenType::Plus | TokenType::Minus
        ) {
            let lhs = Box::new(mul_div_mod);
            let op = scanner.next_token();
            let rhs = Box::new(parse_add_sub(scanner, depth)?);
            mul_div_mod = Expr::Binary { lhs, op, rhs };
        } else {
            break Ok(mul_div_mod);
        }
    }
}

#[ridotto_macros::parser_traced]
fn parse_mul_div_mod<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Expr<'src>, RidottoError> {
    let mut unary = parse_unary(scanner, depth)?;
    loop {
        if matches!(
            scanner.peek_token().type_,
            TokenType::Star | TokenType::Slash | TokenType::Percent
        ) {
            let lhs = Box::new(unary);
            let op = scanner.next_token();
            let rhs = Box::new(parse_mul_div_mod(scanner, depth)?);
            unary = Expr::Binary { lhs, op, rhs };
        } else {
            break Ok(unary);
        }
    }
}

#[ridotto_macros::parser_traced]
fn parse_unary<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Expr<'src>, RidottoError> {
    if matches!(
        scanner.peek_token().type_,
        TokenType::Exclam | TokenType::Minus | TokenType::Amp | TokenType::Star
    ) {
        let op = scanner.next_token();
        let rhs = Box::new(parse_unary(scanner, depth)?);
        Ok(Expr::Unary { op, rhs })
    } else {
        parse_call(scanner, depth)
    }
}

#[ridotto_macros::parser_traced]
fn parse_call<'src>(scanner: &mut Scanner<'src>, depth: usize) -> Result<Expr<'src>, RidottoError> {
    let mut primary = parse_primary(scanner, depth)?;
    loop {
        match scanner.peek_token().type_ {
            TokenType::LeftParen => {
                let callee = Box::new(primary);

                // TODO handle named arguments?
                let args = maybe_comma_delimited(
                    scanner,
                    depth,
                    TokenType::LeftParen,
                    TokenType::RightParen,
                    parse_expr,
                )?;

                if let Ok(ExprToTypeName::TypeName(type_name)) =
                    ExprToTypeName::try_from(callee.as_ref())
                {
                    primary = Expr::TupleInstantiate {
                        type_name,
                        values: args,
                    };
                } else {
                    primary = Expr::Call { callee, args };
                }
            }

            TokenType::Period => {
                consume(scanner, TokenType::Period, depth)?;
                let object = Box::new(primary);
                if let TokenType::Int(int) = scanner.peek_token().type_ {
                    let index = consume(scanner, TokenType::Int(int), depth)?;
                    primary = Expr::TupleIndex { object, index }
                } else {
                    let name = consume_ident(scanner, depth)?;
                    primary = Expr::GetFrom { object, name };
                }
            }

            TokenType::LeftBrace => {
                let name = Box::new(primary);
                match ExprToTypeName::try_from(name.as_ref()) {
                    Ok(ExprToTypeName::TypeName(type_name)) => {
                        let values = maybe_comma_delimited(
                            scanner,
                            depth,
                            TokenType::LeftBrace,
                            TokenType::RightBrace,
                            parse_struct_field,
                        )?;

                        if let Some((_, value)) = values.iter().enumerate().find(|(i, value)| {
                            i + 1 != values.len() && matches!(value, StructField::Spread { .. })
                        }) {
                            let StructField::Spread { value } = value else {
                                unreachable!();
                            };
                            return Err(RidottoError::spread_or_rest_not_last(value.token()));
                        }

                        primary = Expr::StructInstantiate { type_name, values };
                    }
                    Ok(ExprToTypeName::Not(_)) | Err(_) => break (Ok(*name)),
                }
            }

            _ => break (Ok(primary)),
        }
    }
}

#[ridotto_macros::parser_traced]
fn parse_struct_field<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<StructField<'src>, RidottoError> {
    if scanner.peek_token().type_ == TokenType::Period {
        consume(scanner, TokenType::Period, depth)?;
        consume(scanner, TokenType::Period, depth)?;
        return Ok(StructField::Spread {
            value: parse_expr(scanner, depth)?,
        });
    }

    let name = consume_lower(scanner, depth)?;
    if scanner.peek_token().type_ == TokenType::Colon {
        consume(scanner, TokenType::Colon, depth)?;
        let value = parse_expr(scanner, depth)?;
        Ok(StructField::Named { name, value })
    } else {
        Ok(StructField::Shorthand { name })
    }
}

#[ridotto_macros::parser_traced]
fn parse_primary<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Expr<'src>, RidottoError> {
    match scanner.peek_token().type_ {
        TokenType::LowerIdent => Ok(Expr::Variable {
            variable: consume_lower(scanner, depth)?,
        }),

        TokenType::UpperIdent => Ok(Expr::TypeName {
            type_: consume_upper(scanner, depth)?,
        }),

        literal @ (TokenType::Int(_)
        | TokenType::Float(_)
        | TokenType::True
        | TokenType::False
        | TokenType::String) => Ok(Expr::Literal {
            literal: consume(scanner, literal, depth)?,
        }),

        TokenType::LeftBrace => {
            let stmts = parse_brace_delimited_stmts(scanner, depth)?;
            Ok(Expr::Block { stmts })
        }

        TokenType::LeftParen => {
            let mut values = comma_delimited(
                scanner,
                depth,
                TokenType::LeftParen,
                TokenType::RightParen,
                parse_expr,
            )?;
            if values.len() == 1 {
                Ok(Expr::Paren {
                    expr: Box::new(values.pop().unwrap()),
                })
            } else {
                Ok(Expr::Tuple { values })
            }
        }

        _ => Err(RidottoError::expected_expression(scanner.peek_token())),
    }
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
    } else if scanner.peek_token().type_ == TokenType::Star {
        consume(scanner, TokenType::Star, depth)?;
        Ok(TypeExpr::Ptr {
            pointee: Box::new(parse_type_expr(scanner, depth)?),
        })
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
    let inner = comma_delimited(
        scanner,
        depth,
        TokenType::LeftParen,
        TokenType::RightParen,
        parse_type_expr,
    )?;
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
    comma_delimited(
        scanner,
        depth,
        TokenType::LeftBracket,
        TokenType::RightBracket,
        parse_type_expr,
    )
}

#[ridotto_macros::parser_traced]
fn parse_type_expr_no_default<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<TypeExprNoDefault<'src>, RidottoError> {
    let name = parse_type_name(scanner, depth)?;

    if scanner.peek_token().type_ == TokenType::LeftBracket {
        let type_args = comma_delimited(
            scanner,
            depth,
            TokenType::LeftBracket,
            TokenType::RightBracket,
            parse_type_expr_no_default,
        )?;
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
    match consume_ident(scanner, depth)? {
        Identifier::NameUppercase(uppercase) => {
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
        }
        Identifier::NameLowercase(lowercase) => Ok(TypeName::TypeVar(lowercase)),
    }
}

#[ridotto_macros::parser_traced]
fn comma_delimited<'src, T>(
    scanner: &mut Scanner<'src>,
    depth: usize,
    left_delimiter: TokenType,
    right_delimiter: TokenType,
    parse_inner: fn(&mut Scanner<'src>, usize) -> Result<T, RidottoError>,
) -> Result<Vec<T>, RidottoError> {
    let mut inner = Vec::new();
    consume(scanner, left_delimiter, depth)?;
    while scanner.peek_token().type_ != right_delimiter {
        inner.push(parse_inner(scanner, depth)?);
        if scanner.peek_token().type_ != right_delimiter {
            consume(scanner, TokenType::Comma, depth)?;
        }
    }
    if !inner.is_empty() && scanner.peek_token().type_ == TokenType::Comma {
        scanner.next_token();
    }
    consume(scanner, right_delimiter, depth)?;
    Ok(inner)
}

#[ridotto_macros::parser_traced]
fn maybe_comma_delimited<'src, T>(
    scanner: &mut Scanner<'src>,
    depth: usize,
    left_delimiter: TokenType,
    right_delimiter: TokenType,
    parse_inner: fn(&mut Scanner<'src>, usize) -> Result<T, RidottoError>,
) -> Result<Vec<T>, RidottoError> {
    let mut inner = Vec::new();
    consume(scanner, left_delimiter, depth)?;
    while scanner.peek_token().type_ != right_delimiter {
        inner.push(parse_inner(scanner, depth)?);
        if scanner.peek_token().type_ == TokenType::Comma {
            consume(scanner, TokenType::Comma, depth)?;
        }
    }
    if !inner.is_empty() && scanner.peek_token().type_ == TokenType::Comma {
        scanner.next_token();
    }
    consume(scanner, right_delimiter, depth)?;
    Ok(inner)
}

fn consume_ident<'src>(
    scanner: &mut Scanner<'src>,
    depth: usize,
) -> Result<Identifier<'src>, RidottoError> {
    trace(depth, "- ", "consume_ident", scanner);
    let name = scanner.next_token();
    if name.type_.is_ident() {
        match name.type_ {
            TokenType::UpperIdent => {
                Ok(Identifier::NameUppercase(NameUppercase { uppercase: name }))
            }
            TokenType::LowerIdent => {
                Ok(Identifier::NameLowercase(NameLowercase { lowercase: name }))
            }
            _ => unreachable!(),
        }
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
