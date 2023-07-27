use std::ops::Range;

use thiserror::Error;

use crate::{
    ast::{self, TypedName},
    lexer::Lexer,
    tokens::{Token, TokenType},
};

const DUMMY_AST_ID: usize = 0;

#[derive(Error, Debug, Clone)]
#[error("Syntax error [{span:?}] {msg}")]
pub struct SyntaxError {
    pub span: Range<usize>,
    pub msg: String,
}

enum ParseErr {
    /// The rule matched but ended with an error.
    /// The error has already been reported and recovered.
    Err(SyntaxError),
    /// Rule could not match.
    WrongRule,
}

type ParseRes<T> = Result<T, ParseErr>;

trait SyntaxContext
where
    Self: Sized,
{
    fn error(self, parser: &mut Parser, beg: usize, msg: &str) -> Self {
        self.error_with_span(parser, beg..parser.curr_pos(), msg)
    }

    fn error_with_span(self, parser: &mut Parser, range: Range<usize>, msg: &str) -> Self;
}

impl<T> SyntaxContext for ParseRes<T> {
    fn error_with_span(self, parser: &mut Parser, span: Range<usize>, msg: &str) -> Self {
        self.map_err(|_| parser.error(span, msg))
    }
}

pub struct Parser<'source> {
    lexer: Lexer<'source>,
    errors: Vec<SyntaxError>,
}

impl<'source> Parser<'source> {
    pub fn parse(&mut self) -> Result<ast::Program, Vec<SyntaxError>> {
        let mut items = Vec::new();
        loop {
            if self.eof() {
                if !self.errors.is_empty() {
                    return Err(self.errors.clone());
                }
                return Ok(ast::Program { items });
            }
            if let Ok(str_def) = self.parse_type_definition() {
                items.push(ast::TopLevelItem::TypeDefinition(str_def));
            } else if let Ok(fun_def) = self.parse_function_definition(true) {
                items.push(ast::TopLevelItem::FunctionDefinition(fun_def));
            } else if let Ok(meth_block) = self.parse_methods_block() {
                items.push(ast::TopLevelItem::MethodsBlock(meth_block));
            } else if let Ok(const_def) = self.parse_const_definition() {
                items.push(ast::TopLevelItem::ConstDefinition(const_def))
            } else {
                self.error(
                    self.lexer.current().span.clone(),
                    format!(
                        "Expected function or struct definition got {:?}",
                        &self.lexer.current()
                    ),
                );
                // panic on first syntax error, no recovery yet
                return Err(self.errors.clone());
            }
        }
    }

    fn parse_const_definition(&mut self) -> ParseRes<ast::ConstDefinition> {
        self.match_token(TokenType::Const)?;
        let name = self.parse_identifier()?;
        let _ = self.expect_token(TokenType::Colon);
        let typ = self.parse_type()?;
        let _ = self.expect_token(TokenType::Assignment);
        let init = self.parse_expr()?;
        let _ = self.expect_token(TokenType::Semicolon);
        Ok(ast::ConstDefinition {
            id: DUMMY_AST_ID,
            name,
            typ,
            init,
        })
    }

    fn parse_methods_block(&mut self) -> ParseRes<ast::MethodsBlock> {
        let beg = self.curr_pos();
        self.match_token(TokenType::Methods)?;
        let typ = self.parse_type().error(self, beg, "Expected a type")?;
        let _ = self.expect_token(TokenType::LBrace);
        let mut methods = Vec::new();
        while self.lexer.current().typ != TokenType::RBrace {
            let method = self.parse_function_definition(true)?;
            methods.push(method);
        }
        let _ = self.expect_token(TokenType::RBrace);
        let _ = self.expect_token(TokenType::Semicolon);
        Ok(ast::MethodsBlock {
            name: None,
            typ,
            methods,
        })
    }

    fn parse_type_definition(&mut self) -> ParseRes<ast::TypeDefinition> {
        let beg = self.curr_pos();
        self.match_token(TokenType::Type)?;
        let name =
            self.parse_identifier()
                .error(self, beg, "Expected a name after a type keyword")?;
        let type_variables = self.parse_optional_type_variables().error(
            self,
            beg,
            "Expected type variables list",
        )?;
        if self.match_token(TokenType::LBrace).is_ok() {
            let possibly_adt = self.in_scope(|p| p.parse_variants_definition());
            let definition = if let Ok(adt) = possibly_adt {
                adt
            } else {
                let fields =
                    self.parse_delimited(Self::parse_typed_identifier, TokenType::Semicolon);
                ast::DefinedType::Record(ast::Record { fields })
            };
            self.expect_token(TokenType::RBrace)?;
            let _ = self.expect_token(TokenType::Semicolon);
            Ok(ast::TypeDefinition {
                name,
                type_variables,
                definition,
                span: beg..self.curr_pos(),
            })
        } else if self.match_token(TokenType::Assignment).is_ok() {
            let for_type =
                self.parse_type()
                    .error(self, beg, "Expected a type in type alias definition")?;
            let _ = self.expect_token(TokenType::Semicolon);
            Ok(ast::TypeDefinition {
                name,
                type_variables,
                definition: ast::DefinedType::Alias(ast::Alias { for_type }),
                span: beg..self.curr_pos(),
            })
        } else {
            Err(self.error(
                beg..self.curr_pos(),
                "Expected an alias or a type definition",
            ))
        }
    }

    fn parse_variants_definition(&mut self) -> ParseRes<ast::DefinedType> {
        fn parse_discriminant(parser: &mut Parser) -> ParseRes<ast::Discriminant> {
            let constructor = parser.parse_identifier()?;
            // possibly a record definition
            if parser.match_token(TokenType::Colon).is_ok() {
                return wrong_rule();
            }
            if parser.match_token(TokenType::LParen).is_ok() {
                let arguments = parser.parse_delimited(Parser::parse_type, TokenType::Comma);
                let _ = parser.expect_token(TokenType::RParen)?;
                return Ok(ast::Discriminant {
                    constructor,
                    arguments: ast::DiscriminantKind::Tuple(arguments),
                });
            }
            if parser.match_token(TokenType::LBrace).is_ok() {
                let fields =
                    parser.parse_delimited(Parser::parse_typed_identifier, TokenType::Comma);
                let _ = parser.expect_token(TokenType::RBrace)?;
                let fields = fields
                    .into_iter()
                    .map(|field| (field.name, field.typ))
                    .collect();
                return Ok(ast::Discriminant {
                    constructor,
                    arguments: ast::DiscriminantKind::Record(fields),
                });
            }
            Ok(ast::Discriminant {
                constructor,
                arguments: ast::DiscriminantKind::Empty,
            })
        }
        let discriminants = self.parse_delimited(parse_discriminant, TokenType::Semicolon);
        if discriminants.is_empty() {
            wrong_rule()
        } else {
            Ok(ast::DefinedType::ADT(ast::ADT { discriminants }))
        }
    }
    fn parse_optional_type_variables(&mut self) -> ParseRes<Vec<ast::TypeVariable>> {
        if self.match_token(TokenType::LParen).is_err() {
            return Ok(Vec::new());
        }
        let type_vars = self
            .parse_delimited(Self::parse_identifier, TokenType::Comma)
            .into_iter()
            .map(|name| ast::TypeVariable { name })
            .collect();
        let _ = self.expect_token(TokenType::RParen);
        Ok(type_vars)
    }

    fn parse_attributes(&mut self) -> Vec<String> {
        let mut attributes = Vec::new();
        while let TokenType::Attribute(a) = self.lexer.current().typ.clone() {
            attributes.push(a);
            self.lexer.advance();
        }
        attributes
    }

    fn parse_function_definition(
        &mut self,
        eat_semicolon: bool,
    ) -> ParseRes<ast::FunctionDefinition> {
        let beg = self.curr_pos();
        let attributes = self.parse_attributes();
        let name = self.parse_identifier()?;
        let typ = match self.match_token(TokenType::Colon) {
            Ok(_) => {
                let typ = self.parse_type()?;
                let _ = self.expect_token(TokenType::Semicolon);
                typ
            }
            Err(_) => ast::Typ::ToInfere,
        };
        if attributes.contains(&"empty".to_owned()) {
            return Ok(ast::FunctionDefinition {
                name,
                typ,
                parameters: Vec::new(),
                body: mk_expr(ast::ExprKind::Unit),
                attributes,
            });
        }
        if typ != ast::Typ::ToInfere {
            let name_repeated = self.parse_identifier().error(
                self,
                beg,
                "Expected a function definition following its declaration",
            )?;
            if name != name_repeated {
                return Err(self.error(
                    beg..self.curr_pos(),
                    "The name in the function's declaration and definition have to be the same",
                ));
            }
        }
        let parameters = self.parse_argument_list()?;
        let body = if self.lexer.current().typ != TokenType::LBrace {
            let _ = self.match_token(TokenType::Assignment).error(
                self,
                beg,
                "Missing = in the function definition",
            );
            self.parse_expr()?
        } else {
            self.parse_expr_sequence()?
        };
        if eat_semicolon {
            let _ = self.expect_token(TokenType::Semicolon);
        }
        let typ = match typ {
            ast::Typ::ToInfere => {
                let args = parameters
                    .iter()
                    .map(|_| ast::Typ::ToInfere)
                    .collect::<Vec<_>>();
                ast::Typ::Function {
                    generics: Vec::new(),
                    args,
                    ret: Box::new(ast::Typ::ToInfere),
                }
            }
            t => t,
        };
        Ok(ast::FunctionDefinition {
            name,
            typ,
            parameters,
            body,
            attributes,
        })
    }

    fn parse_argument_list(&mut self) -> ParseRes<Vec<String>> {
        let _ = self.match_token(TokenType::LParen);
        let args = self.parse_delimited(Self::parse_identifier, TokenType::Comma);
        let _ = self.match_token(TokenType::RParen);
        Ok(args)
    }

    fn parse_typed_identifier(&mut self) -> ParseRes<TypedName> {
        let beg = self.curr_pos();
        let name = self.parse_identifier()?;
        let _colon =
            self.match_token(TokenType::Colon)
                .error(self, beg, &self.expected_err_msg("a colon"));
        let typ = self
            .parse_type()
            .error(self, beg, &self.expected_err_msg("a type"))?;
        Ok(ast::TypedName { name, typ })
    }

    fn parse_identifier(&mut self) -> ParseRes<String> {
        if let Token {
            typ: TokenType::Identifier(_),
            ..
        } = self.lexer.current()
        {
            let id = self.lexer.advance();
            match id.typ {
                TokenType::Identifier(id) => Ok(id),
                _ => unreachable!("we already checked that this token is an identifier"),
            }
        } else {
            wrong_rule()
        }
    }

    fn parse_type(&mut self) -> ParseRes<ast::Typ> {
        let generics = match self.match_token(TokenType::Forall) {
            Ok(_) => {
                let mut items = Vec::new();
                while let Ok(id) = self.parse_identifier() {
                    items.push(id);
                }
                let _ = self.expect_token(TokenType::FatArrow);
                items
            }
            _ => Vec::new(),
        };
        let typ = self.parse_atomic_type()?;
        if self.match_token(TokenType::LParen).is_err() {
            return match typ {
                ast::Typ::Function { args, ret, .. } => Ok(ast::Typ::Function {
                    args,
                    ret,
                    generics,
                }),
                t => Ok(ast::Typ::Poly(generics, Box::new(t))),
            };
        }
        // type aplication
        let args = self.parse_delimited(Self::parse_type, TokenType::Comma);
        let _ = self.expect_token(TokenType::RParen);
        Ok(ast::Typ::Poly(
            generics,
            Box::new(ast::Typ::Application {
                callee: Box::new(typ),
                args,
            }),
        ))
    }

    fn parse_atomic_type(&mut self) -> ParseRes<ast::Typ> {
        let beg = self.curr_pos();
        match self.lexer.current().typ.clone() {
            TokenType::Identifier(name) => {
                self.lexer.advance();
                Ok(ast::Typ::Name(name))
            }
            TokenType::LBracket => {
                self.lexer.advance();
                let inner_t = self.parse_type()?;
                self.expect_token(TokenType::RBracket)?;
                Ok(ast::Typ::Array(Box::new(inner_t)))
            }
            TokenType::LParen => {
                self.lexer.advance();
                let inner_t = match self.parse_type() {
                    Err(ParseErr::WrongRule) => {
                        if self.match_token(TokenType::RParen).is_err() {
                            return Err(self.error(beg..self.curr_pos(), "Cannot parse a type"));
                        }
                        ast::Typ::Unit
                    }
                    e @ Err(_) => return e,
                    Ok(inner_t) => {
                        let t = if self.match_token(TokenType::Comma).is_ok() {
                            let mut rest = self.parse_delimited(Self::parse_type, TokenType::Comma);
                            let mut tuple = vec![inner_t];
                            tuple.append(&mut rest);
                            ast::Typ::Tuple(tuple)
                        } else {
                            inner_t
                        };
                        self.expect_token(TokenType::RParen)?;
                        t
                    }
                };
                if self.match_token(TokenType::Arrow).is_ok() {
                    let ret = self.parse_type()?;
                    return Ok(ast::Typ::Function {
                        args: match inner_t {
                            ast::Typ::Tuple(args) => args,
                            ast::Typ::Unit => Vec::new(),
                            other => vec![other],
                        },
                        generics: Vec::new(),
                        ret: Box::new(ret),
                    });
                }
                Ok(inner_t)
            }
            _ => wrong_rule(),
        }
    }

    fn parse_delimited<T>(
        &mut self,
        parser: impl Fn(&mut Self) -> ParseRes<T>,
        delimiter: TokenType,
    ) -> Vec<T> {
        let mut items = Vec::new();
        while let Ok(item) = parser(self) {
            items.push(item);
            if self.lexer.current().typ != delimiter {
                break;
            }
            self.lexer.advance();
        }
        items
    }

    fn parse_expr(&mut self) -> ParseRes<ast::Expr> {
        match self.lexer.current().typ.clone() {
            TokenType::If => self.parse_if(),
            TokenType::While => self.parse_while(),
            TokenType::Return => self.parse_return(),
            TokenType::Let => self.parse_let(),
            TokenType::Type => self.parse_type_expr(),
            _ => self.parse_assignment(),
        }
    }

    fn parse_assignment(&mut self) -> ParseRes<ast::Expr> {
        let lhs = self.parse_typed_expression()?;
        if self.match_token(TokenType::Assignment).is_err() {
            return Ok(lhs);
        }
        let rhs = self.parse_typed_expression()?;
        Ok(mk_expr(ast::ExprKind::Assignment {
            lval: Box::new(lhs),
            rval: Box::new(rhs),
        }))
    }

    fn parse_typed_expression(&mut self) -> ParseRes<ast::Expr> {
        let beg = self.curr_pos();
        let expr = self.parse_postfix_match()?;
        if self.match_token(TokenType::Colon).is_err() {
            return Ok(expr);
        }
        let typ = self
            .parse_type()
            .error(self, beg, "expected a type for a typed expression")?;
        Ok(mk_expr(ast::ExprKind::TypedExpr {
            expr: Box::new(expr),
            typ,
        }))
    }

    fn parse_postfix_match(&mut self) -> ParseRes<ast::Expr> {
        let expr = self.parse_postfix_operators()?;
        if self.match_token(TokenType::Match).is_err() {
            return Ok(expr);
        }
        let _ = self.expect_token(TokenType::LBrace);
        let mut cases = Vec::new();
        while self.lexer.current().typ != TokenType::RBrace {
            let pattern = self.parse_pattern()?;
            let guard = match self.match_token(TokenType::If) {
                Ok(_) => Some(Box::new(self.parse_expr()?)),
                _ => None,
            };
            let _ = self.expect_token(TokenType::FatArrow);
            let body = self.parse_expr()?;
            let _ = self.match_token(TokenType::Semicolon);
            cases.push((pattern, body, guard));
        }
        let _ = self.expect_token(TokenType::RBrace);
        Ok(mk_expr(ast::ExprKind::Match {
            expr: Box::new(expr),
            cases,
        }))
    }

    fn parse_fn_call(&mut self, func: ast::Expr) -> ParseRes<ast::Expr> {
        let beg = self.curr_pos();
        let args: Vec<ast::Expr> = if self.match_token(TokenType::LParen).is_ok() {
            // this is a function call with arguments passed in parentheses
            // or a function call in a special form:
            // func (a, b) => { lambda body }
            let mut args = self.parse_delimited(Self::parse_expr, TokenType::Comma);
            let _ =
                self.match_token(TokenType::RParen)
                    .error(self, beg, "Missing closing parenthesis");

            if self.match_token(TokenType::FatArrow).is_ok() {
                // this means that we probably have a call in form
                // func (a, b) => { a + b }
                // which is a function call with trailing lambda with arguments
                let parameters = args
                    .into_iter()
                    .map(|arg| match arg.kind {
                        ast::ExprKind::Name(v) => Ok((v, ast::Typ::ToInfere)),
                        ast::ExprKind::TypedExpr { expr, typ } => match expr.kind {
                            ast::ExprKind::Name(name) => Ok((name, typ)),
                            _ => Err(ParseErr::Err(SyntaxError {
                                msg: "Expected a lambda's arguments list".into(),
                                span: beg..self.curr_pos(),
                            })),
                        },
                        _ => Err(ParseErr::Err(SyntaxError {
                            msg: "Expected a lambda's arguments list".into(),
                            span: beg..self.curr_pos(),
                        })),
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let body =
                    self.parse_expr()
                        .error(self, beg, "Expected a trailing lambda's body")?;
                let lambda = ast::ExprKind::Lambda {
                    parameters: parameters
                        .into_iter()
                        .map(|(name, typ)| ast::TypedName { name, typ })
                        .collect(),
                    body: Box::new(body),
                };
                vec![mk_expr(lambda)]
            } else {
                // todo:
                // form of func(args) this => { lambda body }
                // form of func(args) (a, b) => { lambda body }
                // form of func(args) { lambda body }
                match self.lexer.current().typ.clone() {
                    TokenType::Identifier(name) => {
                        // form of func(args) this => { lambda body }
                        let _ = self.expect_token(TokenType::FatArrow);
                        let body = self.parse_expr().error(
                            self,
                            beg,
                            "expected trailing lambda's body",
                        )?;
                        let lambda = ast::ExprKind::Lambda {
                            parameters: vec![ast::TypedName {
                                name,
                                typ: ast::Typ::ToInfere,
                            }],
                            body: Box::new(body),
                        };
                        args.push(mk_expr(lambda));
                        args
                    }
                    TokenType::LParen => {
                        // form of func(args) (parameters) => { lambda body }
                        self.lexer.advance();
                        let parameters = self
                            .parse_delimited(Self::parse_identifier, TokenType::Comma)
                            .into_iter()
                            .map(|name| ast::TypedName {
                                name,
                                typ: ast::Typ::ToInfere,
                            })
                            .collect();
                        let _ = self.expect_token(TokenType::RParen);
                        let _ = self.expect_token(TokenType::FatArrow);
                        let body = self.parse_expr().error(
                            self,
                            beg,
                            "expected trailing lambda's body",
                        )?;
                        let lambda = ast::ExprKind::Lambda {
                            parameters,
                            body: Box::new(body),
                        };
                        args.push(mk_expr(lambda));
                        args
                    }
                    TokenType::LBrace => {
                        // form of func(args) { lambda body }
                        let body = self.parse_expr_sequence().error(
                            self,
                            beg,
                            "expected trailing lambda's body",
                        )?;
                        let lambda = ast::ExprKind::Lambda {
                            parameters: Vec::new(),
                            body: Box::new(body),
                        };
                        args.push(mk_expr(lambda));
                        args
                    }
                    _ => args,
                }
            }
        } else {
            // form of func { lambda body }
            // form of func a => { lambda body }
            // form of func arg
            // if there is no name, rbrace following then its just a normal expression
            let mut check_for_trailing_lambda = false;
            let mut args = match self.parse_term().map(|e| e.kind) {
                // no name or brace following, just a normal expression
                Err(ParseErr::WrongRule) => return Ok(func),
                Err(e) => return Err(e),
                Ok(ast::ExprKind::Name(name)) => {
                    if self.match_token(TokenType::FatArrow).is_ok() {
                        // func name => { lambda body }
                        let body = self.parse_expr().error(
                            self,
                            beg,
                            "Expected trailing lambda's body",
                        )?;
                        let lambda = ast::ExprKind::Lambda {
                            parameters: vec![ast::TypedName {
                                name,
                                typ: ast::Typ::ToInfere,
                            }],
                            body: Box::new(body),
                        };
                        vec![lambda]
                    } else {
                        // func name
                        check_for_trailing_lambda = true;
                        vec![ast::ExprKind::Name(name)]
                    }
                }
                Ok(body @ ast::ExprKind::Sequence(_)) => {
                    // func { lambda body }
                    let lambda = ast::ExprKind::Lambda {
                        parameters: Vec::new(),
                        body: Box::new(mk_expr(body)),
                    };
                    vec![lambda]
                }
                Ok(val) => {
                    // func expr
                    check_for_trailing_lambda = true;
                    vec![val]
                }
            };
            if check_for_trailing_lambda {
                match self.lexer.current().typ.clone() {
                    TokenType::Identifier(name) => {
                        // a => expr
                        self.lexer.advance();
                        let parameters = vec![ast::TypedName {
                            name,
                            typ: ast::Typ::ToInfere,
                        }];
                        let _ = self.expect_token(TokenType::FatArrow);
                        let body =
                            self.parse_expr()
                                .error(self, beg, "expected trailing lambdas body")?;
                        args.push(ast::ExprKind::Lambda {
                            parameters,
                            body: Box::new(body),
                        });
                    }
                    TokenType::LParen => {
                        // (args) => expr
                        self.lexer.advance();
                        let parameters =
                            self.parse_delimited(Self::parse_possibly_typed_name, TokenType::Comma);
                        let _ = self.expect_token(TokenType::RParen);
                        let _ = self.expect_token(TokenType::FatArrow);
                        let body = self.parse_expr_sequence().error(
                            self,
                            beg,
                            "Expected trailing lambdas body",
                        )?;
                        args.push(ast::ExprKind::Lambda {
                            parameters,
                            body: Box::new(body),
                        })
                    }
                    TokenType::LBrace => {
                        // no args trailing lambda
                        // { expr }
                        let body = self.parse_expr_sequence().error(
                            self,
                            beg,
                            "Expected trailing lambdas body",
                        )?;
                        args.push(ast::ExprKind::Lambda {
                            parameters: Vec::new(),
                            body: Box::new(body),
                        });
                    }
                    _ => {}
                }
            }
            args.into_iter().map(mk_expr).collect()
        };

        let kwargs = args
            .iter()
            .filter_map(|expr| match &expr.kind {
                ast::ExprKind::Assignment { lval, rval } => {
                    if let ast::ExprKind::Name(name) = &lval.as_ref().kind {
                        Some((name.clone(), rval.as_ref().clone()))
                    } else {
                        panic!("Invalid assignment in a function call");
                    }
                }
                _ => None,
            })
            .collect();
        let args = args
            .iter()
            .filter(|expr| !matches!(expr.kind, ast::ExprKind::Assignment { .. }))
            .cloned()
            .collect();
        // todo split args into kwargs and args
        Ok(mk_expr(ast::ExprKind::FunctionCall {
            func: Box::new(func),
            args,
            kwargs,
        }))
    }

    fn parse_postfix_operators(&mut self) -> ParseRes<ast::Expr> {
        let beg = self.curr_pos();
        let mut lhs = self.parse_term()?;
        loop {
            if self.match_token(TokenType::LBracket).is_ok() {
                let index = self
                    .parse_expr()
                    .error(self, beg, "Expected expression on access")?;
                let _ = self.expect_token(TokenType::RBracket);
                lhs = mk_expr(ast::ExprKind::IndexAccess {
                    lhs: Box::new(lhs),
                    index: Box::new(index),
                });
                continue;
            }
            if self.match_token(TokenType::Dot).is_ok() {
                let field =
                    self.parse_identifier()
                        .error(self, beg, "A field has to be an indentifier")?;
                lhs = mk_expr(ast::ExprKind::Access {
                    val: Box::new(lhs),
                    field,
                });
                let curr = self.lexer.current().typ.clone();
                match curr {
                    TokenType::LParen
                    | TokenType::LBrace
                    | TokenType::Identifier(_)
                    | TokenType::False
                    | TokenType::True
                    | TokenType::String(_)
                    | TokenType::Integer(_)
                    | TokenType::Float(_)
                    | TokenType::Array => match self.parse_fn_call(lhs)?.kind {
                        ast::ExprKind::FunctionCall {
                            func:
                                box ast::Expr {
                                    kind:
                                        ast::ExprKind::Access {
                                            val: receiver,
                                            field: method,
                                        },
                                    ..
                                },
                            args,
                            kwargs,
                        } => {
                            lhs = mk_expr(ast::ExprKind::MethodCall {
                                receiver,
                                method,
                                args,
                                kwargs,
                            });
                        }
                        e => unreachable!("parse_fn_call returned an invalid expression {:?}", e),
                    },
                    _ => {}
                }
                continue;
            }
            let curr = self.lexer.current().typ.clone();
            match curr {
                TokenType::LParen
                | TokenType::LBrace
                | TokenType::Identifier(_)
                | TokenType::False
                | TokenType::True
                | TokenType::String(_)
                | TokenType::Integer(_)
                | TokenType::Float(_)
                | TokenType::Array => {
                    lhs = self.parse_fn_call(lhs)?;
                    continue;
                }
                _ => {}
            }
            break;
        }
        Ok(lhs)
    }

    fn parse_term(&mut self) -> ParseRes<ast::Expr> {
        let beg = self.curr_pos();
        let tok = self.lexer.current().typ.clone();
        match tok {
            TokenType::Integer(val) => {
                self.lexer.advance();
                Ok(mk_expr(ast::ExprKind::IntConst(val)))
            }
            TokenType::Float(val) => {
                self.lexer.advance();
                Ok(mk_expr(ast::ExprKind::FloatConst(val)))
            }
            TokenType::True => {
                self.lexer.advance();
                Ok(mk_expr(ast::ExprKind::BoolConst(true)))
            }
            TokenType::False => {
                self.lexer.advance();
                Ok(mk_expr(ast::ExprKind::BoolConst(false)))
            }
            TokenType::Identifier(name) => {
                self.lexer.advance();
                if self.match_token(TokenType::FatArrow).is_ok() {
                    // lambda literal in form
                    // arg => expr
                    let body = self
                        .parse_expr()
                        .error(self, beg, "expected lambdas body")?;
                    return Ok(mk_expr(ast::ExprKind::Lambda {
                        parameters: vec![ast::TypedName {
                            name,
                            typ: ast::Typ::ToInfere,
                        }],
                        body: Box::new(body),
                    }));
                }
                Ok(mk_expr(ast::ExprKind::Name(name)))
            }
            TokenType::LParen => {
                self.lexer.advance();
                if self.match_token(TokenType::RParen).is_ok() {
                    if !self.match_token(TokenType::FatArrow).is_ok() {
                        return Ok(mk_expr(ast::ExprKind::Unit));
                    }
                    let body = self.parse_expr().error(
                        self,
                        beg,
                        "expected a lambda's body after an empty lambda's parameters list",
                    )?;
                    return Ok(mk_expr(ast::ExprKind::Lambda {
                        parameters: Vec::new(),
                        body: Box::new(body),
                    }));
                }
                let inner = self.parse_expr()?;
                if self.match_token(TokenType::Comma).is_err() {
                    let _ = self.match_token(TokenType::RParen).error(
                        self,
                        beg,
                        "Missing closing parenthesis",
                    );
                    if self.match_token(TokenType::FatArrow).is_ok() {
                        // this is a lambda literal in form
                        // (a) => expr
                        return match inner.kind {
                            ast::ExprKind::Name(name) => {
                                // (name) => expr
                                let body = self.parse_expr()?;
                                Ok(mk_expr(ast::ExprKind::Lambda {
                                    parameters: vec![ast::TypedName {
                                        name,
                                        typ: ast::Typ::ToInfere,
                                    }],
                                    body: Box::new(body),
                                }))
                            }
                            ast::ExprKind::TypedExpr { expr, typ } => match expr.kind {
                                ast::ExprKind::Name(name) => {
                                    // (name: type) => expr
                                    let body = self.parse_expr()?;
                                    Ok(mk_expr(ast::ExprKind::Lambda {
                                        parameters: vec![ast::TypedName { name, typ }],
                                        body: Box::new(body),
                                    }))
                                }
                                _ => Err(ParseErr::Err(SyntaxError {
                                    span: beg..self.curr_pos(),
                                    msg: "lambdas parameters must be names".into(),
                                })),
                            },
                            _ => Err(ParseErr::Err(SyntaxError {
                                span: beg..self.curr_pos(),
                                msg: "lambdas parameters must be names".into(),
                            })),
                        };
                    }
                    return Ok(inner);
                }
                let mut tuple_tail = self.parse_delimited(Self::parse_expr, TokenType::Comma);
                let mut tuple = vec![inner];
                tuple.append(&mut tuple_tail);
                let _ = self.match_token(TokenType::RParen).error(
                    self,
                    beg,
                    "Missing closing parenthesis",
                );
                if self.match_token(TokenType::FatArrow).is_ok() {
                    // this is a lambda literal in form
                    // (a, b, c) => expr
                    let parameters = tuple
                        .into_iter()
                        .map(|arg| match arg.kind {
                            ast::ExprKind::Name(v) => Ok((v, ast::Typ::ToInfere)),
                            ast::ExprKind::TypedExpr { expr, typ } => match expr.kind {
                                ast::ExprKind::Name(name) => Ok((name, typ)),
                                _ => Err(ParseErr::Err(SyntaxError {
                                    msg: "Expected a lambda's arguments list".into(),
                                    span: beg..self.curr_pos(),
                                })),
                            },
                            _ => Err(ParseErr::Err(SyntaxError {
                                msg: "Expected a lambda's arguments list".into(),
                                span: beg..self.curr_pos(),
                            })),
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    let body = self.parse_expr().error(self, beg, "missing lambdas body")?;
                    return Ok(mk_expr(ast::ExprKind::Lambda {
                        parameters: parameters
                            .into_iter()
                            .map(|(name, typ)| ast::TypedName { name, typ })
                            .collect(),
                        body: Box::new(body),
                    }));
                }
                Ok(mk_expr(ast::ExprKind::Tuple(tuple)))
            }
            TokenType::LBrace => self.parse_expr_sequence(),
            TokenType::String(s) => {
                self.lexer.advance();
                Ok(mk_expr(ast::ExprKind::StringConst(s)))
            }
            TokenType::Array => {
                self.lexer.advance();
                let size = if self.match_token(TokenType::LBracket).is_ok() {
                    let size = self.parse_expr();
                    let _ = self.expect_token(TokenType::RBracket);
                    match size {
                        Ok(e) => Some(Box::new(e)),
                        Err(_) => None,
                    }
                } else {
                    None
                };
                let t = self.parse_type();
                let init = if self.match_token(TokenType::LBrace).is_ok() {
                    let init = self.parse_delimited(Self::parse_expr, TokenType::Comma);
                    let _ = self.expect_token(TokenType::RBrace);
                    init
                } else {
                    vec![]
                };
                Ok(mk_expr(ast::ExprKind::ArrayLiteral {
                    size: size.unwrap_or_else(|| {
                        Box::new(mk_expr(ast::ExprKind::IntConst(init.len() as isize)))
                    }),
                    typ: match t {
                        Ok(t) => t,
                        Err(_) => ast::Typ::ToInfere,
                    },
                    init,
                }))
            }
            _ => wrong_rule(),
        }
    }

    fn parse_expr_sequence(&mut self) -> ParseRes<ast::Expr> {
        let beg = self.curr_pos();
        self.match_token(TokenType::LBrace)?;
        let body = self.parse_delimited(Self::parse_expr, TokenType::Semicolon);
        if body.is_empty() {
            return Err(self.error(beg..self.curr_pos(), "An expression block cannot be empty"));
        }
        let _ = self.match_token(TokenType::RBrace).error(
            self,
            beg,
            "Missing closing brace for the expression block",
        );
        Ok(mk_expr(ast::ExprKind::Sequence(body)))
    }

    fn parse_possibly_typed_name(&mut self) -> ParseRes<ast::TypedName> {
        let name = self.parse_identifier()?;
        if self.match_token(TokenType::Colon).is_err() {
            return Ok(ast::TypedName {
                name,
                typ: ast::Typ::ToInfere,
            });
        }
        let typ = self.parse_type()?;
        Ok(ast::TypedName { name, typ })
    }

    fn parse_type_expr(&mut self) -> ParseRes<ast::Expr> {
        self.match_token(TokenType::Type)?;
        let _ = self.expect_token(TokenType::LBrace);
        let fields = self.parse_delimited(Self::parse_anon_field_def, TokenType::Semicolon);
        let _ = self.expect_token(TokenType::RBrace);
        Ok(mk_expr(ast::ExprKind::AnonType { fields }))
    }

    fn parse_anon_field_def(&mut self) -> ParseRes<(String, ast::Expr)> {
        let beg = self.curr_pos();
        let name = self.parse_identifier()?;
        let _ = self.expect_token(TokenType::Assignment);
        let init = self.parse_expr().error(
            self,
            beg,
            "init expression is required in the let expression",
        )?;
        Ok((name, init))
    }

    fn parse_let(&mut self) -> ParseRes<ast::Expr> {
        let beg = self.curr_pos();
        self.match_token(TokenType::Let)?;
        if self.match_token(TokenType::Rec).is_ok() {
            println!("Parsing let rec");
            return self.parse_let_rec();
        }
        let name =
            self.parse_identifier()
                .error(self, beg, "expected a name after a let keyword")?;

        let typ = if self.match_token(TokenType::Colon).is_ok() {
            self.parse_type()?
        } else {
            ast::Typ::ToInfere
        };
        let _ = self.expect_token(TokenType::Assignment);
        let init = self.parse_expr().error(
            self,
            beg,
            "init expression is required in the let expression",
        )?;
        Ok(mk_expr(ast::ExprKind::Let {
            name,
            typ,
            init: Box::new(init),
        }))
    }

    fn parse_let_rec(&mut self) -> ParseRes<ast::Expr> {
        let definitions = if self.match_token(TokenType::LBrace).is_ok() {
            let mut res = Vec::new();
            while self.match_token(TokenType::RBrace).is_err() {
                res.push(self.parse_function_definition(true)?);
            }
            res
        } else {
            vec![self.parse_function_definition(false)?]
        };
        Ok(mk_expr(ast::ExprKind::LetRec { definitions }))
    }

    fn parse_pattern(&mut self) -> ParseRes<ast::Pattern> {
        let beg = self.curr_pos();
        let tok = self.lexer.current().typ.clone();
        match tok {
            TokenType::Integer(val) => {
                self.lexer.advance();
                Ok(mk_pattern(ast::PatternKind::Int(val)))
            }
            TokenType::True => {
                self.lexer.advance();
                Ok(mk_pattern(ast::PatternKind::Bool(true)))
            }
            TokenType::False => {
                self.lexer.advance();
                Ok(mk_pattern(ast::PatternKind::Bool(false)))
            }
            TokenType::String(s) => {
                self.lexer.advance();
                Ok(mk_pattern(ast::PatternKind::String(s)))
            }
            TokenType::LParen => {
                self.lexer.advance();
                if self.match_token(TokenType::RParen).is_ok() {
                    return Ok(mk_pattern(ast::PatternKind::Unit));
                }
                let inner = self.parse_delimited(Self::parse_pattern, TokenType::Comma);
                let _ = self.expect_token(TokenType::RParen);
                Ok(mk_pattern(ast::PatternKind::Tuple(inner)))
            }
            TokenType::LBracket => {
                self.lexer.advance();
                let inner = self.parse_delimited(Self::parse_pattern, TokenType::Comma);
                let _ = self.expect_token(TokenType::RBracket);
                Ok(mk_pattern(ast::PatternKind::Tuple(inner)))
            }
            TokenType::LBrace => {
                self.lexer.advance();
                let fields = self.parse_delimited(Self::parse_pattern_field, TokenType::Comma);
                let _ = self.expect_token(TokenType::RBrace);
                Ok(mk_pattern(ast::PatternKind::AnonStruct {
                    fields: fields.into_iter().collect(),
                }))
            }
            TokenType::Identifier(name) => {
                self.lexer.advance();
                if self.match_token(TokenType::LParen).is_ok() {
                    let inner = self.parse_delimited(Self::parse_pattern, TokenType::Comma);
                    let _ = self.expect_token(TokenType::RParen);
                    return Ok(mk_pattern(ast::PatternKind::TupleStruct {
                        strct: name,
                        fields: inner,
                    }));
                }
                if self.match_token(TokenType::LBrace).is_ok() {
                    let fields = self.parse_delimited(Self::parse_pattern_field, TokenType::Comma);
                    let _ = self.expect_token(TokenType::RBrace);
                    return Ok(mk_pattern(ast::PatternKind::Struct {
                        strct: name,
                        fields: fields.into_iter().collect(),
                    }));
                }
                if self.match_token(TokenType::Dot).is_ok() {
                    let case = self.parse_pattern()?;
                    if !matches!(
                        &case.kind,
                        ast::PatternKind::TupleStruct { .. }
                            | ast::PatternKind::Struct { .. }
                            | ast::PatternKind::Name(_)
                    ) {
                        return Err(self.error(beg..self.curr_pos(), "Expected a struct pattern"));
                    }
                    return Ok(mk_pattern(ast::PatternKind::TypeSpecifier(
                        name,
                        Box::new(case),
                    )));
                }
                if name == "_" {
                    return Ok(mk_pattern(ast::PatternKind::Wildcard));
                }
                Ok(mk_pattern(ast::PatternKind::Name(name)))
            }
            TokenType::Star => {
                self.lexer.advance();
                let name = self.parse_identifier()?;
                Ok(mk_pattern(ast::PatternKind::Rest(name)))
            }
            t => panic!("Invalid pattern {:?}", t),
        }
    }

    fn parse_pattern_field(&mut self) -> ParseRes<(String, ast::Pattern)> {
        let name = self.parse_identifier()?;
        let _ = self.expect_token(TokenType::Colon);
        let pattern = self.parse_pattern()?;
        Ok((name, pattern))
    }

    fn parse_while(&mut self) -> ParseRes<ast::Expr> {
        self.match_token(TokenType::While)?;
        let _ = self.expect_token(TokenType::LParen);
        let cond = self.parse_expr()?;
        let _ = self.expect_token(TokenType::RParen);
        let body = self.parse_expr_sequence()?;
        Ok(mk_expr(ast::ExprKind::While {
            cond: Box::new(cond),
            body: Box::new(body),
        }))
    }

    fn parse_return(&mut self) -> ParseRes<ast::Expr> {
        self.match_token(TokenType::Return)?;
        let expr = self.parse_expr()?;
        Ok(mk_expr(ast::ExprKind::Return(Box::new(expr))))
    }

    fn parse_if(&mut self) -> ParseRes<ast::Expr> {
        let beg = self.curr_pos();
        self.match_token(TokenType::If)?;
        let _ = self.expect_token(TokenType::LParen);
        let cond = self.parse_expr()?;
        let _ = self.expect_token(TokenType::RParen);
        let body = self.parse_expr_sequence()?;
        let else_body = self
            .match_token(TokenType::Else)
            .ok()
            .map(|_| match self.lexer.current().typ.clone() {
                TokenType::If => self.parse_if(),
                TokenType::LBrace => self.parse_expr_sequence(),
                _ => Err(self.error(
                    beg..self.curr_pos(),
                    "else has to be followed by either the if or a block",
                )),
            })
            .transpose()?
            .map(Box::new);
        Ok(mk_expr(ast::ExprKind::If {
            cond: Box::new(cond),
            body: Box::new(body),
            else_body,
        }))
    }
}

fn wrong_rule<T>() -> ParseRes<T> {
    Err(ParseErr::WrongRule)
}

impl<'source> Parser<'source> {
    pub fn new(lexer: Lexer<'source>) -> Self {
        Self {
            lexer,
            errors: Vec::new(),
        }
    }

    fn in_scope<T>(&mut self, f: impl FnOnce(&mut Parser) -> ParseRes<T>) -> ParseRes<T> {
        let mut new = Parser {
            lexer: self.lexer.clone(),
            errors: Vec::new(),
        };
        let res = f(&mut new)?;
        self.errors.extend(new.errors);
        self.lexer = new.lexer;
        Ok(res)
    }

    fn eof(&self) -> bool {
        self.lexer.current().typ == TokenType::Eof
    }

    #[inline(always)]
    fn curr_pos(&self) -> usize {
        self.lexer.current().span.start
    }

    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors[..]
    }

    fn error(&mut self, span: Range<usize>, msg: impl Into<String>) -> ParseErr {
        let err = SyntaxError {
            span,
            msg: msg.into(),
        };
        self.errors.push(err.clone());
        ParseErr::Err(err)
    }

    fn expected_err_msg(&self, item: &str) -> String {
        format!(
            "Expected {} got {:?}",
            item,
            std::mem::discriminant(&self.lexer.current().typ)
        )
    }

    fn expect_token(&mut self, typ: TokenType) -> ParseRes<Token> {
        if self.lexer.current().typ == typ {
            Ok(self.lexer.advance())
        } else {
            let err = SyntaxError {
                span: self.lexer.current().span.clone(),
                msg: format!("Expected token {:?} got {:?}", typ, self.lexer.current()),
            };
            self.errors.push(err.clone());
            Err(ParseErr::Err(err))
        }
    }

    fn match_token(&mut self, typ: TokenType) -> ParseRes<Token> {
        if self.lexer.current().typ == typ {
            Ok(self.lexer.advance())
        } else {
            wrong_rule()
        }
    }
}

fn mk_pattern(kind: ast::PatternKind) -> ast::Pattern {
    ast::Pattern {
        kind,
        id: DUMMY_AST_ID,
    }
}

fn mk_expr(kind: ast::ExprKind) -> ast::Expr {
    ast::Expr {
        kind,
        id: DUMMY_AST_ID,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use classy_sexpr::ToSExpr;
    use classy_sexpr_proc_macro::sexpr;

    type TestRes = Result<(), Vec<SyntaxError>>;

    macro_rules! ptest {
        ($name:ident, $source:literal, $expected:expr) => {
            #[test]
            fn $name() -> TestRes {
                let source = $source;
                let lex = Lexer::new(source);
                let mut parser = Parser::new(lex);
                let actual = parser.parse()?.to_sexpr();
                similar_asserts::assert_eq!(expected: $expected, actual: actual);
                Ok(())
            }
        };
    }

    ptest! {
        test_empty_struct_definition,
        r#"
            type  Foo {}
            type  Bar {
            }
        "#,
        sexpr!{(
            (type Foo [] (record))
            (type Bar [] (record))
         )}
    }

    ptest! {
        test_struct_definition_with_simple_fields,
        r#"
            type Foo { x: typ1; y: typ2 }
            type Bar {
                foo: typ_1
            }
        "#,
        sexpr!{(
            (type Foo []
                (record
                    (x (poly [] typ1))
                    (y (poly [] typ2))))
            (type Bar []
                (record
                    (foo (poly [] typ_1))))
        )}
    }

    ptest! {
        test_struct_definition_with_explicit_not_type_vars,
        "type Foo () { x: a };",
        sexpr!{(
            (type Foo []
                (record
                    (x (poly [] a))))
        )}
    }

    ptest! {
        test_struct_definition_with_one_type_var,
        "type Foo (a) { x: a };",
        sexpr!{(
            (type Foo [a]
                (record
                    (x (poly [] a))))
        )}

    }

    ptest! {
        test_struct_definition_with_multiple_type_vars,
        "type Foo (a, b, c) { x: a };",
        sexpr!{(
            (type Foo [a b c]
                (record
                    (x (poly [] a))))
        )}
    }

    ptest! {
        test_simple_adt_definition_with_single_discriminant,
        "type A { B };",
        sexpr!((
            (type A []
                (adt
                    (B unit))
            )
        ))
    }

    ptest! {
        test_simple_adt_with_multiple_no_argument_discriminants,
        "type A { A; B; C };",
        sexpr!((
            (type A []
                (adt
                    (A unit)
                    (B unit)
                    (C unit))
            )
        ))
    }

    ptest! {
        test_adt_with_mixed_discriminants_with_arguments_and_no_arguments,
        "type A { A(T1, T2); B; C(T3); D(T4, T5, T6); E };",
        sexpr!((
            (type A []
                (adt
                    (A (tuple
                        (poly [] T1)
                        (poly [] T2)))
                    (B unit)
                    (C (tuple
                        (poly [] T3)))
                    (D (tuple
                        (poly [] T4)
                        (poly [] T5)
                        (poly [] T6)))
                    (E unit))
            )
        ))
    }

    ptest! {
        test_adt_with_type_vars,
        "type Res(a, b) { Ok(a); Err(b) };",
        sexpr!((
            (type Res [a b]
                (adt
                    (Ok (tuple
                        (poly [] a)))
                    (Err (tuple
                        (poly [] b))))
            )
        ))
    }

    ptest! {
        test_function_definition_with_no_argumentsr,
        "foo: () -> ()
         foo = 10;",
         sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                foo ()
                    10)
         ))
    }

    ptest! {
        test_function_definition_with_arguments,

        r#"foo: (b, d) -> ()
           foo(a, c) = 10;"#,
        sexpr!((
            (fn {}
                (type
                    (fn [] (
                        (poly [] b)
                        (poly [] d))
                            (poly [] unit)))
                foo (a c)
                    10)
        ))
    }

    ptest! {
        test_simple_function_call_expression,
        r#"a: () -> ()
           a() = a(10, 20, 30)
        "#,
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    call a (10 20 30) {}
                })
        ))
    }

    ptest! {
        test_function_returning_a_tuple,
        r#"a: () -> (); a() = (1, 2, (a.b));"#,
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    tuple 1 2 (access a b)
                })
        ))
    }

    ptest! {
        test_parsing_unit_value,
        "a:()->();a=();",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {})
        ))
    }

    ptest! {
        test_chained_access,
        "a:()->();a=a.b.c.d;",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    access (
                        access (
                            access a b)
                        c)
                    d
                })
        ))
    }

    ptest! {
        test_parsing_block,
        "a:()->();a={1; 2};",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    1
                    2
                })
        ))
    }

    ptest! {
        test_function_with_block_body,
        "a:()->();a{1; 2};",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    1
                    2
                })
        ))
    }

    ptest! {
        test_simple_assignment,
        "a:()->();a=a.b=c.d;",
        sexpr!{(
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    assign
                        (access a b)
                        (access c d)
                })
        )}
    }

    ptest! {
        test_func_no_arguments_trailing_lambda,
        "a:()->();a=a{1};",
        sexpr!{(
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    call a ((
                        lambda () { 1 }
                    )) {}
                })
        )}
    }

    ptest! {
        function_call_with_single_non_parenthised_argument,
        "a:()->();a=a a.b;",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    call a ((
                        access a b
                    )) {}
                })
        ))
    }

    ptest! {
        test_func_call_trailing_lambda_with_single_unparenthised_argument,
        "a:()->();a=a.b c => 1;",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    method a b (
                        (lambda ((c infere)) 1)
                    ) {}
                })
        ))
    }

    ptest! {
        test_func_call_trailing_lambda_explicit_no_arguments,
        "a:()->();a=a.b () => 1;",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    method a b (
                        (lambda [] 1)
                    ) {}
                })
        ))
    }

    ptest! {
        test_function_call_with_trailing_with_explicit_parameters,
        "a:()->();a=a(a, b, c) => 1;",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    call a (
                        (lambda (
                            [a infere]
                            [b infere]
                            [c infere])
                            1)) {}
                })
        ))
    }

    ptest! {
        test_function_call_with_arguments_and_parameterless_trailing_lambda,
        "a:()->();a=a(a, b, c) { 1 };",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    call a (
                        a b c (lambda () { 1 })) {}
                })
        ))
    }

    ptest! {
        test_function_call_with_args_trailing_with_explicit_args,
        "a:()->();a=a(a, b, c) (d, e) => { 1 };",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    call a (
                        a b c
                        (lambda ([d infere] [e infere])
                            { 1 })) {}
                })
        ))
    }

    ptest! {
        test_trailing_lambda_with_types,
        "a:()->();a=a(b:c,d:e)=>1;",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    call a (
                        (lambda (
                            [b (poly [] c)]
                            [d (poly [] e)])
                                1)) {}
                })
        ))
    }

    ptest! {
        test_simple_typed_expression,
        "a:()->();a=a.b c : d;",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    typed
                        (method a b (c) {})
                        (poly [] d)
                })
        ))
    }

    ptest! {
        test_single_arg_lambda_expr,
        "a:()->();a=a=>b=>1;",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    lambda ([a infere])
                        (lambda ([b infere])
                            1)
                })
        ))
    }

    ptest! {
        test_lambda_expression_with_no_type_arg_list,
        "a:()->();a=(a, b)=>1;",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    lambda ([a infere] [b infere])
                        1
                })
        ))
    }

    ptest! {
        test_lambda_expression_with_typed_arg_list,
        "a:()->();a=(a: b, c: d)=>1;",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    lambda (
                        [a (poly [] b)]
                        [c (poly [] d)])
                            1
                })
        ))
    }

    ptest! {
        function_call_with_single_non_parenthised_argument_and_non_parenthised_lambda,
        "a:()->();a=a b c => 1;",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    call a (b (lambda ([c infere]) 1)) {}
                })
        ))
    }

    ptest! {
        trailing_lambda_call_in_while_cond,
        "a:()->();a=while(a { b }) { c };",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    while
                        (call a ((lambda () { b })) {})
                        ( c )
                })
        ))
    }

    ptest! {
        if_with_instruction_following,
        "a:()->();a { if(b) { c }; d }",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    (if b { c })
                    d
                })
        ))
    }

    ptest! {
        simple_if_else_chain,
        "a:()->();a=if(b){c}else if (d){e} else {f}",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    if b { c } (if d { e } { f })
                })
        ))
    }

    ptest! {
        simple_return_statement,
        "a:()->();a=return a b",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    return (call a (b) {})
                })
        ))
    }

    ptest! {
        simple_let_expression,
        "a:()->();a=let var = { a }",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    let var infere { a }
                })
        ))
    }

    ptest! {
        keyword_arguments,
        "a:()->();a=a(b=c, d=e, a)",
        sexpr!((
            (fn {}
                (type (fn [] () (poly [] unit)))
                a () {
                    call a (a) {
                        ["b" c]
                        ["d" e]
                     }
                })
        ))
    }

    ptest! {
        function_definition_without_header_infers_type,
        "foo (a, b, c) = 10",
        sexpr!((
            (fn {}
                (type (fn [] (infere infere infere) infere))
                foo (a b c) 10)
        ))
    }

    ptest! {
        empty_function_definition_without_header,
        "foo { 10 }",
        sexpr!((
            (fn {}
                (type (fn [] () infere))
                foo () {
                    10
            })
        ))
    }
}
