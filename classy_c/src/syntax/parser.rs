use std::ops::Range;

use thiserror::Error;

use super::ast::{self, TypedName};
use super::lexer::Lexer;
use super::tokens::{Token, TokenType};

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
            } else if let Ok(fun_def) = self.parse_function_definition() {
                items.push(ast::TopLevelItem::FunctionDefinition(fun_def));
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

    fn parse_type_definition(&mut self) -> ParseRes<ast::TypeDefinition> {
        let beg = self.curr_pos();
        self.match_token(TokenType::Type)?;
        let name =
            self.parse_identifier()
                .error(self, beg, "Expected a name after a struct keyword")?;
        let type_variables = self.parse_optional_type_variables().error(
            self,
            beg,
            "Expected type variables list",
        )?;
        self.expect_token(TokenType::LBrace)?;
        let possibly_adt = self.in_scope(|p| p.parse_variants_definition());
        let definition = if let Ok(adt) = possibly_adt {
            adt
        } else {
            let fields = self.parse_delimited(Self::parse_typed_identifier, TokenType::Semicolon);
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
    }

    fn parse_variants_definition(&mut self) -> ParseRes<ast::DefinedType> {
        fn parse_discriminant(parser: &mut Parser) -> ParseRes<ast::Discriminant> {
            let constructor = parser.parse_identifier()?;
            let mut arguments = Vec::new();
            // possibly a record definition
            if parser.match_token(TokenType::Colon).is_ok() {
                return wrong_rule();
            }
            if parser.match_token(TokenType::LParen).is_ok() {
                arguments = parser.parse_delimited(Parser::parse_type, TokenType::Comma);
                let _ = parser.expect_token(TokenType::RParen)?;
            }
            Ok(ast::Discriminant {
                constructor,
                arguments,
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

    fn parse_function_definition(&mut self) -> ParseRes<ast::FunctionDefinition> {
        let beg = self.curr_pos();
        let name = self.parse_identifier()?;
        let _ = self.expect_token(TokenType::Colon);
        let typ = self.parse_fn_type()?;
        let _ = self.expect_token(TokenType::Semicolon);
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
        let _ = self.expect_token(TokenType::Semicolon);
        Ok(ast::FunctionDefinition {
            name,
            typ,
            parameters,
            body,
        })
    }

    fn parse_fn_type(&mut self) -> ParseRes<ast::Typ> {
        let beg = self.curr_pos();
        match self.parse_type()? {
            f_t @ ast::Typ::Function { .. } => Ok(f_t),
            t => Err(self.error(
                beg..self.curr_pos(),
                format!("The function's type is not a function type: {t:#?}"),
            )),
        }
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
            .parse_identifier()
            .error(self, beg, &self.expected_err_msg("a type"))?;
        Ok(ast::TypedName {
            name,
            typ: ast::Typ::Name(typ),
        })
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
        let beg = self.curr_pos();
        match self.lexer.current().typ.clone() {
            TokenType::Identifier(name) => {
                self.lexer.advance();
                Ok(ast::Typ::Name(name))
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
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> ParseRes<ast::Expr> {
        let lhs = self.parse_fn_call()?;
        if let Err(_) = self.match_token(TokenType::Assignment) {
            return Ok(lhs);
        }
        let rhs = self.parse_fn_call()?;
        Ok(ast::Expr::Assignment {
            lval: Box::new(lhs),
            rval: Box::new(rhs),
        })
    }

    fn parse_fn_call(&mut self) -> ParseRes<ast::Expr> {
        let beg = self.curr_pos();
        let func = self.parse_access()?;
        let args = if let Ok(_) = self.match_token(TokenType::LParen) {
            // this is a function call with arguments passed in parentheses
            // or a function call in a special form:
            // func (a, b) => { lambda body }
            let mut args = self.parse_delimited(Self::parse_expr, TokenType::Comma);
            let _ =
                self.match_token(TokenType::RParen)
                    .error(self, beg, "Missing closing parenthesis");

            if let Ok(_) = self.match_token(TokenType::FatArrow) {
                // this means that we probably have a call in form
                // func (a, b) => { a + b }
                // which is a function call with trailing lambda with arguments
                let parameters = args
                    .into_iter()
                    .map(|arg| match arg {
                        ast::Expr::Name(v) => Ok(v),
                        _ => Err(ParseErr::Err(SyntaxError {
                            msg: "Expected a lambda's arguments list".into(),
                            span: beg..self.curr_pos(),
                        })),
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let body =
                    self.parse_expr()
                        .error(self, beg, "Expected a trailing lambda's body")?;
                let lambda = ast::Expr::Lambda {
                    parameters: parameters
                        .into_iter()
                        .map(|name| ast::TypedName {
                            name,
                            typ: ast::Typ::ToInfere,
                        })
                        .collect(),
                    body: Box::new(body),
                };
                vec![lambda]
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
                        let lambda = ast::Expr::Lambda {
                            parameters: vec![ast::TypedName {
                                name,
                                typ: ast::Typ::ToInfere,
                            }],
                            body: Box::new(body),
                        };
                        args.push(lambda);
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
                        let lambda = ast::Expr::Lambda {
                            parameters,
                            body: Box::new(body),
                        };
                        args.push(lambda);
                        args
                    }
                    TokenType::LBrace => {
                        // form of func(args) { lambda body }
                        let body = self.parse_expr_sequence().error(
                            self,
                            beg,
                            "expected trailing lambda's body",
                        )?;
                        let lambda = ast::Expr::Lambda {
                            parameters: Vec::new(),
                            body: Box::new(body),
                        };
                        args.push(lambda);
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
            match self.parse_expr() {
                // no name or brace following, just a normal expression
                Err(ParseErr::WrongRule) => return Ok(func),
                e @ Err(_) => return e,
                Ok(ast::Expr::Name(name)) => {
                    if let Ok(_) = self.match_token(TokenType::FatArrow) {
                        // func name => { lambda body }
                        let body = self.parse_expr().error(
                            self,
                            beg,
                            "Expected trailing lambda's body",
                        )?;
                        let lambda = ast::Expr::Lambda {
                            parameters: vec![ast::TypedName {
                                name,
                                typ: ast::Typ::ToInfere,
                            }],
                            body: Box::new(body),
                        };
                        vec![lambda]
                    } else {
                        // func name
                        vec![ast::Expr::Name(name)]
                    }
                }
                Ok(body @ ast::Expr::Sequence(_)) => {
                    // func { lambda body }
                    let lambda = ast::Expr::Lambda {
                        parameters: Vec::new(),
                        body: Box::new(body),
                    };
                    vec![lambda]
                }
                Ok(val) => {
                    // func expr
                    vec![val]
                }
            }
        };
        Ok(ast::Expr::FunctionCall {
            func: Box::new(func),
            args,
        })
    }

    fn parse_access(&mut self) -> ParseRes<ast::Expr> {
        let beg = self.curr_pos();
        let mut lhs = self.parse_term()?;
        while let Ok(_) = self.match_token(TokenType::Dot) {
            let field =
                self.parse_identifier()
                    .error(self, beg, "A field has to be an indentifier")?;
            lhs = ast::Expr::Access {
                val: Box::new(lhs),
                field,
            };
        }
        Ok(lhs)
    }

    fn parse_term(&mut self) -> ParseRes<ast::Expr> {
        let beg = self.curr_pos();
        let tok = self.lexer.current().typ.clone();
        match tok {
            TokenType::Integer(val) => {
                self.lexer.advance();
                Ok(ast::Expr::IntConst(val))
            }
            TokenType::Float(val) => {
                self.lexer.advance();
                Ok(ast::Expr::FloatConst(val))
            }
            TokenType::Identifier(name) => {
                self.lexer.advance();
                Ok(ast::Expr::Name(name))
            }
            TokenType::LParen => {
                self.lexer.advance();
                if let Ok(_) = self.match_token(TokenType::RParen) {
                    return Ok(ast::Expr::Unit);
                }
                let inner = self.parse_expr()?;
                if let Err(_) = self.match_token(TokenType::Comma) {
                    let _ = self.match_token(TokenType::RParen).error(
                        self,
                        beg,
                        "Missing closing parenthesis",
                    );
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
                Ok(ast::Expr::Tuple(tuple))
            }
            TokenType::LBrace => self.parse_expr_sequence(),
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
        Ok(ast::Expr::Sequence(body))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::ast;

    type TestRes = Result<(), Vec<SyntaxError>>;

    macro_rules! ptest {
        ($name:ident, $source:literal, $expected:expr) => {
            #[test]
            fn $name() -> TestRes {
                let source = $source;
                let lex = Lexer::new(source);
                let mut parser = Parser::new(lex);
                let actual = parser.parse()?;
                let expected = $expected.build();
                assert_eq!(expected, actual);
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
        ast::Builder::new()
            .empty_struct("Foo")
            .empty_struct("Bar")
    }

    ptest! {
        test_struct_definition_with_simple_fields,
        r#"
            type Foo { x: typ1; y: typ2 }
            type Bar {
                foo: typ_1
            }
        "#,
        ast::Builder::new()
            .struct_def("Foo", |s| s.field("x", "typ1").field("y", "typ2"))
            .struct_def("Bar", |s| s.field("foo", "typ_1"))
    }

    ptest! {
        test_struct_definition_with_explicit_not_type_vars,
        "type Foo () { x: a };",
        ast::Builder::new()
            .struct_def("Foo", |s| s.field("x", "a"))
    }

    ptest! {
        test_struct_definition_with_one_type_var,
        "type Foo (a) { x: a };",
        ast::Builder::new()
            .struct_def("Foo", |s| s.type_var("a")
                                    .field("x", "a"))
    }

    ptest! {
        test_struct_definition_with_multiple_type_vars,
        "type Foo (a, b, c) { x: a };",
        ast::Builder::new()
            .struct_def(
                "Foo", |s| s.type_var("a")
                            .type_var("b")
                            .type_var("c")
                            .field("x", "a"))
    }

    ptest! {
        test_simple_adt_definition_with_single_discriminant,
        "type A { B };",
        ast::Builder::new()
            .adt_def("A", |adt| adt.empty_discriminant("B"))
    }

    ptest! {
        test_simple_adt_with_multiple_no_argument_discriminants,
        "type A { A; B; C };",
        ast::Builder::new()
            .adt_def("A", |adt| adt.empty_discriminant("A")
                                   .empty_discriminant("B")
                                   .empty_discriminant("C"))
    }

    ptest! {
        test_adt_with_mixed_discriminants_with_arguments_and_no_arguments,
        "type A { A(T1, T2); B; C(T3); D(T4, T5, T6); E };",
        ast::Builder::new()
            .adt_def("A", |adt| adt.discriminant("A", &["T1", "T2"])
                                   .empty_discriminant("B")
                                   .discriminant("C", &["T3"])
                                   .discriminant("D", &["T4", "T5", "T6"])
                                   .empty_discriminant("E"))
    }

    ptest! {
        test_adt_with_type_vars,
        "type Res(a, b) { Ok(a); Err(b) };",
        ast::Builder::new()
            .adt_def(
                "Res",
                |adt| adt.type_var("a")
                         .type_var("b")
                         .discriminant("Ok", &["a"])
                         .discriminant("Err", &["b"]))
    }

    ptest! {
        test_function_definition_with_no_argumentsr,
        "foo: () -> ()
         foo = 10;",
        ast::Builder::new()
            .unit_fn("foo", |body| body.integer(10))
    }

    ptest! {
        test_function_definition_with_arguments,

        r#"foo: (b, d) -> ()
           foo(a, c) = 10;"#,
        ast::Builder::new()
            .func_def(
                "foo",
                |a| a.name("a", "b").name("c", "d"),
                ast::Typ::Unit,
                |b| b.integer(10),
            )
    }

    ptest! {
        test_simple_function_call_expression,
        r#"a: () -> ()
           a() = a(10, 20, 30)
        "#,
        ast::Builder::new()
            .unit_fn("a", |body| {
                body.function_call(
                    |f| f.name("a"),
                    |args| args
                        .add(|arg| arg.integer(10))
                        .add(|arg| arg.integer(20))
                        .add(|arg| arg.integer(30)))
            })
    }

    ptest! {
        test_function_returning_a_tuple,
        r#"a: () -> (); a() = (1, 2, (a.b));"#,
        ast::Builder::new()
            .unit_fn("a", |body| {
                body.tuple(|vals|
                    vals.add(|v| v.integer(1))
                        .add(|v| v.integer(2))
                        .add(|v| v.access(|lhs| lhs.name("a"), "b"))
                )
            })
    }

    ptest! {
        test_parsing_unit_value,
        "a:()->();a=();",
        ast::Builder::new()
            .unit_fn("a", |body| body.unit())
    }

    ptest! {
        test_chained_access,
        "a:()->();a=a.b.c.d;",
        ast::Builder::new()
            .unit_fn("a", |body| {
                body
                    .access(|lhs| lhs
                        .access(|lhs| lhs
                            .access(|lhs| lhs.name("a"), "b"),
                        "c"),
                    "d")
            })
    }

    ptest! {
        test_parsing_block,
        "a:()->();a={1; 2};",
        ast::Builder::new()
            .unit_fn("a", |body| {
                body.sequence(|es| es
                    .add(|e| e.integer(1))
                    .add(|e| e.integer(2)))
            })
    }

    ptest! {
        test_function_with_block_body,
        "a:()->();a{1; 2};",
        ast::Builder::new()
            .unit_fn("a", |body| {
                body.sequence(|es| es
                    .add(|e| e.integer(1))
                    .add(|e| e.integer(2)))
            })
    }

    ptest! {
        test_simple_assignment,
        "a:()->();a=a.b=c.d;",
        ast::Builder::new()
            .unit_fn("a", |body| {
                body.assignment(
                    |l| l.access(|s| s.name("a"), "b"),
                    |r| r.access(|s| s.name("c"), "d"))
            })
    }

    ptest! {
        test_func_no_arguments_trailing_lambda,
        "a:()->();a=a{1};",
        ast::Builder::new()
            .unit_fn(
                "a",
                |body| body.function_call(
                    |c| c.name("a"),
                    |args| args.add(|arg| {
                        arg.lambda::<&str>(
                            &[],
                            |body| body.sequence(|seq|
                                seq.add(|expr| expr.integer(1))))
                    })))
    }

    ptest! {
        function_call_with_single_non_parenthised_argument,
        "a:()->();a=a a.b;",
        ast::Builder::new()
            .unit_fn("a", |body| body.function_call(
                |c| c.name("a"),
                |args| args.add(
                    |a| a.access(
                        |lhs| lhs.name("a"),
                        "b")))
        )
    }

    ptest! {
        test_func_call_trailing_lambda_with_single_unparenthised_argument,
        "a:()->();a=a.b c => 1;",
        ast::Builder::new()
            .unit_fn("a", |body| body.function_call(
                |c| c.access(|l| l.name("a"), "b"),
                |args| args
                    .add(|l| l.lambda(
                        &["c"],
                        |body| body.integer(1)))))
    }

    ptest! {
        test_func_call_trailing_lambda_explicit_no_arguments,
        "a:()->();a=a.b () => 1;",
        ast::Builder::new()
            .unit_fn("a", |body| body.function_call(
                |c| c.access(|l| l.name("a"), "b"),
                |args| args
                    .add(|l| l.lambda::<&str>(
                        &[],
                        |body| body.integer(1)))))
    }

    ptest! {
        test_function_call_with_trailing_with_explicit_parameters,
        "a:()->();a=a(a, b, c) => 1;",
        ast::Builder::new()
            .unit_fn("a", |body| body.function_call(
                |c| c.name("a"),
                |args| args
                    .add(|l| l.lambda(
                        &["a", "b", "c"],
                        |body| body.integer(1)))))
    }

    ptest! {
        test_function_call_with_arguments_and_parameterless_trailing_lambda,
        "a:()->();a=a(a, b, c) { 1 };",
        ast::Builder::new()
            .unit_fn("a", |body| body.function_call(
                |c| c.name("a"),
                |args| args
                    .add(|a| a.name("a"))
                    .add(|a| a.name("b"))
                    .add(|a| a.name("c"))
                    .add(|l| l.lambda::<&str>(
                        &[],
                        |body| body.sequence(
                            |s| s.add(
                                |e| e.integer(1)))))))
    }

    ptest! {
        test_function_call_with_args_trailing_with_explicit_args,
        "a:()->();a=a(a, b, c) (d, e) => { 1 };",
        ast::Builder::new()
            .unit_fn("a", |body| body.function_call(
                |c| c.name("a"),
                |args| args
                    .add(|a| a.name("a"))
                    .add(|a| a.name("b"))
                    .add(|a| a.name("c"))
                    .add(|l| l.lambda(
                        &["d", "e"],
                        |body| body.sequence(
                            |s| s.add(
                                |e| e.integer(1)))))))
    }
}
