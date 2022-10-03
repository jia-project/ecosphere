use std::mem::size_of;

use crate::{mark_product, Expr, FnSpec, Loader, Stmt, TypeMeta};

use super::op::ExprOp;

#[derive(Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Special(&'a str),
    Name(&'a str),
    LitInt(i32),
}

pub struct Context<'a> {
    input: &'a str,
    token: Option<Token<'a>>,
    loader: &'a mut Loader<Expr<ExprOp>>,
}

impl<'a> Context<'a> {
    pub fn new(input: &'a str, loader: &'a mut Loader<Expr<ExprOp>>) -> Self {
        Self {
            input: input.trim_start(),
            token: None,
            loader,
        }
    }

    fn parse_token(&mut self) -> Option<Token<'a>> {
        if self.input.is_empty() {
            return None;
        }
        let specials = [
            "fn", "if", "then", "else", "while", "break", "continue", "return", "struct", "enum",
            "end", "and", "or", "(", ")", ",", ";", "not", "+", "-", "*", "/", "true", "false",
            "nil",
        ];
        for special in specials {
            if let Some(input) = self.input.strip_prefix(special) {
                self.input = input.trim_start();
                return Some(Token::Special(special));
            }
        }
        if self.input.starts_with(char::is_numeric) {
            let n = self
                .input
                .chars()
                .take_while(|&c| c.is_numeric())
                .collect::<String>();
            self.input = self.input.strip_prefix(&n).unwrap().trim_start();
            return Some(Token::LitInt(n.parse().unwrap()));
        }
        let input = self.input.trim_start_matches(char::is_alphanumeric);
        let name = &self.input[..self.input.len() - input.len()];
        assert!(!name.is_empty());
        self.input = input.trim_start();
        Some(Token::Name(name))
    }

    fn token(&mut self) -> &Token<'a> {
        if self.token.is_none() {
            self.token = self.parse_token();
        }
        self.token.as_ref().unwrap()
    }

    fn shift(&mut self) -> Token<'a> {
        self.token();
        self.token.take().unwrap()
    }

    pub fn parse(&mut self) {
        while !self.input.is_empty() {
            match self.token() {
                Token::Special("struct") => self.parse_struct(),
                Token::Special("fn") => self.parse_fn(),
                _ => panic!(),
            }
        }
    }

    fn parse_struct(&mut self) {
        assert_eq!(self.shift(), Token::Special("struct"));
        let name = if let Token::Name(name) = self.shift() {
            name
        } else {
            panic!()
        };
        let mut elements = Vec::new();
        loop {
            match self.shift() {
                Token::Name(name) => elements.push(name),
                Token::Special("end") => break,
                _ => panic!(),
            }
        }
        let ty = TypeMeta {
            repr: String::from(name), //
            size: elements.len() * size_of::<usize>(),
            mark_fn: mark_product,
        };
        let ty = self.loader.register_type(ty);
        self.loader.register_op(
            String::from(name),
            vec![],
            elements.len(),
            ExprOp::ProductNew(ty),
        );
        for (i, element) in elements.into_iter().enumerate() {
            self.loader
                .register_op(String::from(element), vec![ty], 0, ExprOp::ProductGet(i));
            self.loader
                .register_op(String::from(element), vec![ty], 1, ExprOp::ProductSet(i))
        }
    }

    fn parse_fn(&mut self) {
        assert_eq!(self.shift(), Token::Special("fn"));
        let name = if let Token::Name(name) = self.shift() {
            name
        } else {
            panic!()
        };
        assert_eq!(self.shift(), Token::Special("("));
        let mut param_names = Vec::new();
        let mut param_types = Vec::new();
        let mut untyped_len = 0;
        while self.token() != &Token::Special(")") {
            if let Token::Name(name) = self.shift() {
                param_names.push(String::from(name));
            }
            if let &Token::Name(ty) = self.token() {
                param_types.push(String::from(ty));
                self.shift();
            } else {
                untyped_len += 1;
            }
            if self.token() != &Token::Special(")") {
                assert_eq!(self.shift(), Token::Special(","));
            }
        }
        self.shift();
        let expr = self.parse_scope(false);
        self.loader.register_fn(FnSpec {
            name: String::from(name),
            param_types,
            untyped_len,
            param_names,
            expr,
        });
    }

    fn parse_scope(&mut self, is_then: bool) -> Expr<ExprOp> {
        let mut scope = Vec::new();
        while !(self.token() == &Token::Special("end")
            || is_then && self.token() == &Token::Special("else"))
        {
            scope.push(self.parse_stmt());
        }
        if self.token() != &Token::Special("else") {
            self.shift();
        }
        Expr::Scope(scope)
    }

    fn parse_stmt(&mut self) -> Stmt<ExprOp> {
        match self.token() {
            Token::Special("return") => {
                self.shift();
                let stmt = Stmt::Return(self.parse_expr());
                assert_eq!(self.shift(), Token::Special(";"));
                stmt
            }
            Token::Special("break") => {
                self.shift();
                assert_eq!(self.shift(), Token::Special(";"));
                Stmt::Break
            }
            Token::Special("continue") => {
                self.shift();
                assert_eq!(self.shift(), Token::Special(";"));
                Stmt::Continue
            }
            Token::Special("if") => self.parse_if(),
            _ => {
                let stmt = Stmt::Expr(self.parse_expr());
                assert_eq!(self.shift(), Token::Special(";"));
                stmt
            }
        }
    }

    fn parse_if(&mut self) -> Stmt<ExprOp> {
        assert_eq!(self.shift(), Token::Special("if"));
        let expr = self.parse_expr();
        assert_eq!(self.shift(), Token::Special("then"));
        let then_scope = self.parse_scope(true);
        let mut else_scope = Expr::Scope(Vec::new());
        if self.token() == &Token::Special("else") {
            self.shift();
            else_scope = self.parse_scope(false);
        }
        Stmt::IfElse(expr, then_scope, else_scope)
    }

    fn parse_expr(&mut self) -> Expr<ExprOp> {
        self.parse_infix0()
    }

    fn parse_single_expr(&mut self) -> Expr<ExprOp> {
        match self.shift() {
            Token::LitInt(lit) => Expr::Op(ExprOp::IntNew(lit), vec![]),
            Token::Special("(") => {
                let expr = self.parse_expr();
                assert_eq!(self.shift(), Token::Special(")"));
                expr
            }
            Token::Special("nil") => Expr::Op(ExprOp::NilNew, vec![]),
            Token::Name(name) => {
                if self.token() != &Token::Special("(") {
                    return Expr::Name(String::from(name));
                }
                self.shift();
                let mut args = Vec::new();
                while self.token() != &Token::Special(")") {
                    args.push(self.parse_expr());
                    if self.token() != &Token::Special(")") {
                        assert_eq!(self.shift(), Token::Special(","));
                    }
                }
                self.shift();
                Expr::Call(String::from(name), args)
            }
            token => todo!("{token:?}"),
        }
    }

    fn parse_infix(
        &mut self,
        ops: &[(&str, &str)],
        nested: impl Fn(&mut Self) -> Expr<ExprOp>,
    ) -> Expr<ExprOp> {
        let mut expr = nested(self);
        while let Some(&(_, op)) = ops
            .iter()
            .find(|(op, _)| self.token() == &Token::Special(op))
        {
            self.shift();
            let latter_expr = self.parse_single_expr();
            expr = Expr::Call(String::from(op), vec![expr, latter_expr]);
        }
        expr
    }

    fn parse_infix0(&mut self) -> Expr<ExprOp> {
        self.parse_infix(&[("+", "add"), ("-", "sub")], Self::parse_infix1)
    }

    fn parse_infix1(&mut self) -> Expr<ExprOp> {
        self.parse_infix(&[("*", "mul"), ("/", "div")], Self::parse_prefix)
    }

    fn parse_prefix(&mut self) -> Expr<ExprOp> {
        match self.token() {
            Token::Special("not") => {
                self.shift();
                Expr::Op(ExprOp::BoolNot, vec![self.parse_prefix()])
            }
            _ => self.parse_single_expr(),
        }
    }
}