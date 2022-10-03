use crate::{Expr, FnDef, Loader, Stmt};

use super::op::ExprOp;

#[derive(Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Special(&'a str),
    Name(&'a str),
    LitInt(i32),
    LitStr(String), // could be escaped
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
            "fn", "if", "then", "else", "while", "break", "continue", "return", "let", "mut",
            "struct", "enum", "end", "and", "or", "(", ")", ",", ";", "not", "+", "-", "*", "/",
            "%", "==", "!=", "<", "=", "true", "false", "nil",
        ];
        for special in specials {
            if let Some(input) = self.input.strip_prefix(special) {
                self.input = input.trim_start();
                return Some(Token::Special(special));
            }
        }
        if self.input.starts_with('"') {
            // TODO escape
            let n = self.input[1..].find('"').unwrap() + 1;
            let s = String::from(&self.input[1..n]);
            self.input = self.input[n + 1..].trim_start();
            return Some(Token::LitStr(s));
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
        let name_len = self
            .input
            .chars()
            .take_while(|&c| c.is_alphanumeric() || c == '_')
            .count();
        assert_ne!(name_len, 0, "name not valid: {:?}", &self.input[..32]);
        let name = &self.input[..name_len];
        self.input = self.input[name_len..].trim_start();
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
        let ty = self.loader.register_type(0, String::from(name));
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
        self.loader.register_fn(FnDef {
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
            Token::Special("while") => self.parse_while(),
            Token::Special("if") => self.parse_if(),
            Token::Special("let") => {
                self.shift();
                let name = if let Token::Name(name) = self.shift() {
                    name
                } else {
                    panic!()
                };
                assert_eq!(self.shift(), Token::Special(";"));
                Stmt::AllocName(String::from(name))
            }
            Token::Special("mut") => {
                self.shift();
                let name = if let Token::Name(name) = self.shift() {
                    name
                } else {
                    panic!()
                };
                assert_eq!(self.shift(), Token::Special("="));
                let expr = self.parse_expr();
                assert_eq!(self.shift(), Token::Special(";"));
                Stmt::Mutate(String::from(name), expr)
            }
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

    fn parse_while(&mut self) -> Stmt<ExprOp> {
        assert_eq!(self.shift(), Token::Special("while"));
        let expr = self.parse_expr();
        assert_eq!(self.shift(), Token::Special("then"));
        let scope = self.parse_scope(false);
        Stmt::While(expr, scope)
    }

    fn parse_expr(&mut self) -> Expr<ExprOp> {
        self.parse_infix0()
    }

    fn parse_single_expr(&mut self) -> Expr<ExprOp> {
        match self.shift() {
            Token::LitInt(lit) => Expr::Op(ExprOp::IntNew(lit), vec![]),
            Token::LitStr(lit) => Expr::Op(ExprOp::StrNew(lit), vec![]),
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
        compose: impl Fn(&str, Expr<ExprOp>, Expr<ExprOp>) -> Expr<ExprOp>,
    ) -> Expr<ExprOp> {
        let mut expr = nested(self);
        while let Some(&(_, name)) = ops
            .iter()
            .find(|(op, _)| self.token() == &Token::Special(op))
        {
            self.shift();
            let latter_expr = nested(self);
            expr = compose(name, expr, latter_expr);
        }
        expr
    }

    fn infix_call(name: &str, expr1: Expr<ExprOp>, expr2: Expr<ExprOp>) -> Expr<ExprOp> {
        Expr::Call(String::from(name), vec![expr1, expr2])
    }

    fn parse_infix0(&mut self) -> Expr<ExprOp> {
        self.parse_infix(
            &[("==", "eq"), ("!=", "ne"), ("<", "lt")],
            Self::parse_infix1,
            |name, expr1, expr2| {
                if name == "ne" {
                    Expr::Call(
                        String::from("not"),
                        vec![Expr::Call(String::from("eq"), vec![expr1, expr2])],
                    )
                } else {
                    Self::infix_call(name, expr1, expr2)
                }
            },
        )
    }

    fn parse_infix1(&mut self) -> Expr<ExprOp> {
        self.parse_infix(
            &[("+", "add"), ("-", "sub")],
            Self::parse_infix2,
            |name, expr1, expr2| {
                if name == "sub" {
                    Expr::Call(
                        String::from("add"),
                        vec![expr1, Expr::Call(String::from("neg"), vec![expr2])],
                    )
                } else {
                    Self::infix_call(name, expr1, expr2)
                }
            },
        )
    }

    fn parse_infix2(&mut self) -> Expr<ExprOp> {
        self.parse_infix(
            &[("*", "mul"), ("/", "div"), ("%", "rem")],
            Self::parse_prefix,
            Self::infix_call,
        )
    }

    fn parse_prefix(&mut self) -> Expr<ExprOp> {
        match self.token() {
            Token::Special("not") => {
                self.shift();
                Expr::Call(String::from("not"), vec![self.parse_prefix()])
            }
            _ => self.parse_single_expr(),
        }
    }
}
