use std::{
    collections::HashMap,
    mem::{replace, take},
};

use crate::{vanilla::op::Eval, FnDef, Instr, InstrBlocks, InstrId, Loader};

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
    loader: &'a mut Loader<ExprOp>,
    blocks: InstrBlocks<ExprOp>,
    block_id: usize,
    break_id: usize,
    continue_id: usize,
    scopes: Vec<HashMap<String, InstrId>>,
}

impl<'a> Context<'a> {
    pub fn new(input: &'a str, loader: &'a mut Loader<ExprOp>) -> Self {
        Self {
            input: input.trim_start(),
            token: None,
            loader,
            blocks: Default::default(),
            block_id: Default::default(),
            break_id: Default::default(),
            continue_id: Default::default(),
            scopes: Vec::new(),
        }
    }

    fn parse_token(&mut self) -> Option<Token<'a>> {
        if self.input.is_empty() {
            return None;
        }
        let specials = [
            "fn", "if", "then", "else", "while", "break", "continue", "return", "let", "mut",
            "struct", "enum", "end", "and", "or", "(", ")", ",", ";", "not", "+", "-", "*", "/",
            "%", "==", "!=", "<=", ">=", ">", "<", "=", "true", "false", "nil",
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

        self.blocks = vec![vec![]];
        // argument block: turn arguments into references
        self.block_id = self.push_block();
        let mut scope = HashMap::new();
        for (i, name) in param_names.iter().enumerate() {
            scope.insert(name.clone(), self.ref_new((0, i)));
        }
        let block = self.push_block();
        self.push_instr(Instr::goto(block));

        self.block_id = block;
        self.parse_scope(scope, false);
        take(&mut self.block_id);
        assert_eq!((self.break_id, self.continue_id), Default::default());
        assert!(self.scopes.is_empty());
        self.loader.register_fn(FnDef {
            name: String::from(name),
            param_types,
            untyped_len,
            param_names,
            blocks: take(&mut self.blocks),
        });
    }

    fn parse_scope(&mut self, names: HashMap<String, InstrId>, expect_else: bool) {
        self.scopes.push(names);
        while !(self.token() == &Token::Special("end")
            || expect_else && self.token() == &Token::Special("else"))
        {
            self.parse_stmt();
            while self.token() == &Token::Special(";") {
                self.shift();
            }
        }
        if self.token() != &Token::Special("else") {
            self.shift();
        }
        self.scopes.pop();
    }

    fn push_instr(&mut self, instr: Instr<ExprOp>) -> InstrId {
        assert_ne!(self.block_id, Default::default());
        let id = (self.block_id, self.blocks[self.block_id].len());
        self.blocks[self.block_id].push(instr);
        id
    }

    fn push_block(&mut self) -> usize {
        let id = self.blocks.len();
        self.blocks.push(Vec::new());
        id
    }

    fn parse_stmt(&mut self) {
        match self.token() {
            Token::Special("if") => self.parse_if(),
            Token::Special("let") => self.parse_let(),
            Token::Special("mut") => self.parse_mut(),
            Token::Special("while") => self.parse_while(),
            Token::Special("break") => {
                self.shift();
                assert_ne!(self.break_id, Default::default());
                self.push_instr(Instr::goto(self.break_id));
            }
            Token::Special("continue") => {
                self.shift();
                assert_ne!(self.continue_id, Default::default());
                self.push_instr(Instr::goto(self.continue_id));
            }
            Token::Special("return") => {
                self.shift();
                let expr_instr = self.parse_expr();
                self.push_instr(Instr::Return(expr_instr));
            }
            _ => {
                self.parse_expr();
            }
        }
    }

    fn parse_while(&mut self) {
        assert_eq!(self.shift(), Token::Special("while"));
        let continue_block = self.push_block();
        let then_block = self.push_block();
        let break_block = self.push_block();
        self.push_instr(Instr::goto(continue_block));

        self.block_id = continue_block;
        let expr_instr = self.parse_expr();
        self.push_instr(Instr::Branch(expr_instr, then_block, break_block));

        self.block_id = then_block;
        assert_eq!(self.shift(), Token::Special("then"));
        let saved_break = replace(&mut self.break_id, break_block);
        let saved_continue = replace(&mut self.continue_id, continue_block);
        self.parse_scope(HashMap::new(), false);
        self.push_instr(Instr::goto(self.continue_id));

        self.block_id = break_block;
        self.break_id = saved_break;
        self.continue_id = saved_continue;
    }

    fn parse_if(&mut self) {
        assert_eq!(self.shift(), Token::Special("if"));
        let then_block = self.push_block();
        let else_block = self.push_block();
        let end_block = self.push_block();
        let expr_instr = self.parse_expr();
        self.push_instr(Instr::Branch(expr_instr, then_block, else_block));

        self.block_id = then_block;
        assert_eq!(self.shift(), Token::Special("then"));
        self.parse_scope(HashMap::new(), true);
        self.push_instr(Instr::goto(end_block));

        self.block_id = else_block;
        if self.token() == &Token::Special("else") {
            self.shift();
            self.parse_scope(HashMap::new(), false);
        }
        self.push_instr(Instr::goto(end_block));

        self.block_id = end_block;
    }

    fn ref_new(&mut self, expr: InstrId) -> InstrId {
        self.push_instr(Instr::Op(ExprOp::ProductNew(Eval::TYPE_REF), vec![expr]))
    }

    fn ref_mut(&mut self, ref_instr: InstrId, expr: InstrId) -> InstrId {
        self.push_instr(Instr::Op(ExprOp::ProductSet(0), vec![ref_instr, expr]))
    }

    fn ref_get(&mut self, ref_instr: InstrId) -> InstrId {
        self.push_instr(Instr::Op(ExprOp::ProductGet(0), vec![ref_instr]))
    }

    fn parse_let(&mut self) {
        assert_eq!(self.shift(), Token::Special("let"));
        let name = if let Token::Name(name) = self.shift() {
            name
        } else {
            panic!()
        };
        let expr_instr = if self.token() == &Token::Special("=") {
            self.shift();
            self.parse_expr()
        } else {
            self.push_instr(Instr::Op(ExprOp::NilNew, vec![]))
        };
        let ref_instr = self.ref_new(expr_instr);
        self.scopes
            .last_mut()
            .unwrap()
            .insert(String::from(name), ref_instr);
    }

    fn parse_mut(&mut self) {
        assert_eq!(self.shift(), Token::Special("mut"));
        let name = if let Token::Name(name) = self.shift() {
            name
        } else {
            panic!()
        };
        let ref_instr = 'found: loop {
            for scope in self.scopes.iter().rev() {
                if let Some(&instr) = scope.get(name) {
                    break 'found instr;
                }
            }
            panic!("not found {name}");
        };

        assert_eq!(self.shift(), Token::Special("="));
        let expr_instr = self.parse_expr();
        self.ref_mut(ref_instr, expr_instr);
    }

    fn parse_expr(&mut self) -> InstrId {
        self.parse_infix0()
    }

    fn parse_single_expr(&mut self) -> InstrId {
        match self.shift() {
            Token::LitInt(lit) => self.push_instr(Instr::Op(ExprOp::IntNew(lit), vec![])),
            Token::LitStr(lit) => self.push_instr(Instr::Op(ExprOp::StrNew(lit), vec![])),
            Token::Special("(") => {
                let expr = self.parse_expr();
                assert_eq!(self.shift(), Token::Special(")"));
                expr
            }
            Token::Special("nil") => self.push_instr(Instr::Op(ExprOp::NilNew, vec![])),
            Token::Special(lit @ ("true" | "false")) => {
                let nil = self.push_instr(Instr::Op(ExprOp::NilNew, vec![]));
                self.push_instr(Instr::Op(
                    ExprOp::SumNew(Eval::TYPE_RESULT, (lit == "true") as _),
                    vec![nil],
                ))
            }
            Token::Name(name) => {
                if self.token() != &Token::Special("(") {
                    return self.transpile_name(name);
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
                self.push_instr(Instr::Call(String::from(name), args))
            }
            token => panic!("{token:?} at: {}", &self.input[..32]),
        }
    }

    fn transpile_name(&mut self, name: &str) -> InstrId {
        let ref_instr = 'found: loop {
            for scope in self.scopes.iter().rev() {
                if let Some(&instr) = scope.get(name) {
                    break 'found instr;
                }
            }
            panic!("not found: {name} at {}", &self.input[..32]);
        };
        self.push_instr(Instr::Op(ExprOp::ProductGet(0), vec![ref_instr]))
    }

    fn parse_infix(&mut self, ops: &[&str], nested: impl Fn(&mut Self) -> InstrId) -> InstrId {
        let mut expr = nested(self);
        while let Some(name) = ops.iter().find(|op| self.token() == &Token::Special(op)) {
            self.shift();
            let latter_expr = nested(self);
            expr = self.transpile_infix(name, expr, latter_expr);
        }
        expr
    }

    fn parse_short_circut(&mut self, op: &str, nested: impl Fn(&mut Self) -> InstrId) -> InstrId {
        let expr = nested(self);
        if self.token() != &Token::Special(op) {
            expr
        } else {
            let mut expr = expr;
            let ref_instr = self.ref_new(expr);
            let end_block = self.push_block();
            while self.token() == &Token::Special(op) {
                self.shift();
                let alter_block = self.push_block();
                self.push_instr(match op {
                    "or" => Instr::Branch(expr, end_block, alter_block),
                    "and" => Instr::Branch(expr, alter_block, end_block),
                    _ => unreachable!(),
                });

                self.block_id = alter_block;
                expr = nested(self);
                self.ref_mut(ref_instr, expr);
            }
            self.push_instr(Instr::goto(end_block));

            self.block_id = end_block;
            self.ref_get(ref_instr)
        }
    }

    fn parse_infix0(&mut self) -> InstrId {
        self.parse_short_circut("or", Self::parse_infix1)
    }

    fn parse_infix1(&mut self) -> InstrId {
        self.parse_short_circut("and", Self::parse_infix2)
    }

    fn parse_infix2(&mut self) -> InstrId {
        self.parse_infix(&["==", "!=", ">", "<", ">=", "<="], Self::parse_infix3)
    }

    fn parse_infix3(&mut self) -> InstrId {
        self.parse_infix(&["+", "-"], Self::parse_infix4)
    }

    fn parse_infix4(&mut self) -> InstrId {
        self.parse_infix(&["*", "/", "%"], Self::parse_prefix)
    }

    fn parse_prefix(&mut self) -> InstrId {
        let ops = ["not", "-"];
        if let Some(op) = ops.iter().find(|op| self.token() == &Token::Special(op)) {
            self.shift();
            let expr = self.parse_prefix();
            self.transpile_prefix(op, expr)
        } else {
            self.parse_single_expr()
        }
    }

    fn transpile_prefix(&mut self, op: &str, expr: InstrId) -> InstrId {
        self.push_instr(Instr::Call(
            String::from(match op {
                "-" | "not" => "neg",
                _ => unreachable!(),
            }),
            vec![expr],
        ))
    }

    fn transpile_infix(&mut self, op: &str, expr1: InstrId, expr2: InstrId) -> InstrId {
        self.push_instr(Instr::Call(
            String::from(match op {
                "+" => "add",
                "-" => "sub",
                "*" => "mul",
                "/" => "div",
                "%" => "rem",
                "==" => "eq",
                "!=" => "ne",
                "<" => "lt",
                ">" => "gt",
                "<=" => "le",
                ">=" => "ge",
                _ => unreachable!(),
            }),
            vec![expr1, expr2],
        ))
    }
}
