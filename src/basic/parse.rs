use std::{collections::HashMap, mem::take};

use crate::{
    instr::{FuncBuilder, Val},
    loader::{Loader, MatchExpr, Param},
    mem::Mem,
};

#[derive(Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Name(&'a str),
    LitI32(i32),
    LitBool(bool),
    LitStr(&'a str),
    Special(&'a str),
}

impl<'a> Token<'a> {
    fn into_name(self) -> &'a str {
        if let Self::Name(name) = self {
            name
        } else {
            panic!("unexpected token {self:?}");
        }
    }
}

fn next_token<'a>(s: &mut &'a str) -> Option<Token<'a>> {
    *s = s.trim_start();
    if s.is_empty() {
        return None;
    }
    match s.split_whitespace().next().unwrap() {
        special @ ("prod" | "sum" | "func" | "tag" | "do" | "end" | "if" | "else" | "while"
        | "break" | "continue" | "return" | "let" | "is") => {
            *s = s.strip_prefix(special).unwrap();
            Some(Token::Special(special))
        }
        "true" => {
            *s = s.strip_prefix("true").unwrap();
            Some(Token::LitBool(true))
        }
        "false" => {
            *s = s.strip_prefix("false").unwrap();
            Some(Token::LitBool(false))
        }
        lit_i32 if lit_i32.starts_with(char::is_numeric) => {
            let lit_i32 = s.split(|c: char| !c.is_numeric()).next().unwrap();
            let token = Token::LitI32(lit_i32.parse().unwrap());
            *s = s.strip_prefix(lit_i32).unwrap();
            Some(token)
        }
        lit_str if lit_str.starts_with('"') => {
            let mut escaping = false;
            let (lit_str, next_s) = s[1..]
                .split_once(|c| {
                    let res = c == '"' && !escaping;
                    escaping = if c == '\\' { !escaping } else { false };
                    res
                })
                .unwrap();
            *s = next_s;
            Some(Token::LitStr(lit_str))
        }
        special if ["==", "!="].into_iter().any(|s| special.starts_with(s)) => {
            *s = s.strip_prefix(special).unwrap();
            Some(Token::Special(special))
        }
        special if special.starts_with(['=', '+', '-', '*', '/', '%', ',', '(', ')', '<', '>']) => {
            let (special, next_s) = s.split_at(1);
            *s = next_s;
            Some(Token::Special(special))
        }
        name => {
            let name = name
                .split(|c: char| !c.is_alphanumeric() && c != '.' && c != '_' && c != ':')
                .next()
                .unwrap();
            assert!(!name.is_empty());
            *s = s.strip_prefix(name).unwrap();
            Some(Token::Name(name))
        }
    }
}

pub struct Module<'a> {
    path: &'a str,
    source: &'a str,
    loader: &'a mut Loader,
    mem: &'a Mem,
    name_table: Vec<HashMap<&'a str, Val>>,
    builder: FuncBuilder,
}

impl<'a> Module<'a> {
    pub fn new(path: &'a str, source: &'a str, loader: &'a mut Loader, mem: &'a Mem) -> Self {
        Self {
            path,
            source,
            loader,
            mem,
            name_table: Vec::new(),
            builder: FuncBuilder::default(),
        }
    }

    pub fn work(mut self) {
        while let Some(token) = next_token(&mut self.source) {
            match token {
                Token::Special("func") => self.func(),
                Token::Special("prod") => todo!(),
                Token::Special("sum") => todo!(),
                Token::Special("tag") => todo!(),
                _ => panic!("unexpected token {token:?}"),
            }
        }
    }

    fn func(&mut self) {
        let name = next_token(&mut self.source).unwrap().into_name();
        assert_eq!(next_token(&mut self.source), Some(Token::Special("(")));
        self.name_table.push(HashMap::new());
        let mut param_list = Vec::new();
        let mut token = next_token(&mut self.source).unwrap();
        while token != Token::Special(")") {
            self.insert_name(token.into_name(), Val::Arg(param_list.len()));
            let mut param = Param::Match(MatchExpr::And(Vec::new()));
            token = next_token(&mut self.source).unwrap();
            if token == Token::Special("is") {
                param = Param::Genuine(
                    next_token(&mut self.source)
                        .unwrap()
                        .into_name()
                        .to_string(),
                );
                token = next_token(&mut self.source).unwrap();
            }
            // TODO match param
            assert!(token == Token::Special(",") || token == Token::Special(")"));
            param_list.push(param);
            if token == Token::Special(",") {
                token = next_token(&mut self.source).unwrap();
            }
        }
        self.do_block();
        self.name_table.pop().unwrap();
        let func = take(&mut self.builder).finish();
        self.loader.register_func(name, &param_list, func);
    }

    fn do_block(&mut self) {}

    fn insert_name(&mut self, name: &'a str, val: Val) {
        self.name_table.last_mut().unwrap().insert(name, val);
    }
}
