use std::{
    collections::HashMap,
    mem::{replace, take},
};

use crate::{
    instr::{CoreOp, FuncBuilder, Instr, InstrCall, LabelId, Val, ValConst},
    interp::I32,
    loader::{Loader, MatchExpr, Param},
    mem::Mem,
    OwnedName,
};

use super::obj::Str;

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
    while s.starts_with(";") {
        *s = s.trim_start_matches(|c| c != '\n').trim_start();
    }
    if s.is_empty() {
        return None;
    }
    let name_pat = |c: char| c.is_alphabetic() || c == '_' || c == '.';
    let name_pat_head = |c| name_pat(c) || c == ':';
    let name_pat_tail = |c| name_pat(c) || c.is_numeric();
    let special1 = ["mut->", "->", "==", "!=", "<<", ">>", ">=", "<="];
    match s.split_whitespace().next().unwrap() {
        // match longest possible
        special if special1.iter().any(|&s| special.starts_with(s)) => {
            let &special = special1
                .iter()
                .find(|&special| s.starts_with(special))
                .unwrap();
            *s = s.strip_prefix(special).unwrap();
            Some(Token::Special(special))
        }
        special
            if special.starts_with([
                '=', '+', '-', '*', '/', '%', ',', '(', ')', '<', '>', '^', '&', '|',
            ]) =>
        {
            let (special, next_s) = s.split_at(1);
            *s = next_s;
            Some(Token::Special(special))
        }
        // name and name-like specials
        name if name.starts_with(name_pat_head) => {
            let name = name.split(|c: char| !name_pat_tail(c)).next().unwrap();
            *s = s.strip_prefix(name).unwrap();
            Some(match name {
                special @ ("prod" | "sum" | "func" | "tag" | "alias" | "load" | "do" | "end"
                | "if" | "else" | "while" | "break" | "continue" | "return"
                | "match" | "let" | "mut" | "run" | "is" | "as" | "has" | "spawn"
                | "wait" | "and" | "or" | "not" | "_") => Token::Special(special),
                "true" => Token::LitBool(true),
                "false" => Token::LitBool(false),
                _ => Token::Name(name),
            })
        }

        lit_i32 if lit_i32.starts_with(char::is_numeric) => {
            let mut radix = 10;
            let mut prefix = "";
            let mut pat = char::is_numeric as fn(_) -> _;
            if lit_i32.starts_with("0x") {
                radix = 16;
                prefix = "0x";
                pat = |c: char| c.is_ascii_hexdigit();
            }
            let lit_i32 = lit_i32
                .strip_prefix(prefix)
                .unwrap()
                .split(|c: char| !pat(c))
                .next()
                .unwrap();
            let token = Token::LitI32(i32::from_str_radix(lit_i32, radix).unwrap());
            *s = s
                .strip_prefix(prefix)
                .unwrap()
                .strip_prefix(lit_i32)
                .unwrap();
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

        _ => panic!(
            "not a token {:.10}{}",
            s,
            if s.len() > 10 { ".." } else { "" }
        ),
    }
}

pub struct Module<'a> {
    path: &'a str,
    source: &'a str,
    loader: &'a mut Loader,
    mem: &'a Mem,
    name_table: Vec<HashMap<&'a str, Val>>,
    builder: FuncBuilder,
    token: Option<Token<'a>>,
    label_break: Option<LabelId>,
    label_continue: Option<LabelId>,
    alias_table: HashMap<&'a str, OwnedName>,
    pub load_list: Vec<OwnedName>,
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
            token: None,
            label_break: None,
            label_continue: None,
            alias_table: [
                ("Unit", "intrinsic.Unit".to_owned()),
                ("Ref", "intrinsic.Ref".to_owned()),
                ("Int", I32::NAME.to_owned()),
            ]
            .into_iter()
            .collect(),
            load_list: Vec::new(),
        }
    }

    fn shift(&mut self) -> Option<Token<'a>> {
        replace(&mut self.token, next_token(&mut self.source))
    }

    // notice this assert not EOF
    fn not_end(&self) -> bool {
        *self.token.as_ref().unwrap() != Token::Special("end")
    }

    pub fn load(&mut self) {
        self.shift();
        while let Some(token) = &self.token {
            match token {
                Token::Special("func") => self.func(),
                Token::Special("prod") => self.prod(),
                Token::Special("sum") => self.sum(),
                Token::Special("tag") => todo!(),
                Token::Special("alias") => self.alias(),
                Token::Special("load") => self.load_decl(),
                _ => panic!("unexpected token {token:?}"),
            }
        }
    }

    fn func(&mut self) {
        assert_eq!(self.shift(), Some(Token::Special("func")));
        let func_id = self.shift().unwrap().into_name();
        let func_id = self.canonical_name(func_id);
        dbg!(&func_id);
        assert_eq!(self.shift(), Some(Token::Special("(")));
        self.name_table.push(HashMap::new());
        let mut param_list = Vec::new();
        while *self.token.as_ref().unwrap() != Token::Special(")") {
            let name = self.shift().unwrap().into_name();
            let val = self.builder.push_instr(Instr::Alloc);
            self.builder
                .push_instr(Instr::Store(val, Val::Arg(param_list.len())));
            self.insert_name(name, val);
            let mut param = Param::Match(MatchExpr::Anything);
            if self.token == Some(Token::Special("is")) {
                self.shift();
                let name = self.shift().unwrap().into_name();
                param = Param::Genuine(self.canonical_name(name));
            }
            // TODO match param
            param_list.push(param);
            if self.token == Some(Token::Special(",")) {
                self.shift();
            } else {
                assert_eq!(self.token, Some(Token::Special(")")));
            }
        }
        self.shift().unwrap();
        self.do_block();
        self.name_table.pop().unwrap();
        let func = take(&mut self.builder).finish();
        self.loader.register_func(&func_id, &param_list, func);
    }

    fn prod(&mut self) {
        assert_eq!(self.shift(), Some(Token::Special("prod")));
        let name = self.shift().unwrap().into_name();
        let name = self.canonical_name(name);
        assert_eq!(self.shift(), Some(Token::Special("has")));
        let mut key_list = Vec::new();
        while self.not_end() {
            key_list.push(self.shift().unwrap().into_name());
            if self.token == Some(Token::Special("is")) {
                self.shift().unwrap();
                // TODO collect this type message
                assert!(matches!(self.shift().unwrap(), Token::Name(..)));
            }
        }
        self.shift().unwrap();
        self.loader.register_prod(&name, &key_list);
    }

    // identical to prod actually
    fn sum(&mut self) {
        assert_eq!(self.shift(), Some(Token::Special("sum")));
        let name = self.shift().unwrap().into_name();
        let name = self.canonical_name(name);
        assert_eq!(self.shift(), Some(Token::Special("has")));
        let mut key_list = Vec::new();
        while self.not_end() {
            key_list.push(self.shift().unwrap().into_name());
            if self.token == Some(Token::Special("is")) {
                self.shift().unwrap();
                // TODO collect this type message
                assert!(matches!(self.shift().unwrap(), Token::Name(..)));
            }
        }
        self.shift().unwrap();
        self.loader.register_sum(&name, &key_list);
    }

    fn alias(&mut self) {
        assert_eq!(self.shift(), Some(Token::Special("alias")));
        let name = self.shift().unwrap().into_name();
        if self.token != Some(Token::Special("has")) {
            let target = self.canonical_name(name);
            self.alias_table
                .insert(name.rsplit(".").next().unwrap(), target);
            return;
        }
        self.shift().unwrap();
        let namespace = self.canonical_name(name);
        while self.not_end() {
            let name = self.shift().unwrap().into_name();
            self.alias_table
                .insert(name, [&namespace, ".", name].concat());
        }
        self.shift().unwrap();
    }

    fn load_decl(&mut self) {
        assert_eq!(self.shift(), Some(Token::Special("load")));
        let name = self.shift().unwrap().into_name();
        let name = self.canonical_name(name);
        self.load_list.push(name);
    }

    fn do_block(&mut self) {
        if self.token == Some(Token::Special("do")) {
            self.shift();
            self.name_table.push(HashMap::new());
            while self.not_end() {
                self.stmt();
            }
            self.shift();
            self.name_table.pop().unwrap();
        } else {
            self.stmt();
        }
    }

    fn stmt(&mut self) {
        match self.token.as_ref().unwrap() {
            // add `_stmt` to avoid keyword confliction
            Token::Special("let") => self.let_stmt(),
            Token::Special("mut") => self.mut_stmt(),
            Token::Special("mut->") => self.set(),
            Token::Special("if") => self.if_stmt(),
            Token::Special("while") => self.while_stmt(),
            Token::Special("return") => self.return_stmt(),
            Token::Special("break") => {
                self.shift().unwrap();
                self.builder
                    .push_instr(Self::jmp(self.label_break.unwrap()));
            }
            Token::Special("continue") => {
                self.shift().unwrap();
                self.builder
                    .push_instr(Self::jmp(self.label_continue.unwrap()));
            }
            Token::Special("run") => {
                self.shift().unwrap();
                self.expr(); // limit to call expr?
            }
            Token::Special("wait") => self.wait(),
            token => panic!("unexpected token {token:?}"),
        }
    }

    fn let_stmt(&mut self) {
        assert_eq!(self.shift(), Some(Token::Special("let")));
        let name = self.shift().unwrap().into_name();
        let val = self.builder.push_instr(Instr::Alloc);
        if self.token == Some(Token::Special("=")) {
            self.shift();
            let expr_val = self.expr();
            self.builder.push_instr(Instr::Store(val, expr_val));
        }
        // only insert(override) name after expr is evaluated, so as Rust's name
        // shadowing
        self.insert_name(name, val);
    }

    fn mut_stmt(&mut self) {
        assert_eq!(self.shift(), Some(Token::Special("mut")));
        let name = self.shift().unwrap().into_name();
        let place_val = self.resolve_name(name);
        assert_eq!(self.shift(), Some(Token::Special("=")));
        let val = self.expr();
        self.builder.push_instr(Instr::Store(place_val, val));
    }

    fn set(&mut self) {
        assert_eq!(self.shift(), Some(Token::Special("mut->")));
        let key = self.shift().unwrap().into_name();
        let prod = self.expr();
        assert_eq!(self.shift(), Some(Token::Special("=")));
        let val = self.expr();
        self.builder
            .push_instr(Instr::CoreOp(CoreOp::Set(prod, key.to_string(), val)));
    }

    fn if_stmt(&mut self) {
        assert_eq!(self.shift(), Some(Token::Special("if")));
        let expr = self.expr();
        if self.token == Some(Token::Special("match")) {
            self.if_match(expr);
            return;
        }
        let label_then = self.builder.push_block();
        let label_else = self.builder.push_block();
        let label_end = self.builder.push_block();
        self.builder
            .push_instr(Instr::Br(expr, label_then, label_else));
        self.builder.with_block(label_then);
        self.do_block();
        self.builder.push_instr(Self::jmp(label_end));
        self.builder.with_block(label_else);
        if self.token == Some(Token::Special("else")) {
            self.shift().unwrap();
            self.do_block();
        }
        self.builder.push_instr(Self::jmp(label_end));
        self.builder.with_block(label_end);
    }

    // currently for single match (similar to if let in Rust), a double end
    // pattern may be required:
    // if ... match ... do
    //     ...
    // end end
    // first one close do block, second one close match list
    // consider specialize it if myself made mistake on it too often
    fn if_match(&mut self, sum: Val) {
        assert_eq!(self.shift(), Some(Token::Special("match")));
        let mut label_next = self.builder.push_block();
        let label_end = self.builder.push_block();
        while self.not_end() {
            // TODO add wildcard case
            let key = self.shift().unwrap().into_name();
            let is_val = self
                .builder
                .push_instr(Instr::CoreOp(CoreOp::Is(sum, key.to_string())));
            let label_then = label_next;
            label_next = self.builder.push_block();
            self.builder
                .push_instr(Instr::Br(is_val, label_then, label_next));
            self.builder.with_block(label_then);
            match self.shift().unwrap() {
                Token::Special("_") => self.do_block(), // skip name binding
                Token::Name(name) => {
                    let name_val = self
                        .builder
                        .push_instr(Instr::CoreOp(CoreOp::As(sum, key.to_string())));
                    self.name_table.push(HashMap::new());
                    self.insert_name(name, name_val);
                    self.do_block();
                    self.name_table.pop().unwrap();
                }
                token => panic!("unexpected token: {token:?}"),
            }
            self.builder.push_instr(Self::jmp(label_end));
        }
        self.shift().unwrap();
        self.builder.with_block(label_next);
        self.builder.push_instr(Self::jmp(label_end));
        self.builder.with_block(label_end);
    }

    fn while_stmt(&mut self) {
        assert_eq!(self.shift(), Some(Token::Special("while")));
        let label_expr = self.builder.push_block();
        let label_continue = self.builder.push_block();
        let saved_continue = self.label_continue.replace(label_continue);
        let label_break = self.builder.push_block();
        let saved_break = self.label_break.replace(label_break);
        self.builder.push_instr(Self::jmp(label_expr));
        self.builder.with_block(label_expr);
        let val = self.expr();
        self.builder
            .push_instr(Instr::Br(val, label_continue, label_break));
        self.builder.with_block(label_continue);
        self.do_block();
        self.builder.push_instr(Self::jmp(label_expr));
        self.builder.with_block(label_break);
        self.label_continue = saved_continue;
        self.label_break = saved_break;
    }

    fn return_stmt(&mut self) {
        assert_eq!(self.shift(), Some(Token::Special("return")));
        let val = self.expr();
        self.builder.push_instr(Instr::Ret(val));
    }

    fn wait(&mut self) {
        assert_eq!(self.shift(), Some(Token::Special("wait")));
        let val = self.expr();
        self.builder.push_instr(Instr::Wait(val));
    }

    fn jmp(label: LabelId) -> Instr {
        Instr::Br(Val::Const(ValConst::Bool(true)), label, label)
    }

    // use a precedence similar to Rust
    // expr0 := expr1 op0 expr0 | expr1
    // expr1 := op1 expr1 | expr1 [->|is|as] {key} | expr2
    // expr2 := lit_i32 | lit_str | lit_bool | {name}
    //        | {call expr} | spawn ... | prod ... | sum ...
    //        | ( expr0 )

    fn expr(&mut self) -> Val {
        self.expr0()
    }

    fn expr0(&mut self) -> Val {
        let mut val = self.expr1();
        while let Some(next_val) = self.guess_binary(val) {
            val = next_val;
        }
        val
    }

    fn expr1(&mut self) -> Val {
        let mut val = self.expr1_prefix();
        while let Some(next_val) = self.guess_postfix(val) {
            val = next_val;
        }
        val
    }

    fn expr2(&mut self) -> Val {
        match self.token.as_ref().unwrap() {
            Token::Special("(") => self.paren(),
            Token::Special("_") => {
                let val = Val::Const(ValConst::Unit);
                self.shift();
                val
            }
            Token::Special("spawn") => self.spawn(),
            Token::Special("prod") => self.prod_expr(),
            Token::Special("sum") => self.sum_expr(),
            Token::LitBool(content) => {
                let val = Val::Const(ValConst::Bool(*content));
                self.shift();
                val
            }
            Token::LitI32(content) => {
                let val = Val::Const(ValConst::I32(*content));
                self.shift();
                val
            }
            // every time copy a new str object, ensure asset is read-only
            // should bool and i32 be copied as well?
            Token::LitStr(content) => {
                let val = Val::Const(ValConst::Asset(
                    self.loader
                        .create_asset(Str(content.to_string()), &self.mem),
                ));
                self.shift();
                self.builder
                    .push_instr(Instr::Op("basic.str_copy".to_string(), vec![val]))
            }
            Token::Name(..) => {
                // I don't really like to lookahead twice :|
                let name = self.shift().unwrap().into_name();
                self.guess_call(name).unwrap_or_else(|| {
                    let val = self.resolve_name(name);
                    self.builder.push_instr(Instr::Load(val))
                })
            }
            token => panic!("unexpected token {token:?}"),
        }
    }

    fn paren(&mut self) -> Val {
        assert_eq!(self.shift(), Some(Token::Special("(")));
        let val = self.expr0();
        assert_eq!(self.shift(), Some(Token::Special(")")));
        val
    }

    fn spawn(&mut self) -> Val {
        assert_eq!(self.shift(), Some(Token::Special("spawn")));
        let name = self.shift().unwrap().into_name();
        let instr_call = self.call_like(name);
        self.builder.push_instr(Instr::Spawn(instr_call))
    }

    fn expr1_prefix(&mut self) -> Val {
        if let Some(Token::Special(op)) = self.token {
            if op == "not" || op == "-" {
                self.shift().unwrap();
                let val = self.expr1();
                return self.unary(op, val);
            }
        }
        self.expr2()
    }

    fn prod_expr(&mut self) -> Val {
        assert_eq!(self.shift(), Some(Token::Special("prod")));
        let name = self.shift().unwrap().into_name();
        let name = self.canonical_name(name);
        let prod = self
            .builder
            .push_instr(Instr::CoreOp(CoreOp::NewProd(name)));
        if self.token == Some(Token::Special("do")) {
            self.shift().unwrap();
            while self.not_end() {
                assert_eq!(self.shift(), Some(Token::Special("mut")));
                let key = self.shift().unwrap().into_name();
                assert_eq!(self.shift(), Some(Token::Special("=")));
                let val = self.expr();
                self.builder
                    .push_instr(Instr::CoreOp(CoreOp::Set(prod, key.to_string(), val)));
            }
            self.shift().unwrap(); // end
        }
        prod
    }

    fn sum_expr(&mut self) -> Val {
        assert_eq!(self.shift(), Some(Token::Special("sum")));
        let name = self.shift().unwrap().into_name();
        let (name, key) = name.rsplit_once('.').unwrap();
        let name = self.canonical_name(name);
        let expr = self.expr();
        self.builder
            .push_instr(Instr::CoreOp(CoreOp::NewSum(name, key.to_string(), expr)))
    }

    fn guess_call(&mut self, name: &str) -> Option<Val> {
        if self.token != Some(Token::Special("(")) {
            return None;
        }
        let instr_call = self.call_like(name);
        Some(self.builder.push_instr(Instr::Call(instr_call)))
    }

    fn call_like(&mut self, name: &str) -> InstrCall {
        assert_eq!(self.shift(), Some(Token::Special("(")));
        let mut arg_list = Vec::new();
        while *self.token.as_ref().unwrap() != Token::Special(")") {
            let val = self.expr0();
            // TODO morph
            arg_list.push((val, None));
            if self.token == Some(Token::Special(",")) {
                self.shift();
            } else {
                assert_eq!(self.token, Some(Token::Special(")")));
            }
        }
        self.shift().unwrap();
        InstrCall {
            name: self.canonical_name(name),
            arg_list,
        }
    }

    fn guess_binary(&mut self, val: Val) -> Option<Val> {
        let token = if let Some(token) = &self.token {
            token
        } else {
            return None;
        };
        // simply copied from Rust reference
        let tower = [
            &["*", "/", "%"] as &[_],
            &["+", "-"],
            &["<<", ">>"],
            &["&"],
            &["^"],
            &["|"],
            &["==", "!=", ">", "<", ">=", "<="],
            &["and"],
            &["or"],
        ];
        let token_level = |token: &Token| {
            for (i, layer) in tower.iter().enumerate() {
                if layer.iter().any(|op| *token == Token::Special(op)) {
                    return Some(i);
                }
            }
            None
        };
        let level = if let Some(level) = token_level(token) {
            level
        } else {
            return None;
        };
        let op = if let Token::Special(op) = self.shift().unwrap() {
            op
        } else {
            unreachable!()
        };
        let middle_expr = self.expr1(); // stop before next op0
        let right_val = if self
            .token
            .as_ref()
            .and_then(token_level)
            .map(|right_level| {
                if right_level == level {
                    assert_ne!(level, 6); // require explicit paren
                }
                right_level < level
            })
            .unwrap_or(false)
        {
            self.guess_binary(middle_expr).unwrap()
        } else {
            middle_expr
        };
        Some(self.binary(op, val, right_val))
    }

    fn unary(&mut self, op: &str, val: Val) -> Val {
        self.builder.push_instr(Instr::CoreOp(match op {
            "not" => CoreOp::BoolNeg(val),
            "-" => CoreOp::I32Neg(val),
            "~" => CoreOp::I32Inv(val),
            _ => unreachable!(),
        }))
    }

    fn binary(&mut self, op: &str, v1: Val, v2: Val) -> Val {
        match op {
            "or" => {
                let not_v1 = self.unary("not", v1);
                let not_v2 = self.unary("not", v2);
                let not_val = self.binary("and", not_v1, not_v2);
                self.unary("not", not_val)
            }
            "|" => {
                let inv_v1 = self.unary("~", v1);
                let inv_v2 = self.unary("~", v2);
                let inv_val = self.binary("&", inv_v1, inv_v2);
                self.unary("~", inv_val)
            }
            "^" => {
                let v3 = self.binary("&", v1, v2);
                let v4 = self.unary("~", v3);
                let v5 = self.binary("|", v1, v2);
                self.binary("&", v4, v5)
            }
            "%" => {
                let v3 = self.binary("/", v1, v2);
                let v4 = self.binary("*", v3, v2);
                self.binary("-", v1, v4)
            }
            "-" => {
                let v3 = self.unary("-", v2);
                self.binary("+", v1, v3)
            }
            "<=" => {
                let v3 = self.binary("<", v1, v2);
                let v4 = self.binary("==", v1, v2);
                self.binary("or", v3, v4)
            }
            ">" => {
                let v3 = self.binary("<=", v1, v2);
                self.unary("not", v3)
            }
            "!=" => {
                let v3 = self.binary("==", v1, v2);
                self.unary("not", v3)
            }
            ">=" => {
                let v3 = self.binary("<", v1, v2);
                self.unary("not", v3)
            }
            op => self.builder.push_instr(Instr::CoreOp(match op {
                "+" => CoreOp::I32Add(v1, v2),
                "*" => CoreOp::I32Mul(v1, v2),
                "/" => CoreOp::I32Div(v1, v2),
                "<" => CoreOp::I32Lt(v1, v2),
                "==" => CoreOp::I32Eq(v1, v2),
                "&" => CoreOp::I32And(v1, v2),
                "<<" => CoreOp::I32Shl(v1, v2),
                ">>" => CoreOp::I32Shr(v1, v2),
                "and" => CoreOp::BoolAnd(v1, v2),
                _ => unreachable!(),
            })),
        }
    }

    fn guess_postfix(&mut self, val: Val) -> Option<Val> {
        if let Some(Token::Special(op)) = self.token {
            if op == "->" || op == "is" || op == "as" {
                self.shift().unwrap();
                let key = self.shift().unwrap().into_name().to_string();
                return Some(self.builder.push_instr(Instr::CoreOp(match op {
                    "->" => CoreOp::Get(val, key),
                    "is" => CoreOp::Is(val, key),
                    "as" => CoreOp::As(val, key),
                    _ => unreachable!(),
                })));
            }
        }
        None
    }

    fn insert_name(&mut self, name: &'a str, val: Val) {
        self.name_table.last_mut().unwrap().insert(name, val);
    }

    fn resolve_name(&self, name: &str) -> Val {
        for scope in self.name_table.iter().rev() {
            if let Some(val) = scope.get(name) {
                return *val;
            }
        }
        panic!("cannot resolve name {name}");
    }

    fn canonical_name(&self, name: &str) -> OwnedName {
        if name.starts_with("_.") {
            return name.strip_prefix("_.").unwrap().to_string();
        }
        for (alias, replace) in &self.alias_table {
            if let Some(tail) = name.strip_prefix(alias) {
                return replace.clone() + tail;
            }
        }

        return [self.path, ".", name].concat();
    }
}
