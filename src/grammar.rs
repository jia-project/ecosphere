use pest::{
    iterators::{Pair, Pairs},
    pratt_parser::{Assoc, Op, PrattParser},
    Parser as _,
};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct Parser;

pub fn parse(input: &str) {
    let program = Parser::parse(Rule::Program, input).unwrap();
    // dbg!(&program);
    for rule in program {
        parse_rule(rule)
    }
}

fn parse_rule(pair: Pair<Rule>) {
    match pair.as_rule() {
        Rule::Expr => parse_expr(pair.into_inner()),
        // _ => todo!(),
        _ => {}
    }
}

pub fn parse_expr(pairs: Pairs<Rule>) {
    PrattParser::new()
        .op(Op::prefix(Rule::Not))
        .op(Op::infix(Rule::Add, Assoc::Left) | Op::infix(Rule::Sub, Assoc::Left))
        .map_primary(|primary| match primary.as_rule() {
            Rule::Integer => todo!(),
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::Add => todo!(),
            Rule::Sub => todo!(),
            _ => unreachable!(),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::Sub => todo!(),
            Rule::Not => todo!(),
            _ => unreachable!(),
        })
        // postfix
        .parse(pairs)
}
