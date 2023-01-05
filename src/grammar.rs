use std::collections::HashMap;

use pest::{
    iterators::{Pair, Pairs},
    pratt_parser::{Assoc, Op, PrattParser},
    Parser as _,
};
use pest_derive::Parser;

use crate::{Instruction, InstructionLiteral, Operator2, RegisterIndex};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct Parser;

pub fn parse(input: &str) -> Box<[Instruction]> {
    let program = Parser::parse(Rule::Program, input).unwrap();
    // dbg!(&program);
    let mut layers = Layers::default();
    layers.enter();
    let mut visitor = ProgramVisitor {
        instructions: Default::default(),
        layers,
    };
    for pair in program {
        visitor.visit_program_stmt(pair)
    }
    visitor.push_instruction(Instruction::MakeLiteralObject(0, InstructionLiteral::Nil));
    visitor.push_instruction(Instruction::Return(0));
    visitor.instructions.into_boxed_slice()
}

struct ProgramVisitor {
    instructions: Vec<Instruction>,
    layers: Layers,
}

#[derive(Default)]
struct Layers {
    scopes: Vec<HashMap<String, RegisterIndex>>,
    register_level: RegisterIndex,
}

impl Layers {
    fn allocate(&mut self) -> RegisterIndex {
        let r = self.register_level;
        self.register_level += 1;
        r
    }

    fn save(&mut self) -> RegisterIndex {
        self.register_level
    }

    fn restore(&mut self, level: RegisterIndex) {
        self.register_level = level;
    }

    fn enter(&mut self) {
        self.scopes.push(Default::default())
    }

    fn exit(&mut self) {
        self.scopes.pop().unwrap();
    }

    fn allocate_name(&mut self, name: String) {
        let r = self.allocate();
        self.scopes.last_mut().unwrap().insert(name, r);
    }

    fn find(&mut self, name: &str) -> Option<RegisterIndex> {
        for scope in self.scopes.iter().rev() {
            if let Some(r) = scope.get(name) {
                return Some(*r);
            }
        }
        None
    }
}

impl ProgramVisitor {
    fn visit_program_stmt(&mut self, stmt: Pair<'_, Rule>) {
        match stmt.as_rule() {
            Rule::MakeFunctionStmt => self.visit_make_function_stmt(stmt.into_inner()),
            Rule::EOI => {}
            _ => self.visit_stmt(stmt),
        }
    }

    fn visit_make_function_stmt(&mut self, mut pairs: Pairs<'_, Rule>) {
        let name = pairs.next().unwrap().as_str().to_owned();
        let parameters = pairs
            .next()
            .unwrap()
            .into_inner()
            .map(|param| param.as_str().to_owned())
            .collect::<Vec<_>>();
        let arity = parameters.len();
        let mut layers = Layers::default();
        layers.enter();
        for parameter in parameters {
            layers.allocate_name(parameter);
        }
        let mut visitor = FunctionVisitor {
            instructions: Default::default(),
            layers,
        };
        let expr = pairs.next().unwrap();
        assert_eq!(expr.as_rule(), Rule::BlockExpr); // or allow single line function?
        visitor.visit_primary_expr(expr);
        visitor.layers.exit();
        if visitor
            .instructions
            .last()
            .filter(|instruction| matches!(instruction, Instruction::Return(_)))
            .is_none()
        {
            visitor.push_instruction(Instruction::Return(arity as _));
        }
        self.instructions.push(Instruction::MakeFunction(
            Box::new([]),
            name,
            arity,
            visitor.instructions.into_boxed_slice(),
        ))
    }
}

struct FunctionVisitor {
    instructions: Vec<Instruction>,
    layers: Layers,
}

trait StmtVisitor {
    fn push_instruction(&mut self, instruction: Instruction);

    fn layers(&mut self) -> &mut Layers;

    fn visit_expr(&mut self, pairs: Pairs<'_, Rule>) -> RegisterIndex {
        let expr = PrattParser::new()
            .op(Op::prefix(Rule::Not))
            .op(Op::infix(Rule::Add, Assoc::Left) | Op::infix(Rule::Sub, Assoc::Left))
            .map_primary(Expr::Primary)
            .map_infix(|lhs, op, rhs| Expr::Operator2(op, Box::new(lhs), Box::new(rhs)))
            .map_prefix(|op, rhs| Expr::Operator1(op, Box::new(rhs)))
            // postfix
            .parse(pairs);
        self.visit_expr_internal(expr)
    }

    fn visit_expr_internal(&mut self, expr: Expr<'_>) -> RegisterIndex {
        match expr {
            Expr::Primary(expr) => self.visit_primary_expr(expr),
            Expr::Operator2(op, expr1, expr2) => {
                let level = self.layers().save();
                let r1 = self.visit_expr_internal(*expr1);
                let r2 = self.visit_expr_internal(*expr2);
                let op = match op.as_rule() {
                    Rule::Add => Operator2::Add,
                    _ => unreachable!(),
                };
                self.layers().restore(level);
                let r = self.layers().allocate();
                self.push_instruction(Instruction::Operator2(r, op, r1, r2));
                r
            }
            Expr::Operator1(op, expr) => todo!(),
        }
    }

    fn visit_primary_expr(&mut self, expr: Pair<'_, Rule>) -> RegisterIndex {
        match expr.as_rule() {
            Rule::Expr => self.visit_expr(expr.into_inner()),
            Rule::Ident => {
                if let Some(r) = self.layers().find(expr.as_str()) {
                    r
                } else {
                    let r = self.layers().allocate();
                    self.push_instruction(Instruction::Load(r, expr.as_str().to_owned()));
                    r
                }
            }
            Rule::Integer => {
                let register = self.layers().allocate();
                self.push_instruction(Instruction::MakeLiteralObject(
                    register,
                    InstructionLiteral::Integer(expr.as_str().parse().unwrap()),
                ));
                register
            }
            Rule::String => {
                // handle escaping properly
                let value = expr.as_str()[1..expr.as_str().len() - 1].to_owned();
                let register = self.layers().allocate();
                self.push_instruction(Instruction::MakeLiteralObject(
                    register,
                    InstructionLiteral::String(value),
                ));
                register
            }
            Rule::Float => todo!(),
            Rule::BlockExpr => {
                let mut expr = expr.into_inner().peekable();
                if expr.peek().is_none() {
                    let r = self.layers().allocate();
                    self.push_instruction(Instruction::MakeLiteralObject(
                        r,
                        InstructionLiteral::Nil,
                    ));
                    return r;
                }
                self.layers().enter();
                let level = self.layers().save();
                let mut item;
                while {
                    item = expr.next().unwrap();
                    expr.peek().is_some()
                } {
                    self.visit_stmt(item);
                    self.layers().restore(level);
                }
                let r = if item.as_rule() == Rule::ValueExpr {
                    self.visit_expr(item.into_inner())
                } else {
                    self.visit_stmt(item);
                    self.layers().restore(level);
                    let r = self.layers().allocate();
                    self.push_instruction(Instruction::MakeLiteralObject(
                        r,
                        InstructionLiteral::Nil,
                    ));
                    r
                };
                self.layers().exit();
                r
            }
            Rule::CallExpr => {
                let mut expr = expr.into_inner();
                let name = expr.next().unwrap().as_str().to_owned();
                let mut arity = 0;
                let level = self.layers().save();
                for expr in expr.next().unwrap().into_inner() {
                    assert_eq!(expr.as_rule(), Rule::Expr);
                    let register = self.visit_expr(expr.into_inner());
                    assert_eq!(register, level + arity);
                    arity += 1;
                }
                self.push_instruction(Instruction::Call(
                    level,
                    Box::new([]),
                    name,
                    (level..level + arity).collect(),
                ));
                self.layers().restore(level + 1);
                level
            }
            _ => unreachable!(),
        }
    }

    fn visit_stmt(&mut self, stmt: Pair<'_, Rule>) {
        match stmt.as_rule() {
            Rule::InspectStmt => self.visit_inspect_stmt(stmt.into_inner()),
            Rule::Expr => {
                self.visit_expr(stmt.into_inner());
            }
            _ => unreachable!(),
        }
    }

    fn visit_inspect_stmt(&mut self, mut pairs: Pairs<'_, Rule>) {
        let register = self.visit_expr(pairs.next().unwrap().into_inner());
        self.push_instruction(Instruction::Inspect(register))
    }
}

enum Expr<'a> {
    Primary(Pair<'a, Rule>),
    Operator1(Pair<'a, Rule>, Box<Expr<'a>>),
    Operator2(Pair<'a, Rule>, Box<Expr<'a>>, Box<Expr<'a>>),
}

impl StmtVisitor for ProgramVisitor {
    fn push_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction)
    }

    fn layers(&mut self) -> &mut Layers {
        &mut self.layers
    }
}

impl StmtVisitor for FunctionVisitor {
    fn push_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction)
    }

    fn layers(&mut self) -> &mut Layers {
        &mut self.layers
    }
}
