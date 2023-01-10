use std::collections::HashMap;

use pest::{
    iterators::Pair,
    pratt_parser::{Assoc, Op, PrattParser},
    Parser as _,
};
use pest_derive::Parser;

use crate::{Instruction, InstructionLiteral, Operator1, Operator2, RegisterIndex, TypeOperator};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct Parser;

pub fn parse(input: &str) -> Box<[Instruction]> {
    let program = Parser::parse(Rule::Program, input).unwrap();
    let mut layers = Context::default();
    layers.enter();
    let mut visitor = ProgramVisitor {
        instructions: Default::default(),
        context: layers,
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
    context: Context,
}

#[derive(Default)]
struct Context {
    scopes: Vec<HashMap<String, RegisterIndex>>,
    register_level: RegisterIndex,
}

impl Context {
    fn allocate(&mut self) -> RegisterIndex {
        let r = self.register_level;
        self.register_level += 1;
        r
    }

    fn save(&self) -> RegisterIndex {
        self.register_level
    }

    fn restore(&mut self, level: RegisterIndex) {
        assert!(self.register_level >= level); // always?
        self.register_level = level;
    }

    fn restore_allocate(&mut self, level: RegisterIndex) {
        self.register_level = level + 1;
    }

    fn enter(&mut self) {
        self.scopes.push(Default::default())
    }

    fn exit(&mut self) {
        self.scopes.pop().unwrap();
    }

    fn bind(&mut self, name: String, register: RegisterIndex) {
        self.scopes.last_mut().unwrap().insert(name, register);
    }

    fn find(&self, name: &str) -> Option<RegisterIndex> {
        for scope in self.scopes.iter().rev() {
            if let Some(r) = scope.get(name) {
                return Some(*r);
            }
        }
        None
    }

    fn find_update(&mut self, name: &str, register: RegisterIndex) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(r) = scope.get_mut(name) {
                *r = register;
                return true;
            }
        }
        false
    }
}

impl ProgramVisitor {
    fn visit_program_stmt(&mut self, stmt: Pair<'_, Rule>) {
        match stmt.as_rule() {
            Rule::MakeFunctionStmt => self.visit_make_function_stmt(stmt),
            Rule::MakeTypeStmt => self.visit_make_type_stmt(stmt),
            Rule::EOI => {}
            _ => self.visit_stmt(stmt),
        }
    }

    fn visit_make_function_stmt(&mut self, stmt: Pair<'_, Rule>) {
        let mut stmt = stmt.into_inner();
        // TODO context parameters
        let name = stmt.next().unwrap().as_str().to_owned();
        let parameters = stmt.next().unwrap();
        let expr = stmt.next().unwrap();
        assert_eq!(expr.as_rule(), Rule::BlockExpr); // or allow single line function?

        let parameters = parameters
            .into_inner()
            .map(|param| param.as_str().to_owned())
            .collect::<Vec<_>>();
        let arity = parameters.len();

        let mut context = Context::default();
        context.restore(arity as _);
        context.enter();
        for (i, parameter) in parameters.into_iter().enumerate() {
            context.bind(parameter, i as _);
        }

        let mut visitor = FunctionVisitor {
            instructions: Default::default(),
            context,
        };
        visitor.visit_primary_expr(expr);
        visitor.context.exit();
        if !matches!(visitor.instructions.last(), Some(Instruction::Return(_))) {
            let r = visitor.context.allocate();
            visitor.push_instruction(Instruction::MakeLiteralObject(r, InstructionLiteral::Nil));
            visitor.push_instruction(Instruction::Return(r));
        }

        self.instructions.push(Instruction::MakeFunction(
            Box::new([]),
            name,
            arity,
            visitor.instructions.into_boxed_slice(),
        ))
    }

    fn visit_make_type_stmt(&mut self, stmt: Pair<'_, Rule>) {
        let mut stmt = stmt.into_inner();
        let name = stmt.next().unwrap().as_str().to_owned();
        let items = stmt.next().unwrap();

        fn visit(visitor: &mut ProgramVisitor, name: String, items: Pair<'_, Rule>) {
            let type_op = match items.as_rule() {
                Rule::ProductTypeItems => TypeOperator::Product,
                Rule::SumTypeItems => TypeOperator::Sum,
                _ => unreachable!(),
            };
            let mut item_names = Vec::new();
            for item in items.into_inner() {
                let mut item = item.into_inner();
                let item_name = item.next().unwrap().as_str();
                item_names.push(item_name.to_owned());
                if let Some(items) = item.next() {
                    visit(visitor, name.clone() + "." + item_name, items)
                }
            }
            visitor.instructions.push(Instruction::MakeType(
                name,
                type_op,
                item_names.into_boxed_slice(),
            ))
        }
        visit(self, name, items)
    }
}

struct FunctionVisitor {
    instructions: Vec<Instruction>,
    context: Context,
}

enum Expr<'a> {
    Primary(Pair<'a, Rule>),
    Operator1(Pair<'a, Rule>, Box<Expr<'a>>),
    Operator2(Pair<'a, Rule>, Box<Expr<'a>>, Box<Expr<'a>>),
}

trait StmtVisitor {
    fn push_instruction(&mut self, instruction: Instruction);

    fn context(&mut self) -> &mut Context;

    fn visit_expr(&mut self, expr: Pair<'_, Rule>) -> RegisterIndex {
        let expr = PrattParser::new()
            .op(Op::prefix(Rule::Inspect))
            .op(Op::postfix(Rule::Or))
            .op(Op::postfix(Rule::And))
            .op(Op::prefix(Rule::Not))
            .op(Op::postfix(Rule::Is))
            .op(Op::infix(Rule::LessThan, Assoc::Left)
                | Op::infix(Rule::GreaterThan, Assoc::Left)
                | Op::infix(Rule::Equal, Assoc::Left))
            .op(Op::infix(Rule::Add, Assoc::Left) | Op::infix(Rule::Sub, Assoc::Left))
            .op(Op::prefix(Rule::Sub))
            .op(Op::postfix(Rule::Dot) | Op::postfix(Rule::As) | Op::postfix(Rule::Call1))
            .map_primary(Expr::Primary)
            .map_infix(|lhs, op, rhs| Expr::Operator2(op, Box::new(lhs), Box::new(rhs)))
            .map_prefix(|op, rhs| Expr::Operator1(op, Box::new(rhs)))
            .parse(expr.into_inner());
        self.visit_expr_internal(expr)
    }

    fn visit_expr_internal(&mut self, expr: Expr<'_>) -> RegisterIndex {
        match expr {
            Expr::Primary(expr) => self.visit_primary_expr(expr),
            Expr::Operator2(op, expr1, expr2) => {
                let r = self.context().save();
                let r1 = self.visit_expr_internal(*expr1);
                let r2 = self.visit_expr_internal(*expr2);
                let op = match op.as_rule() {
                    Rule::Add => Operator2::Add,
                    Rule::Sub => Operator2::Sub,
                    Rule::And => Operator2::And,
                    Rule::Or => Operator2::Or,
                    Rule::LessThan => Operator2::LessThan,
                    Rule::GreaterThan => Operator2::GreaterThan,
                    Rule::Equal => Operator2::Equal,
                    _ => unreachable!(),
                };
                self.context().restore_allocate(r);
                self.push_instruction(Instruction::Operator2(r, op, r1, r2));
                r
            }
            Expr::Operator1(op, expr) => {
                let r = self.context().save();
                let r1 = self.visit_expr_internal(*expr);

                if op.as_rule() == Rule::Call1 {
                    let mut op = op.into_inner();
                    let name = op.next().unwrap().as_str().to_owned();
                    let arguments = op.next().unwrap();

                    let arguments = arguments
                        .into_inner()
                        .map(|argument| {
                            assert_eq!(argument.as_rule(), Rule::Expr);
                            self.visit_expr(argument)
                        })
                        .collect();
                    self.context().restore_allocate(r);
                    self.push_instruction(Instruction::Call(r, Box::new([r1]), name, arguments));
                    return r;
                }

                if op.as_rule() == Rule::Inspect {
                    self.push_instruction(Instruction::Inspect(r1));
                    return r1;
                }

                self.context().restore_allocate(r);
                match op.as_rule() {
                    Rule::Not => {
                        self.push_instruction(Instruction::Operator1(r, Operator1::Not, r1));
                        r
                    }
                    Rule::Sub => todo!(),
                    Rule::Dot => {
                        let component = op.into_inner().next().unwrap().as_str().to_owned();
                        self.push_instruction(Instruction::Get(r, r1, component));
                        r
                    }
                    Rule::As => {
                        let variant = op.into_inner().next().unwrap().as_str().to_owned();
                        self.push_instruction(Instruction::As(r, r1, variant));
                        r
                    }
                    Rule::Is => {
                        let mut op = op.into_inner();
                        let variant = op.next().unwrap().as_str();
                        todo!()
                    }
                    Rule::Match => todo!(),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn visit_primary_expr(&mut self, expr: Pair<'_, Rule>) -> RegisterIndex {
        match expr.as_rule() {
            Rule::Expr => self.visit_expr(expr),
            Rule::Ident => {
                if let Some(r) = self.context().find(expr.as_str()) {
                    r
                } else {
                    let r = self.context().allocate();
                    self.push_instruction(Instruction::Load(r, expr.as_str().to_owned()));
                    r
                }
            }
            Rule::Integer => {
                let r = self.context().allocate();
                self.push_instruction(Instruction::MakeLiteralObject(
                    r,
                    InstructionLiteral::Integer(expr.as_str().parse().unwrap()),
                ));
                r
            }
            Rule::String => {
                // handle escaping properly
                let value = expr.as_str()[1..expr.as_str().len() - 1].to_owned();
                let r = self.context().allocate();
                self.push_instruction(Instruction::MakeLiteralObject(
                    r,
                    InstructionLiteral::String(value),
                ));
                r
            }
            Rule::Float => todo!(),
            Rule::DataExpr => self.visit_data_expr(expr),
            Rule::BlockExpr => self.visit_block_expr(expr),
            Rule::Call0Expr => {
                let mut expr = expr.into_inner();
                let name = expr.next().unwrap().as_str();
                let arguments = expr.next().unwrap();
                self.visit_call_expr([].into_iter(), name, arguments.into_inner())
            }
            Rule::CallNExpr => {
                let mut expr = expr.into_inner();
                let context = expr.next().unwrap();
                let name = expr.next().unwrap().as_str();
                let arguments = expr.next().unwrap();
                self.visit_call_expr(context.into_inner(), name, arguments.into_inner())
            }
            _ => unreachable!(),
        }
    }

    fn visit_data_expr(&mut self, expr: Pair<'_, Rule>) -> RegisterIndex {
        let mut expr = expr.into_inner();
        let name = expr.next().unwrap().as_str().to_owned();
        let items = expr.next().unwrap();
        self.visit_data_items(name, items)
    }

    fn visit_data_items(&mut self, name: String, items: Pair<'_, Rule>) -> RegisterIndex {
        let data = items
            .into_inner()
            .map(|item| {
                let mut item = item.into_inner();
                let item_name = item.next().unwrap().as_str();
                let value = item.next().unwrap();
                (
                    item_name.to_owned(),
                    match value.as_rule() {
                        Rule::Expr => self.visit_expr(value),
                        Rule::DataItems => self.visit_data_items(name.clone() + item_name, value),
                        _ => unreachable!(),
                    },
                )
            })
            .collect();
        let r = self.context().allocate();
        self.push_instruction(Instruction::MakeDataObject(r, name, data));
        r
    }

    fn visit_block_expr(&mut self, expr: Pair<'_, Rule>) -> RegisterIndex {
        let mut expr = expr.into_inner().peekable();
        if expr.peek().is_none() {
            let r = self.context().allocate();
            self.push_instruction(Instruction::MakeLiteralObject(r, InstructionLiteral::Nil));
            return r;
        }
        self.context().enter();
        let level = self.context().save();
        let mut item;
        while {
            item = expr.next().unwrap();
            expr.peek().is_some()
        } {
            self.visit_stmt(item);
            self.context().restore(level);
        }
        let r = if item.as_rule() == Rule::ValueExpr {
            self.visit_expr(item)
        } else {
            self.visit_stmt(item);
            self.context().restore(level);
            let r = self.context().allocate();
            self.push_instruction(Instruction::MakeLiteralObject(r, InstructionLiteral::Nil));
            r
        };
        self.context().exit();
        r
    }

    fn visit_call_expr<'a>(
        &mut self,
        context_arguments: impl Iterator<Item = Pair<'a, Rule>>,
        mut name: &'a str,
        arguments: impl Iterator<Item = Pair<'a, Rule>>,
    ) -> RegisterIndex {
        let level = self.context().save();
        let mut context_arguments = context_arguments
            .map(|expr| {
                assert_eq!(expr.as_rule(), Rule::Expr);
                self.visit_expr(expr)
            })
            .collect::<Vec<_>>();
        if let [context, n] = *name.split('.').collect::<Vec<_>>() {
            assert!(context_arguments.is_empty());
            if let Some(r) = self.context().find(context) {
                context_arguments.push(r);
                name = n;
            }
        }
        let arguments = arguments
            .map(|expr| {
                assert_eq!(expr.as_rule(), Rule::Expr);
                self.visit_expr(expr)
            })
            .collect();
        self.context().restore(level);
        let r = self.context().allocate();
        self.push_instruction(Instruction::Call(
            r,
            context_arguments.into_boxed_slice(),
            name.to_owned(),
            arguments,
        ));
        r
    }

    fn visit_if_expr(&mut self, expr: Pair<'_, Rule>) -> RegisterIndex {
        todo!()
    }

    fn visit_stmt(&mut self, stmt: Pair<'_, Rule>) {
        match stmt.as_rule() {
            Rule::VarStmt => self.visit_var_stmt(stmt),
            Rule::AssignStmt => self.visit_assign_stmt(stmt),
            Rule::SetStmt => todo!(),
            Rule::AssertStmt => {
                let r = self.visit_expr(stmt.into_inner().next().unwrap());
                self.push_instruction(Instruction::Assert(r))
            }
            Rule::Expr => {
                self.visit_expr(stmt);
            }
            _ => unreachable!(),
        }
    }

    fn visit_var_stmt(&mut self, stmt: Pair<'_, Rule>) {
        let mut stmt = stmt.into_inner();
        let name = stmt.next().unwrap().as_str().to_owned();
        let r = if let Some(expr) = stmt.next() {
            self.visit_assign_expr(expr)
        } else {
            let r = self.context().allocate();
            self.push_instruction(Instruction::MakeLiteralObject(r, InstructionLiteral::Nil));
            r
        };
        self.context().bind(name, r);
    }

    fn visit_assign_stmt(&mut self, stmt: Pair<'_, Rule>) {
        let mut stmt = stmt.into_inner();
        let name = stmt.next().unwrap().as_str();
        let expr = stmt.next().unwrap();
        let r = self.visit_assign_expr(expr);
        if !self.context().find_update(name, r) {
            todo!() // update global
        }
    }

    fn visit_assign_expr(&mut self, expr: Pair<'_, Rule>) -> RegisterIndex {
        let r = self.context().save();
        let r1 = self.visit_expr(expr);
        if r1 < r {
            self.context().restore(r);
            r1
        } else {
            self.context().restore_allocate(r);
            self.push_instruction(Instruction::Operator1(r, Operator1::Copy, r1));
            r
        }
    }
}

impl StmtVisitor for ProgramVisitor {
    fn push_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction)
    }

    fn context(&mut self) -> &mut Context {
        &mut self.context
    }
}

impl StmtVisitor for FunctionVisitor {
    fn push_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction)
    }

    fn context(&mut self) -> &mut Context {
        &mut self.context
    }
}
