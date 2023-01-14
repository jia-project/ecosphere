use std::{collections::HashMap, iter::repeat_with};

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
    let mut visitor = ProgramVisitor(FunctionVisitor::default());
    visitor.0.enter();
    for pair in Parser::parse(Rule::Program, input).unwrap() {
        visitor.visit_program_stmt(pair)
    }
    visitor.0.exit();
    visitor
        .0
        .instructions
        .push(Instruction::MakeLiteralObject(0, InstructionLiteral::Nil));
    visitor.0.instructions.push(Instruction::Return(0));
    visitor.0.instructions.into()
}

struct ProgramVisitor(FunctionVisitor);

enum Expr<'a> {
    Primary(Pair<'a, Rule>),
    Operator1(Pair<'a, Rule>, Box<Expr<'a>>),
    Operator2(Pair<'a, Rule>, Box<Expr<'a>>, Box<Expr<'a>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Placeholder {
    // Jump(RegisterIndex),
    JumpIf(RegisterIndex),     // fallthrough on negative
    JumpUnless(RegisterIndex), // fallthrough on positive
    JumpContinue,
    JumpBreak,
    JumpEndIf,
}

enum ControlScope {
    If,
    While,
    Shortcut,
}

#[derive(Default)]
struct FunctionVisitor {
    instructions: Vec<Instruction>,
    scopes: Vec<HashMap<String, RegisterIndex>>,
    register_level: RegisterIndex,
    control_scopes: Vec<(ControlScope, Vec<usize>)>,
}

fn split<const N: usize>(pair: Pair<'_, Rule>) -> [Pair<'_, Rule>; N] {
    pair.into_inner().collect::<Vec<_>>().try_into().unwrap()
}

fn split_option<const N: usize>(
    pair: Pair<'_, Rule>,
) -> ([Pair<'_, Rule>; N], Option<Pair<'_, Rule>>) {
    let mut pairs = pair.into_inner();
    let fixed = repeat_with(|| pairs.next().unwrap())
        .take(N)
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();
    (fixed, pairs.next())
}

impl ProgramVisitor {
    fn visit_program_stmt(&mut self, stmt: Pair<'_, Rule>) {
        match stmt.as_rule() {
            Rule::MakeFunctionStmt => self.visit_make_function_stmt(stmt),
            Rule::MakeTypeStmt => self.visit_make_type_stmt(stmt),
            Rule::EOI => {}
            _ => self.0.visit_stmt(stmt),
        }
    }

    fn visit_make_function_stmt(&mut self, stmt: Pair<'_, Rule>) {
        // TODO context parameters
        let [dispatch, parameters, expr] = split(stmt);
        let mut visitor = FunctionVisitor::default();
        visitor.enter();

        let mut context_parameters = Vec::new();
        let name = match dispatch.as_rule() {
            Rule::PublicIdent => dispatch,
            Rule::ContextAndName => {
                let [parameters, name] = split(dispatch);
                for parameter in parameters.into_inner() {
                    let [type_name, name] = split(parameter);
                    context_parameters.push(type_name.as_str().into());
                    let r = visitor.allocate();
                    visitor.bind(name.as_str().to_owned(), r);
                }
                name
            }
            _ => unreachable!(),
        };

        let arity = parameters
            .into_inner()
            .map(|name| {
                let r = visitor.allocate();
                visitor.bind(name.as_str().to_owned(), r);
            })
            .count();
        let r = visitor.visit_block_expr(expr);
        visitor.exit();
        // patch a `return;` if function is not ending with one
        if !matches!(visitor.instructions.last(), Some(Instruction::Return(_))) {
            visitor.instructions.push(Instruction::Return(r));
        }

        self.0.instructions.push(Instruction::MakeFunction(
            context_parameters.into(),
            name.as_str().into(),
            arity,
            visitor.instructions.into(),
        ))
    }

    fn visit_make_type_stmt(&mut self, stmt: Pair<'_, Rule>) {
        let [name, items] = split(stmt);

        fn visit(visitor: &mut ProgramVisitor, name: Box<str>, items: Pair<'_, Rule>) {
            let type_op = match items.as_rule() {
                Rule::ProductTypeItems => TypeOperator::Product,
                Rule::SumTypeItems => TypeOperator::Sum,
                _ => unreachable!(),
            };
            let mut item_names = Vec::new();
            for item in items.into_inner() {
                let mut item = item.into_inner();
                let item_name = item.next().unwrap().as_str();
                item_names.push(item_name.into());
                if let Some(items) = item.next() {
                    visit(
                        visitor,
                        (String::from(&*name) + "." + item_name).into(),
                        items,
                    )
                }
            }
            visitor
                .0
                .instructions
                .push(Instruction::MakeType(name, type_op, item_names.into()))
        }
        visit(self, name.as_str().into(), items)
    }
}

impl FunctionVisitor {
    fn allocate(&mut self) -> RegisterIndex {
        let r = self.register_level;
        self.register_level += 1;
        r
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

    fn enter_control(&mut self, control_type: ControlScope) {
        self.control_scopes.push((control_type, Default::default()))
    }

    fn push_control(&mut self, placeholder: Placeholder) {
        let index = self.instructions.len();
        self.instructions
            .push(Instruction::ParsingPlaceholder(placeholder));
        let mut scope = self.control_scopes.last_mut().unwrap();
        'find: {
            if matches!(
                placeholder,
                Placeholder::JumpBreak | Placeholder::JumpContinue
            ) {
                for s in self.control_scopes.iter_mut().rev() {
                    if matches!(s.0, ControlScope::While) {
                        scope = s;
                        break 'find;
                    }
                }
                panic!()
            }
        }
        scope.1.push(index)
    }

    fn exit_control(&mut self, positive: usize, negative: usize) {
        let (control_type, indexes) = self.control_scopes.pop().unwrap();
        for index in indexes {
            use Instruction::Jump as J;
            use Instruction::ParsingPlaceholder as P;
            use Placeholder::*;
            let check = |target: usize| {
                assert_ne!(target, usize::MAX);
                target
            };
            let instruction = match self.instructions[index].clone() {
                // P(Jump(r)) => J(r, check(positive), check(negative)),
                P(JumpIf(r)) => J(r, check(positive), index + 1),
                P(JumpUnless(r)) => J(r, index + 1, check(negative)),
                P(JumpContinue) => {
                    assert!(matches!(control_type, ControlScope::While));
                    J(RegisterIndex::MAX, check(positive), positive)
                }
                P(JumpBreak) => {
                    assert!(matches!(control_type, ControlScope::While));
                    J(RegisterIndex::MAX, check(negative), negative)
                }
                P(JumpEndIf) => {
                    assert!(matches!(control_type, ControlScope::If));
                    J(RegisterIndex::MAX, check(positive), positive)
                }
                _ => unreachable!(),
            };
            self.instructions[index] = instruction
        }
    }

    fn visit_expr(&mut self, expr: Pair<'_, Rule>) -> RegisterIndex {
        let expr = PrattParser::new()
            .op(Op::prefix(Rule::Inspect))
            .op(Op::infix(Rule::Or, Assoc::Left))
            .op(Op::infix(Rule::And, Assoc::Left))
            .op(Op::prefix(Rule::Not))
            .op(Op::postfix(Rule::Is))
            .op(Op::infix(Rule::Lt, Assoc::Left)
                | Op::infix(Rule::Gt, Assoc::Left)
                | Op::infix(Rule::Le, Assoc::Left)
                | Op::infix(Rule::Ge, Assoc::Left)
                | Op::infix(Rule::Eq, Assoc::Left)
                | Op::infix(Rule::Ne, Assoc::Left))
            .op(Op::infix(Rule::BitOr, Assoc::Left))
            .op(Op::infix(Rule::BitXor, Assoc::Left))
            .op(Op::infix(Rule::BitAnd, Assoc::Left))
            .op(Op::infix(Rule::Shl, Assoc::Left) | Op::infix(Rule::Shr, Assoc::Left))
            .op(Op::infix(Rule::Add, Assoc::Left) | Op::infix(Rule::Sub, Assoc::Left))
            .op(Op::infix(Rule::Mul, Assoc::Left)
                | Op::infix(Rule::Div, Assoc::Left)
                | Op::infix(Rule::Rem, Assoc::Left))
            .op(Op::prefix(Rule::Neg))
            .op(Op::postfix(Rule::Dot) | Op::postfix(Rule::As) | Op::postfix(Rule::Call1))
            .map_primary(Expr::Primary)
            .map_infix(|lhs, op, rhs| Expr::Operator2(op, Box::new(lhs), Box::new(rhs)))
            .map_prefix(|op, rhs| Expr::Operator1(op, Box::new(rhs)))
            .map_postfix(|lhs, op| Expr::Operator1(op, Box::new(lhs)))
            .parse(expr.into_inner());
        self.visit_expr_internal(expr)
    }

    fn visit_expr_internal(&mut self, expr: Expr<'_>) -> RegisterIndex {
        match expr {
            Expr::Primary(expr) => self.visit_primary_expr(expr),
            Expr::Operator2(op, expr1, expr2) => {
                match op.as_rule() {
                    Rule::And => return self.visit_and_expr(*expr1, *expr2),
                    Rule::Or => return self.visit_or_expr(*expr1, *expr2),
                    _ => {}
                }

                let r1 = self.visit_expr_internal(*expr1);
                let r2 = self.visit_expr_internal(*expr2);
                let op = match op.as_rule() {
                    Rule::Add => Operator2::Add,
                    Rule::Sub => Operator2::Sub,
                    Rule::Mul => Operator2::Mul,
                    Rule::Div => Operator2::Div,
                    Rule::Rem => Operator2::Rem,
                    Rule::Lt => Operator2::Lt,
                    Rule::Gt => Operator2::Gt,
                    Rule::Eq => Operator2::Eq,
                    Rule::Ne => Operator2::Ne,
                    Rule::Le => Operator2::Le,
                    Rule::Ge => Operator2::Ge,
                    Rule::BitAnd => Operator2::BitAnd,
                    Rule::BitOr => Operator2::BitOr,
                    Rule::BitXor => Operator2::BitXor,
                    Rule::Shl => Operator2::Shl,
                    Rule::Shr => Operator2::Shr,
                    _ => unreachable!(),
                };
                let r = self.allocate();
                self.instructions
                    .push(Instruction::Operator2(r, op, r1, r2));
                r
            }
            Expr::Operator1(op, expr) => {
                let r1 = self.visit_expr_internal(*expr);
                match op.as_rule() {
                    Rule::Inspect => {
                        self.instructions.push(Instruction::Inspect(
                            r1,
                            op.as_span().lines().collect::<Vec<_>>().join("\n").into(),
                        ));
                        return r1;
                    }
                    Rule::Call1 => {
                        let [name, arguments] = split(op);
                        let arguments = arguments
                            .into_inner()
                            .map(|argument| self.visit_expr(argument))
                            .collect();
                        let r = self.allocate();
                        self.instructions.push(Instruction::Call(
                            r,
                            Box::new([r1]),
                            name.as_str().into(),
                            arguments,
                        ));
                        return r;
                    }
                    _ => {}
                }

                let r = self.allocate();
                match op.as_rule() {
                    Rule::Not => {
                        self.instructions
                            .push(Instruction::Operator1(r, Operator1::Not, r1));
                        r
                    }
                    Rule::Neg => {
                        self.instructions
                            .push(Instruction::Operator1(r, Operator1::Neg, r1));
                        r
                    }
                    Rule::Dot => {
                        let component = split::<1>(op)[0].as_str().into();
                        self.instructions.push(Instruction::Get(r, r1, component));
                        r
                    }
                    Rule::As => {
                        let variant = split::<1>(op)[0].as_str().into();
                        self.instructions.push(Instruction::As(r, r1, variant));
                        r
                    }
                    Rule::Is => {
                        let ([variant], name) = split_option(op);
                        let variant = <Box<str>>::from(variant.as_str());
                        self.instructions
                            .push(Instruction::Is(r, r1, variant.clone()));
                        if let Some(name) = name {
                            // TODO check name binding can only be used in control flow expression
                            let name = name.as_str().into();
                            // if expr is positive, fallthrough to next instruction in bind the name
                            // otherwise, skip the next instruction
                            let index = self.instructions.len();
                            self.instructions
                                .push(Instruction::Jump(r, index + 1, index + 2));
                            let as_r = self.allocate();
                            self.instructions.push(Instruction::As(as_r, r1, variant));
                            self.bind(name, as_r);
                        }
                        r
                    }
                    Rule::Match => todo!(),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn visit_shortcut(
        &mut self,
        expr1: Expr<'_>,
        expr2: Expr<'_>,
        placeholder: impl Fn(RegisterIndex) -> Placeholder,
        after_all: bool,
    ) -> RegisterIndex {
        let r = self.allocate();
        self.instructions.push(Instruction::MakeLiteralObject(
            r,
            InstructionLiteral::Bool(!after_all),
        ));
        self.enter_control(ControlScope::Shortcut);
        let r1 = self.visit_expr_internal(expr1);
        self.push_control(placeholder(r1));
        let r2 = self.visit_expr_internal(expr2);
        self.push_control(placeholder(r2));
        self.instructions.push(Instruction::MakeLiteralObject(
            r,
            InstructionLiteral::Bool(after_all),
        ));
        // control exit in caller
        r
    }

    fn visit_and_expr(&mut self, expr1: Expr<'_>, expr2: Expr<'_>) -> RegisterIndex {
        let r = self.visit_shortcut(expr1, expr2, Placeholder::JumpUnless, true);
        self.exit_control(usize::MAX, self.instructions.len());
        r
    }

    fn visit_or_expr(&mut self, expr1: Expr<'_>, expr2: Expr<'_>) -> RegisterIndex {
        let r = self.visit_shortcut(expr1, expr2, Placeholder::JumpIf, false);
        self.exit_control(self.instructions.len(), usize::MAX);
        r
    }

    fn visit_primary_expr(&mut self, expr: Pair<'_, Rule>) -> RegisterIndex {
        match expr.as_rule() {
            Rule::Expr => self.visit_expr(expr),
            Rule::Ident => {
                if let Some(r) = self.find(expr.as_str()) {
                    r
                } else {
                    let r = self.allocate();
                    self.instructions
                        .push(Instruction::Load(r, expr.as_str().into()));
                    r
                }
            }
            Rule::Integer => {
                let r = self.allocate();
                self.instructions.push(Instruction::MakeLiteralObject(
                    r,
                    InstructionLiteral::Integer(expr.as_str().parse().unwrap()),
                ));
                r
            }
            Rule::String => {
                // TODO handle escaping properly
                let value = expr.as_str()[1..expr.as_str().len() - 1].into();
                let r = self.allocate();
                self.instructions.push(Instruction::MakeLiteralObject(
                    r,
                    InstructionLiteral::String(value),
                ));
                r
            }
            Rule::Float => todo!(),
            Rule::DataExpr => self.visit_data_expr(expr),
            Rule::BlockExpr => self.visit_block_expr(expr),
            Rule::Call0Expr => {
                let [name, arguments] = split(expr);
                self.visit_call_expr([].into_iter(), name.as_str(), arguments.into_inner())
            }
            Rule::CallNExpr => {
                let [context, name, arguments] = split(expr);
                self.visit_call_expr(context.into_inner(), name.as_str(), arguments.into_inner())
            }
            Rule::IfExpr => self.visit_if_expr(expr),
            _ => unreachable!(),
        }
    }

    fn visit_data_expr(&mut self, expr: Pair<'_, Rule>) -> RegisterIndex {
        let [name, items] = split(expr);
        self.visit_data_items(name.as_str().into(), items)
    }

    fn visit_data_items(&mut self, name: Box<str>, items: Pair<'_, Rule>) -> RegisterIndex {
        // fieldless sum type variant
        if items.as_rule() == Rule::Ident {
            let r = self.allocate();
            self.instructions
                .push(Instruction::MakeLiteralObject(r, InstructionLiteral::Nil));
            self.instructions.push(Instruction::MakeTypedObject(
                r,
                name,
                Box::new([(items.as_str().into(), r)]),
            ));
            return r;
        }
        let data = items
            .into_inner()
            .map(|item| {
                let [item_name, value] = split(item);
                let item_name = item_name.as_str();
                (
                    item_name.into(),
                    match value.as_rule() {
                        Rule::Expr => self.visit_expr(value),
                        Rule::DataItems => {
                            self.visit_data_items((String::from(&*name) + item_name).into(), value)
                        }
                        _ => unreachable!(),
                    },
                )
            })
            .collect();
        let r = self.allocate();
        self.instructions
            .push(Instruction::MakeTypedObject(r, name, data));
        r
    }

    fn visit_block_expr(&mut self, expr: Pair<'_, Rule>) -> RegisterIndex {
        let ([stmts], expr) = split_option(expr);
        self.enter();
        for stmt in stmts.into_inner() {
            self.visit_stmt(stmt);
        }
        let r = if let Some(expr) = expr {
            self.visit_expr(expr)
        } else {
            let r = self.allocate();
            self.instructions
                .push(Instruction::MakeLiteralObject(r, InstructionLiteral::Nil));
            r
        };
        self.exit();
        r
    }

    fn visit_call_expr<'a>(
        &mut self,
        context_arguments: impl Iterator<Item = Pair<'a, Rule>>,
        mut name: &'a str,
        arguments: impl Iterator<Item = Pair<'a, Rule>>,
    ) -> RegisterIndex {
        let mut context_arguments = context_arguments
            .map(|expr| self.visit_expr(expr))
            .collect::<Vec<_>>();
        if let [context, n] = *name.split('.').collect::<Vec<_>>() {
            assert!(context_arguments.is_empty());
            if let Some(r) = self.find(context) {
                context_arguments.push(r);
                name = n;
            }
        }
        let arguments = arguments.map(|expr| self.visit_expr(expr)).collect();
        let r = self.allocate();
        self.instructions.push(Instruction::Call(
            r,
            context_arguments.into(),
            name.into(),
            arguments,
        ));
        r
    }

    fn visit_if_expr(&mut self, expr: Pair<'_, Rule>) -> RegisterIndex {
        let ([test, positive], negative) = split_option(expr);

        self.enter(); // scope for `is` expression in `test`
        self.enter_control(ControlScope::If);

        let r = self.visit_expr(test);
        // positive target: after else block, negative target: else block
        self.push_control(Placeholder::JumpUnless(r));

        let r = self.allocate();

        let positive_r = self.visit_block_expr(positive);
        self.instructions
            .push(Instruction::Operator1(r, Operator1::Copy, positive_r));
        self.push_control(Placeholder::JumpEndIf);

        let negative_index = self.instructions.len();
        if let Some(negative) = negative {
            let negative_r = self.visit_block_expr(negative);
            self.instructions
                .push(Instruction::Operator1(r, Operator1::Copy, negative_r))
        } else {
            self.instructions
                .push(Instruction::MakeLiteralObject(r, InstructionLiteral::Nil))
        }

        self.exit(); // `is` expression scope
        let positive_index = self.instructions.len();
        self.exit_control(positive_index, negative_index);
        r
    }

    fn visit_stmt(&mut self, stmt: Pair<'_, Rule>) {
        match stmt.as_rule() {
            Rule::VarStmt => self.visit_var_stmt(stmt),
            Rule::AssignStmt => {
                let [name, expr] = split(stmt);
                let expr_r = self.visit_expr(expr);
                let r = self.find(name.as_str()).unwrap();
                self.instructions
                    .push(Instruction::Operator1(r, Operator1::Copy, expr_r));
            }
            Rule::MakeAssignStmt => {
                let [name, expr] = split(stmt);
                let expr_r = self.visit_expr(expr);
                self.instructions
                    .push(Instruction::Store(expr_r, name.as_str().into()));
            }
            Rule::PutStmt => {
                let [data, component, expr] = split(stmt);
                let r = self.visit_expr(data);
                let expr_r = self.visit_expr(expr);
                self.instructions
                    .push(Instruction::Put(r, component.as_str().into(), expr_r));
            }
            Rule::AssertStmt => {
                let [expr] = split(stmt);
                let source = expr.as_str().into();
                let r = self.visit_expr(expr);
                self.instructions.push(Instruction::Assert(r, source));
            }
            Rule::WhileStmt => self.visit_while_stmt(stmt),
            Rule::BreakStmt => self.push_control(Placeholder::JumpBreak),
            Rule::ContinueStmt => self.push_control(Placeholder::JumpContinue),
            Rule::ReturnStmt => {
                let r = if let Some(expr) = split_option::<0>(stmt).1 {
                    self.visit_expr(expr)
                } else {
                    let r = self.allocate();
                    self.instructions
                        .push(Instruction::MakeLiteralObject(r, InstructionLiteral::Nil));
                    r
                };
                self.instructions.push(Instruction::Return(r));
            }
            Rule::Expr => {
                self.visit_expr(stmt);
            }
            Rule::IfExpr => {
                self.visit_if_expr(stmt);
            }
            _ => unreachable!(),
        }
    }

    fn visit_var_stmt(&mut self, stmt: Pair<'_, Rule>) {
        let ([name], expr) = split_option(stmt);
        let r;
        if let Some(expr) = expr {
            let expr_r = self.visit_expr(expr);
            r = self.allocate();
            self.instructions
                .push(Instruction::Operator1(r, Operator1::Copy, expr_r))
        } else {
            r = self.allocate();
            self.instructions
                .push(Instruction::MakeLiteralObject(r, InstructionLiteral::Nil))
        };
        self.bind(name.as_str().to_owned(), r)
    }

    fn visit_while_stmt(&mut self, stmt: Pair<'_, Rule>) {
        let [test, expr] = split(stmt);

        // scope for `is` expression in `test`
        self.enter();
        // positive target: before test expression, negative target: after block expression
        self.enter_control(ControlScope::While);
        let continue_index = self.instructions.len();

        let r = self.visit_expr(test);
        self.push_control(Placeholder::JumpUnless(r));
        self.visit_block_expr(expr);
        self.push_control(Placeholder::JumpContinue);

        self.exit(); // `is` expression scope
        let negative_index = self.instructions.len();
        self.exit_control(continue_index, negative_index)
    }
}
