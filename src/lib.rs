pub mod arena;
pub mod eval;

pub type RegisterIndex = u8;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    MakeLiteralObject(RegisterIndex, InstructionLiteral),
    MakeProductTypeObject(RegisterIndex, String, Box<[(String, RegisterIndex)]>),
    MakeSumTypeObject(RegisterIndex, String, String, RegisterIndex),

    JumpIf(RegisterIndex, i32), // jump 0 to the following instruction
    Return(RegisterIndex),
    Call(
        RegisterIndex,
        Box<[RegisterIndex]>, // context object(s)
        String,               // name
        Box<[RegisterIndex]>, // argument object(s)
    ),
    ProductObjectGet(RegisterIndex, RegisterIndex, String),
    ProductObjectSet(RegisterIndex, String, RegisterIndex),
    // sum type intrinsic
    Operator1(RegisterIndex, Operator1, RegisterIndex),
    Operator2(RegisterIndex, Operator2, RegisterIndex, RegisterIndex),

    MakeProductType(String, Box<[String]>),
    MakeSumType(String, Box<[String]>),
    // context type names, name, argument number, instructions
    MakeFunction(Box<[String]>, String, usize, Box<[Instruction]>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstructionLiteral {
    Integer(i64),
    // float
    String(String),
}

pub type TypeIndex = u32;

pub enum Object {
    Integer(i64),
    // float
    String(String),
    Product(TypeIndex, Box<[*mut Object]>),
    Sum(TypeIndex, u8, *mut Object),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator1 {
    //
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator2 {
    Add,
}
