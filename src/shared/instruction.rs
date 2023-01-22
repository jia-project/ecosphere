#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub instructions: Box<[Instruction]>,
    pub register_level: RegisterIndex,
}

pub type RegisterIndex = u32;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    MakeType(Box<str>, TypeOperator, Box<[Box<str>]>),
    // context type names, name, argument number, instructions
    MakeFunction(Box<[Box<str>]>, Box<str>, usize, Module),

    Jump(RegisterIndex, usize, usize),
    Return(RegisterIndex),
    Call(
        RegisterIndex,
        Box<[RegisterIndex]>, // context object(s)
        Box<str>,             // name
        Box<[RegisterIndex]>, // argument object(s)
    ),

    // intrinsic operations
    MakeLiteralObject(RegisterIndex, Literal),
    MakeTypedObject(RegisterIndex, Box<str>, Box<[(Box<str>, RegisterIndex)]>),
    Load(RegisterIndex, Box<str>),
    Get(RegisterIndex, RegisterIndex, Box<str>),
    Put(RegisterIndex, Box<str>, RegisterIndex),
    Is(RegisterIndex, RegisterIndex, Box<str>),
    As(RegisterIndex, RegisterIndex, Box<str>),
    // overloadable operations
    Operator1(RegisterIndex, Operator1, RegisterIndex),
    Operator2(RegisterIndex, Operator2, RegisterIndex, RegisterIndex),

    // side effects
    Inspect(RegisterIndex, Box<str>),
    Assert(RegisterIndex, Box<str>),
    Store(RegisterIndex, Box<str>),

    // optimized instructions
    // AsOrJump(i, x, variant, target) ~>
    //   Is(t, x, variant); JumpUnless(t, target); As(i, x, variant)
    ParsingPlaceholder(crate::grammar::Placeholder),
}

// must be embedded in bytecode directly because parser emitts these
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Unit,
    Bool(bool),
    Integer(i64),
    // float
    String(Box<str>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeOperator {
    Product,
    Sum,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator1 {
    Copy,
    Not,
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator2 {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Lt,
    Gt,
    Eq,
    Ne,
    Le,
    Ge,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}
