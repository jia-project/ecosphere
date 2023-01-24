#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub instructions: Box<[Instruction]>,
    pub register_level: RegisterIndex,
}

pub type RegisterIndex = u32;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction<D = RegisterIndex, U = RegisterIndex> {
    Define(D, Value<U>),
    Effect(Effect<U>),
    ParsePlaceholder(crate::grammar::Placeholder),
    OptimizePlaceholder,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value<U = RegisterIndex> {
    // "Operator0"
    MakeLiteralObject(Literal),
    Load(Box<str>),

    Operator1(Operator1, U),

    Operator2(Operator2, U, U),

    // "OperatorN"
    Phi(Box<[U]>),
    // context arguments, name, arguments
    Call(Box<[U]>, Box<str>, Box<[U]>),
    MakeTypedObject(Box<str>, Box<[(Box<str>, U)]>),
    // optimized instructions
    // AsOrJump(i, x, variant, target) ~>
    //   Is(t, x, variant); Jump(t, <fall through>, target); As(i, x, variant)
}

#[derive(Debug, Clone, PartialEq, Eq)]
// default `J` = (instruction offset, phi selector)
pub enum Effect<U, J = (usize, usize)> {
    MakeType(Box<str>, TypeOperator, Box<[Box<str>]>),
    // context type names, name, argument number, instructions
    MakeFunction(Box<[Box<str>]>, Box<str>, usize, Module),
    Jump(U, J, J),
    Return(U),
    Put(U, Box<str>, U),
    Inspect(U, Box<str>),
    Assert(U, Box<str>),
    Store(U, Box<str>),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator1 {
    Copy,
    Get(Box<str>),
    Is(Box<str>),
    As(Box<str>),
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
