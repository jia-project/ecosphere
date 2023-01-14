pub mod arena;
pub mod eval;
pub mod grammar;

pub type RegisterIndex = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    MakeType(Box<str>, TypeOperator, Box<[Box<str>]>),
    // context type names, name, argument number, instructions
    MakeFunction(Box<[Box<str>]>, Box<str>, usize, Box<[Instruction]>),

    Jump(RegisterIndex, usize, usize),
    Return(RegisterIndex),
    Call(
        RegisterIndex,
        Box<[RegisterIndex]>, // context object(s)
        Box<str>,             // name
        Box<[RegisterIndex]>, // argument object(s)
    ),

    // intrinsic operations
    MakeLiteralObject(RegisterIndex, InstructionLiteral),
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
    Assert(RegisterIndex),
    Store(RegisterIndex, Box<str>),

    // optimized instructions
    // AsOrJump(i, x, variant, target) ~>
    //   Is(t, x, variant); JumpUnless(t, target); As(i, x, variant)
    ParsingPlaceholder(grammar::Placeholder),
}

// must be embedded in bytecode directly because parser emitts these
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstructionLiteral {
    Nil,
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

pub type TypeIndex = u32;

// TODO reduce object size to 16 bytes if possible
// potential layout:
// * 60 bits pointer without trailing zeros,
// * 4 bits tag
// * + 32 bits type index * 32 bits item count
//   + 64 bits length (for Box<str> and Box<[_]>)
//   + 64 bits virtual table pointer (for Box<dyn _>)
//   + 64 bits native
#[derive(Default)]
pub struct Object {
    // bits: std::sync::atomic::AtomicU8,
    data: ObjectData,
}

#[derive(Default)]
pub enum ObjectData {
    #[default]
    Vacant,
    Forwarded(std::ptr::NonNull<Object>),

    Integer(i64),
    // float
    String(Box<str>),
    Array(Box<[std::ptr::NonNull<Object>]>),
    Typed(TypeIndex, Box<[std::ptr::NonNull<Object>]>),

    Any(Box<dyn ObjectAny>),
}

/// # Safety
/// `on_scan` must call `process` on every address that may be access in the future.
pub unsafe trait ObjectAny: std::any::Any {
    fn on_scan(&mut self, scanner: &mut arena::ObjectScanner<'_>);

    fn type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

pub trait CastData: Sized + 'static {
    fn cast_ref(data: &ObjectData) -> Option<&Self> {
        let ObjectData::Any(data) = data else {
            return None;
        };
        (data as &dyn std::any::Any).downcast_ref()
    }

    fn cast_mut(data: &mut ObjectData) -> Option<&mut Self> {
        let ObjectData::Any(data) = data else {
            return None;
        };
        (data as &mut dyn std::any::Any).downcast_mut()
    }
}

impl Object {
    pub fn cast_ref<T: CastData>(&self) -> Option<&T> {
        T::cast_ref(&self.data)
    }

    pub fn cast_mut<T: CastData>(&mut self) -> Option<&mut T> {
        T::cast_mut(&mut self.data)
    }
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
