pub mod arena;
pub mod eval;
pub mod grammar;

pub type RegisterIndex = u8;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    MakeLiteralObject(RegisterIndex, InstructionLiteral),
    MakeDataObject(RegisterIndex, String, Box<[(String, RegisterIndex)]>),

    Jump(RegisterIndex, usize, usize),
    Return(RegisterIndex),
    Call(
        RegisterIndex,
        Box<[RegisterIndex]>, // context object(s)
        String,               // name
        Box<[RegisterIndex]>, // argument object(s)
    ),

    Inspect(RegisterIndex),
    Assert(RegisterIndex),
    Load(RegisterIndex, String),
    Store(String, RegisterIndex),
    Get(RegisterIndex, RegisterIndex, String),
    Set(RegisterIndex, String, RegisterIndex),
    Is(RegisterIndex, RegisterIndex, String),
    As(RegisterIndex, RegisterIndex, String),
    // overloadable operations
    Operator1(RegisterIndex, Operator1, RegisterIndex),
    Operator2(RegisterIndex, Operator2, RegisterIndex, RegisterIndex),

    MakeType(String, TypeOperator, Box<[String]>),
    // context type names, name, argument number, instructions
    MakeFunction(Box<[String]>, String, usize, Box<[Instruction]>),
    //
    // AsOrJump(i, x, variant, target) ~>
    //   Is(t, x, variant); JumpUnless(t, target); As(i, x, variant)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstructionLiteral {
    Nil,
    Integer(i64),
    // float
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeOperator {
    Product,
    Sum,
}

pub type TypeIndex = u32;

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
    String(String),
    // vector
    Data(TypeIndex, Box<[std::ptr::NonNull<Object>]>),

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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator2 {
    Add,
    Sub,
    And,
    Or,
    LessThan,
    GreaterThan,
    Equal,
}
