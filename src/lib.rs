pub mod arena;
pub mod eval;
pub mod parse;
pub use parse::parse;

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

    Inspect(RegisterIndex),
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

#[derive(Default)]
pub struct Object {
    // bits: std::sync::atomic::AtomicU8,
    data: ObjectData,
}

#[derive(Default)]
pub enum ObjectData {
    #[default]
    Vacant,
    Forwarded(*mut Object),

    Integer(i64),
    // float
    String(String),
    // vector
    Product(TypeIndex, Box<[*mut Object]>),
    Sum(TypeIndex, u8, *mut Object),
    Any(Box<dyn ObjectAny>),
}

pub trait ObjectAny: std::any::Any {
    fn on_scan(&self, scanner: arena::ObjectScanner<'_>);

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
    //
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator2 {
    Add,
}
