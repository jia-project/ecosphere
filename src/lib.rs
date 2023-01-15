pub mod arena;
pub mod eval;
pub mod grammar;
mod shared {
    pub mod instruction;
    pub mod object;
}
pub use shared::instruction::{self, Instruction, RegisterIndex};
pub use shared::object::{CastData, Object, ObjectAny, ObjectData};

pub type TypeIndex = u32;
