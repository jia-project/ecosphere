pub mod arena;
pub mod eval;
pub mod grammar;
mod shared {
    pub mod instruction;
    pub mod object;
}
pub use shared::instruction::{self, Instruction, Module, RegisterIndex};
pub use shared::object::Object;

pub type TypeIndex = u32;
