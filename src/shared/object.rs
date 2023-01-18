use crate::{TypeIndex, arena::CollectContext};

#[derive(Clone, Copy)]
pub union Object0 {
    pub p: usize,
    pub p1: *mut Object,
    pub i: i64,
    pub f: f64,
}

pub struct Object(pub Object0, pub TypeIndex, pub u32);

impl Object {
    pub const VACANT: TypeIndex = 0;

    pub fn visit_nop(&self, _: &mut CollectContext<'_>) {}
    pub fn drop_nop(_: Self) {}
}

impl Default for Object {
    fn default() -> Self {
        Self(Object0 { p: 0 }, Self::VACANT, 0)
    }
}
