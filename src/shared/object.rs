use crate::TypeIndex;

#[derive(Clone, Copy)]
pub union Object0 {
    pub p: usize,
    pub i: i64,
    pub f: f64,
}

pub struct Object(pub Object0, pub TypeIndex, pub u32);

impl Object {
    pub const VACANT: TypeIndex = 0;
    pub const USED_SLAB: TypeIndex = 0xffffffff;
    pub const FORWARDED: TypeIndex = 0xfffffffe;
}

impl Default for Object {
    fn default() -> Self {
        Self(Object0 { p: 0 }, Self::VACANT, 0)
    }
}
