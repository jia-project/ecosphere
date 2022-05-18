use std::{collections::HashMap, fmt::Display};

pub type DataId = u32;
pub type LayoutId = u32;

pub enum Obj {
    I32(i32),
    Str(String),
    Prod(LayoutId, Vec<DataId>),
    Sum(LayoutId, usize, DataId),
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I32(val) => write!(f, "i32 {val}"),
            Self::Str(val) => write!(f, "str"), // TODO
            Self::Prod(layout, data) => {
                if *layout == 0 {
                    write!(f, "unit")
                } else {
                    write!(f, "<prod#{layout} size={}", data.len())
                }
            }
            Self::Sum(layout, variant, ..) => {
                if *layout == 0 {
                    write!(f, "{}", if *variant == 0 { "true" } else { "false" })
                } else {
                    write!(f, "<sum#{layout} variant={variant}")
                }
            }
        }
    }
}

impl Obj {
    pub fn unit() -> Self {
        Self::Prod(0, Vec::new())
    }

    pub fn bool_true() -> Self {
        Self::Sum(0, 0, 0)
    }

    pub fn bool_false() -> Self {
        Self::Sum(0, 1, 0)
    }
}

pub struct Layout {
    name_table: HashMap<String, usize>,
    // tag
}
