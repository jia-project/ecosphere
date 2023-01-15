use crate::{TypeIndex, arena};

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
    pub data: ObjectData,
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

    pub fn type_name(&self) -> &str {
        match &self.data {
            ObjectData::Vacant => "(Vacant)",
            ObjectData::Forwarded(_) => "(Forwarded)",
            ObjectData::Integer(_) => "Integer",
            ObjectData::String(_) => "String",
            ObjectData::Array(_) => "Array",
            ObjectData::Typed(..) => "Typed",
            ObjectData::Any(object) => object.type_name(),
        }
    }
}
