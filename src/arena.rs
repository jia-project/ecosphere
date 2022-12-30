use crate::Object;

#[derive(Default)]
pub struct Arena; //

impl Arena {
    pub fn allocate(&self, object: Object) -> *mut Object {
        Box::leak(Box::new(object)) as _
    }

    pub fn read(&self, object: *mut Object) -> ArenaRead<'_> {
        ArenaRead(self)
    }

    pub fn write(&self, object: *mut Object) -> ArenaWrite<'_> {
        ArenaWrite(self)
    }
}

pub struct ArenaRead<'a>(&'a Arena);
pub struct ArenaWrite<'a>(&'a Arena);
