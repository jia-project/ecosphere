pub mod instr;
pub mod interp;
pub mod loader;
pub mod mem;
pub mod worker;

pub mod basic {
    pub mod op;
    pub use op::Op;
    pub mod obj;
    pub mod parse;
}

use std::{
    any::Any,
    io::Write,
    ops::{Deref, DerefMut},
    str,
    time::Instant,
};

use crate::{instr::Val, interp::OpContext, mem::Obj};

pub type AssetId = u32;
pub type Name = str;
pub type OwnedName = <Name as ToOwned>::Owned;

/// # Safety
/// As long as all `*mut Obj` that accessible from arguments are valid and alive,
/// the returned pointer must point to valid and alive object.
pub unsafe trait Operator {
    fn perform(&mut self, code: &str, val: &[Val], context: &mut OpContext) -> Option<*mut Obj>;
}

pub trait AsAny {
    fn any_ref(&self) -> &dyn Any;
    fn any_mut(&mut self) -> &mut dyn Any;
}
impl<T: Any> AsAny for T {
    fn any_ref(&self) -> &dyn Any {
        self
    }
    fn any_mut(&mut self) -> &mut dyn Any {
        self
    }
}
impl Deref for dyn ObjCore {
    type Target = dyn Any;
    fn deref(&self) -> &Self::Target {
        self.any_ref()
    }
}
impl DerefMut for dyn ObjCore {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.any_mut()
    }
}

/// # Safety
/// While calling `trace`, all marked objects must be valid and alive, i.e.
/// must be returned by `Mutator::make`, and must be either root object or
/// traced in all previous collections.
pub unsafe trait ObjCore: AsAny {
    #[allow(unused_variables)]
    fn trace(&self, mark: &mut dyn FnMut(*mut Obj)) {}
    fn name(&self) -> &Name;
}

// consider make a util module
pub struct TraceOut<W> {
    write: W,
    t0: Instant,
}

impl<W> TraceOut<W> {
    pub fn new(write: W, t0: Instant) -> Self {
        Self { write, t0 }
    }
}

impl<W: Write> Write for TraceOut<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        write!(
            self.write,
            "[{:11.6}] {}",
            (Instant::now() - self.t0).as_secs_f64(),
            str::from_utf8(buf).unwrap()
        )
        .unwrap();
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
