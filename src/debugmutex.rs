
use crate::prelude::*;

#[derive(Debug, Default)]
pub struct Mutex<T>(parking_lot::Mutex<T>);

#[derive(Debug, Display)]
pub struct MutexGuard<'g, T>(parking_lot::MutexGuard<'g, T>)
where T: DebugIdentify;

impl<T> Mutex<T> where T: DebugIdentify {
  pub fn new(t: T) -> Self {
    Mutex(parking_lot::Mutex::new(t))
  }
  pub fn lock(&self) -> MutexGuard<T> {
    MutexGuard(self.0.lock())
  }
}

impl<'g,T> Deref for MutexGuard<'g,T> where T: DebugIdentify {
  type Target = T;
  fn deref(&self) -> &T { &*self.0 }
}
impl<'g,T> DerefMut for MutexGuard<'g,T> where T: DebugIdentify {
  fn deref_mut(&mut self) -> &mut T { &mut *self.0 }
}

pub struct DisplayFormatter<F>(F);
impl<F> Display for DisplayFormatter<F>
where F: Fn(&mut fmt::Formatter) -> fmt::Result {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.0(f) }
}

pub struct DisplayDebugIdentify<'t,T>(&'t T) where T: DebugIdentify;
impl<T> Display for DisplayDebugIdentify<'_,T> where T: DebugIdentify {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.0.debug_identify(f)
  }
}

pub trait DebugIdentify {
  fn debug_identify_type(f: &mut fmt::Formatter) -> fmt::Result;
  fn debug_identify(&self, f: &mut fmt::Formatter) -> fmt::Result {
    Self::debug_identify_type(f)?;
    write!(f, "({:?})", self as *const _)?;
    Ok(())
  }
}

impl<T> DebugIdentify for Option<T> where T: DebugIdentify {
  #[throws(fmt::Error)]
  fn debug_identify_type(f: &mut fmt::Formatter) {
    write!(f, "Option<{}>", DisplayFormatter(T::debug_identify_type))?;
  }

  #[throws(fmt::Error)]
  fn debug_identify(&self, f: &mut fmt::Formatter) {
    match self {
      None => write!(f, "None<{}>", DisplayFormatter(T::debug_identify_type))?,
      Some(t) => t.debug_identify(f)?,
    }
  }
}

impl<T> DebugIdentify for VecDeque<T> where T: DebugIdentify {
  #[throws(fmt::Error)]
  fn debug_identify_type(f: &mut fmt::Formatter) {
    write!(f, "VecDeque<{}>", DisplayFormatter(T::debug_identify_type))?;
  }
}

impl DebugIdentify for File{
  #[throws(fmt::Error)]
  fn debug_identify_type(f: &mut fmt::Formatter) {
    write!(f, "File")?;
  }

  #[throws(fmt::Error)]
  fn debug_identify(&self, f: &mut fmt::Formatter) {
    write!(f, "File({})", self.as_raw_fd())?;
  }
}
