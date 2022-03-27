
use crate::prelude::*;

#[derive(Debug, Default)]
pub struct Mutex<T>(parking_lot::Mutex<T>);

#[derive(Debug, Display)]
pub struct MutexGuard<'g, T>(parking_lot::MutexGuard<'g, T>);

impl<T> Mutex<T> {
  pub fn new(t: T) -> Self {
    Mutex(parking_lot::Mutex::new(t))
  }
  pub fn lock(&self) -> MutexGuard<T> {
    MutexGuard(self.0.lock())
  }
}

impl<'g,T> Deref for MutexGuard<'g,T> {
  type Target = T;
  fn deref(&self) -> &T { &*self.0 }
}
impl<'g,T> DerefMut for MutexGuard<'g,T> {
  fn deref_mut(&mut self) -> &mut T { &mut *self.0 }
}
