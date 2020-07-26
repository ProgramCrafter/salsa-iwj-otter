
pub trait OrdExt : Ord + Sized {
  fn update_max(&mut self, new: Self) {
    if new > *self { *self = new }
  }
}
impl<T> OrdExt for T where T : Ord + Sized {
}
