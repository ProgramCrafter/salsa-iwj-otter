
pub trait OrdExt : Ord + Sized {
  fn update_max(&mut self, new: Self) {
    if new > *self { *self = new }
  }
}
impl<T> OrdExt for T where T : Ord + Sized {
}

pub fn error_display<E: AsRef<dyn std::error::Error + 'static>>(err: &E) {
  for (e,i) in err.as_ref().chain().zip(0..) {
    eprintln!("error {:2}: {}", i,e);
  }
}
