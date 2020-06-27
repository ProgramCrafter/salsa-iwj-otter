
#[macro_export]
macro_rules! display_consequential_impls {
  ( $x:path ) => {
    impl From<$x> for String {
      fn from(p : $x) -> String { format!("{}",p) }
    }
    impl Debug for $x {
      fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        <Self as Display>::fmt(self, f)
      }
    }
  }
}

pub use crate::display_consequential_impls; // this is madness!
