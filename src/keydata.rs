
use crate::imports::*;

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

#[throws(AE)]
pub fn slotkey_parse(s : &str) -> slotmap::KeyData {
  let e = || anyhow!("could not deserialise visibile piece id");
  let mut i = s.splitn(2,'.').map(|s| s.parse().map_err(|_| e()));
  let h : u32 = i.next().ok_or_else(e)??;
  let l : u32 = i.next().ok_or_else(e)??;
  let v = ((h as u64) << 32) | (l as u64);
  slotmap::KeyData::from_ffi(v)
}
