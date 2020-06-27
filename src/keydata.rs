
use crate::imports::*;

type SKD = slotmap::KeyData;

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
pub fn slotkey_parse(s : &str, sep : char) -> SKD {
  let e = || anyhow!("could not deserialise visibile piece id");
  let mut i = s.splitn(2,sep).map(|s| s.parse().map_err(|_| e()));
  let h : u32 = i.next().ok_or_else(e)??;
  let l : u32 = i.next().ok_or_else(e)??;
  let v = ((h as u64) << 32) | (l as u64);
  SKD::from_ffi(v)
}

#[throws(fmt::Error)]
pub fn slotkey_write(k : SKD, sep : char, f : &mut fmt::Formatter) {
  let v = k.as_ffi();
  write!(f, "{}{}{}", v >> 32, sep, v & 0xffffffff)?
}

#[macro_export]
macro_rules! visible_slotmap_key {
  ( $x:ident ) => {
    #[derive(Copy,Clone,Serialize,Deserialize)]
    #[serde(into="String")]
    #[serde(try_from="&str")]
    pub struct $x(pub slotmap::KeyData);

    impl Display for $x {
      #[throws(fmt::Error)]
      fn fmt(&self, f : &mut fmt::Formatter) { slotkey_write(self.0,'.',f)? }
    }

    impl TryFrom<&str> for VisiblePieceId {
      type Error = AE;
      #[throws(AE)]
      fn try_from(s : &str) -> VisiblePieceId {
        VisiblePieceId(slotkey_parse(s,'.')?)
      }
    }
  }
}
pub use crate::visible_slotmap_key; // this is madness!
