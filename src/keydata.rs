// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(clippy::many_single_char_names)]

use crate::imports::*;

type SKD = slotmap::KeyData;

#[macro_export]
macro_rules! display_consequential_impls {
  ( $x:path ) => {
    impl From<$x> for String {
      fn from(p: $x) -> String { format!("{}",p) }
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
pub fn slotkey_parse(s: &str, sep: char) -> SKD {
  let e = || anyhow!("could not deserialise visibile piece id");
  let mut i = s.splitn(2, sep).map(|s| s.parse().map_err(|_| e()));
  let l: u32 = i.next().ok_or_else(e)??;
  let h: u32 = i.next().ok_or_else(e)??;
  let v = ((h as u64) << 32) | (l as u64);
  SKD::from_ffi(v)
}

#[throws(fmt::Error)]
pub fn slotkey_write(k: SKD, sep: char, f: &mut fmt::Formatter) {
  let v = k.as_ffi();
  write!(f, "{}{}{}", v & 0xffffffff, sep, v >> 32)?
}

#[macro_export]
macro_rules! visible_slotmap_key {
  ( $x:ident($sep:expr) ) => {

    #[derive(Copy,Default,Clone,Eq,PartialEq,Ord,PartialOrd,Serialize,Deserialize,Hash)]
    #[serde(into="String")]
    #[serde(try_from="String")]
    pub struct $x(pub slotmap::KeyData);

    impl Display for $x {
      #[throws(fmt::Error)]
      fn fmt(&self, f: &mut fmt::Formatter) { slotkey_write(self.0,$sep,f)? }
    }

    impl TryFrom<&str> for $x {
      type Error = AE;
      #[throws(AE)]
      fn try_from(s: &str) -> $x { $x(slotkey_parse(s,$sep)?) }
    }
    impl TryFrom<String> for $x {
      type Error = AE;
      #[throws(AE)]
      fn try_from(s: String) -> $x { $x(slotkey_parse(&s,$sep)?) }
    }

    impl slotmap::Key for $x {
      fn data(&self) -> slotmap::KeyData { self.0 }
    }
    impl From<slotmap::KeyData> for $x {
      fn from(d: slotmap::KeyData) -> Self { $x(d) }
    }
    impl From<$x> for slotmap::KeyData {
      fn from(p: $x) -> Self {
        p.0
      }
    }

    display_consequential_impls!{$x}
  }
}
pub use crate::visible_slotmap_key; // this is madness!
