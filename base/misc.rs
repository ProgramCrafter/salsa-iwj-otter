// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// This is in this crate for convenience, not because it's to do with
// Z coordinates.

use crate::prelude::*;

pub fn timestring_abbreviate<'x>(base: &str, this: &'x str)
                                 -> (&'x str, bool)
{
  fn split(s: &str) -> ArrayVec<[&str; 3]> {
    s.splitn(3, ' ').collect()
  }
  let base3 = split(base);
  let this3 = split(this);
  let matches = |i| base3.get(i) == this3.get(i);
  if_chain! {
    if matches(0) && matches(2);
    if let Some(abbrev) = this3.get(1);
    then { (abbrev, true) }
    else { (this, false) }
  }
}

pub fn raw_angle_transform(compass: u8) -> String {
  assert!(compass < 8);
  if compass == 0 { default() }
  else { format!("rotate({})", -45 * (compass as i16)) }
}

pub fn default<T:Default>() -> T { Default::default() }

#[macro_export]
macro_rules! display_as_debug {
  {$x:ty $( , $($gen_tt:tt)* )?} => {
    impl $( $($gen_tt)* )? std::fmt::Display for $x {
      #[throws(std::fmt::Error)]
      fn fmt(&self, f: &mut std::fmt::Formatter) {
        <Self as Debug>::fmt(self, f)?
      }
    }
  }
}
pub use crate::display_as_debug;
