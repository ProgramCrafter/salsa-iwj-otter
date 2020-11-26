// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// This is in this crate for convenience, not because it's to do with
// Z coordinates.

use if_chain::if_chain;
use arrayvec::ArrayVec;

pub fn timesting_abbreviate<'x>(base: &str, this: &'x str) -> (&'x str, bool) {
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
