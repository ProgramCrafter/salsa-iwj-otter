// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub trait OrdExt : Ord + Sized + Clone {
  fn update_max(&mut self, new: &Self) {
    if *new > *self { *self = new.clone() }
  }
}
impl<T> OrdExt for T where T : Ord + Sized + Clone {
}

pub trait SplitAtDelim<Delim> {
  fn split_at_delim(&self, delim: Delim) -> (&Self, &Self);
}

impl SplitAtDelim<char> for str {
  fn split_at_delim(&self, delim: char) -> (&Self, &Self) {
    match self.find(delim) {
      Some(index) => self.split_at(index),
      None => (self, ""),
    }
  }
}
