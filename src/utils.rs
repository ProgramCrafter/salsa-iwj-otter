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
