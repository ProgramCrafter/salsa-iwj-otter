// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::*;

type Ctx = UsualCtx;

impl Ctx {
  #[throws(Explode)]
  fn multigrab(&mut self) {
    let mut alice = self.connect_player(&self.alice)?;
    let mut a_pieces = alice.pieces::<PIA>()?;

    let [bn] = a_pieces.iter_enumerated()
      .filter_map(|(i,p)| {
        if ! p.info["desc"].as_str()?.contains("400") { return None }
        Some(i)
      })
      .collect::<ArrayVec<_,1>>()
      .into_inner().unwrap();

    let other_pile = [40,20];
    alice.api_piece_op_single(PuSynch((&mut a_pieces, bn)).id(), (
      "multigrab", json!({ "n": 50, 'z': "q000000000" })
    ))?;
    alice.synch()?;

    let _ = other_pile;
  }
}

#[throws(Explode)]
fn tests(mut c: Ctx) {
  test!(c, "multigrab",                     c.multigrab()              ?);
}

#[throws(Explode)]
pub fn main() {
  tests(Ctx::setup()?)?;
}
