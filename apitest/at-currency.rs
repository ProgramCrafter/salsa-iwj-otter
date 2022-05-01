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

    let bn1 = a_pieces.find_by_desc_glob("*400ƒ*");

    let other_pile = PosC::new(40,20);

    alice.api_piece_op_single(PuSynch((&mut a_pieces, bn1)).id(), (
      "multigrab", json!({ "n": 50, 'z': "q000000000" })
    ))?;
    alice.synchu(&mut a_pieces)?;

    let bn_desc = a_pieces[bn1].info["desc"].as_str().unwrap();
    assert!( bn_desc.contains("50ƒ"), "{bn_desc}" );
    let _change = a_pieces.find_by_desc_glob("*350ƒ*");

    alice.api_piece(GH::Ungrab, PuSynch((&mut a_pieces, bn1)), other_pile)?;
    alice.synchu(&mut a_pieces)?;

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
