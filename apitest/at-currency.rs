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

    let bank = a_pieces.find_by_desc_glob("*400ƒ*");

    let pile_pos = PosC::new(40,20);
    let temp_pos = PosC::new(40,10);

    alice.api_piece_op_single(PuSynch((&mut a_pieces, bank)).id(), (
      "multigrab", json!({ "n": 50, 'z': "q000000000" })
    ))?;
    alice.synchu(&mut a_pieces)?;

    let pile = bank;
    let bank = a_pieces.find_by_desc_glob("* 350ƒ*");
    a_pieces[pile].assert_desc_contains(" 50ƒ");

    alice.api_piece(GH::Ungrab, PuSynch((&mut a_pieces, pile)), pile_pos)?;
    alice.synchu(&mut a_pieces)?;

    alice.api_piece_op_single(PuSynch((&mut a_pieces, bank)).id(), (
      "multigrab", json!({ "n": 13, 'z': "t000000000" })
    ))?;
    let moved = bank;
    alice.api_piece(GH::Ungrab, PuSynch((&mut a_pieces, moved)), temp_pos)?;
    alice.synchu(&mut a_pieces)?;
    let bank = a_pieces.find_by_desc_glob("* 337ƒ*");
    a_pieces[moved].assert_desc_contains(" 13ƒ");

    alice.api_piece(GH::With, PuSynch((&mut a_pieces, moved)), pile_pos)?;
    alice.synchu(&mut a_pieces)?;
    assert!(a_pieces[moved].info.is_null());
    a_pieces[pile].assert_desc_contains(" 63ƒ");

    // This saves us some complaints
    let _ = moved;
    let _ = bank;
    let _ = pile;
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
