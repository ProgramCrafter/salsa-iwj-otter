// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::*;

type Ctx = UsualCtx;

impl Ctx {
  #[throws(Explode)]
  fn dice(&mut self) {
    self.su().mgmt_conn().cmd(&MC::SetFakeTime(FakeTimeSpec(Some(0))))?;
    self.otter_resetting(&G("reset demo"))?;
    
    let mut alice = self.connect_player(&self.alice)?;
    let mut bob = self.connect_player(&self.bob)?;

    let mut a_pieces = alice.pieces::<PIA>()?;
    let hand = a_pieces.find_by_desc_glob(otter::hand::UNCLAIMED_HAND_DESC);
    let hand_pos = a_pieces[hand].pos;
    let die_a = a_pieces.find_by_desc_glob("a d2 *");
    let die_red = a_pieces.find_by_desc_glob("a red die *");

    let move_into_hand = |alice: &mut Session, a_pieces: &'_ mut _, p, x_off| {
      let pos = (hand_pos + PosC::new(x_off,0))?;
      alice.api_piece(GH::With, PuSynch(&mut (a_pieces, p)), pos)?;
      Ok::<_,Explode>(())
    };
    move_into_hand(&mut alice, &mut a_pieces, die_a, -10)?;

    alice.api_piece(GH::With, PuSynch(&mut (&mut a_pieces, hand)),
                    ("k", json!({ "opname": "claim",
                                   "wrc": WRC::Unpredictable })))?;
    move_into_hand(&mut alice, &mut a_pieces, die_red, 10)?;

    self.su().mgmt_conn().cmd(&MC::SetFakeTime(FakeTimeSpec(Some(6000))))?;
    alice.synch()?;
    bob.synch()?;
    alice.api_piece(GH::With, PuSynch(&mut (&mut a_pieces, die_red)),
                    ("k", json!({ "opname": "roll",
                                   "wrc": WRC::UpdateSvg })))?;

    alice.api_piece(GH::With, PuSynch(&mut (&mut a_pieces, hand)),
                    ("k", json!({ "opname": "deactivate",
                                   "wrc": WRC::Unpredictable })))?;
    alice.synch()?;
    bob.synch()?;
  }
}

#[throws(Explode)]
fn tests(mut c: Ctx) {
  test!(c, "dice",                          c.dice()                   ?);
}

#[throws(Explode)]
pub fn main() {
  tests(Ctx::setup()?)?;
}
