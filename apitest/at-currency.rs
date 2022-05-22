// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::*;

type Ctx = UsualCtx;

impl Session {
  #[throws(Explode)]
  fn move_money<PI:Idx>(&mut self,
                        a_pieces: &mut Pieces<PI>, piece: PI,
                        qty: i32, z: &str, pos: Pos) {
    self.api_piece_op_single(PuSynch((&mut *a_pieces, piece)).id(), (
      "multigrab", json!({ "n": qty, 'z': z })
    ))?;

    self.api_piece(GH::Ungrab, PuSynch((&mut *a_pieces, piece)), pos)?;
  }
}

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

    alice.move_money(&mut a_pieces, bank, 13, "t000000000", temp_pos)?;
    let moved = bank;
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

  #[throws(Explode)]
  fn occult(&mut self) {
    self.clear_reset_to_demo()?;

    let mut alice = self.connect_player(&self.alice)?;
    let mut a_pieces = alice.pieces::<PIA>()?;

    let mut bob = self.connect_player(&self.bob)?;

    let hand = a_pieces.find_by_desc_glob(otter::hand::UNCLAIMED_HAND_DESC);
    alice.api_piece(GH::With, PuSynch(&mut (&mut a_pieces, hand)),
                    ("k", json!({ "opname": "claim",
                                   "wrc": WRC::Unpredictable })))?;
    let hand_pos = a_pieces[hand].pos;

    alice.synchu(&mut a_pieces)?;

    let tmoney = a_pieces.find_by_desc_glob("*400ƒ*");
    let pubmoney_pos = a_pieces[tmoney].pos;

    alice.move_money(&mut a_pieces, tmoney, 399, "u000000000", hand_pos)?;
    alice.synchu(&mut a_pieces)?;
    // old tmoney has 1f, current piece has 399

    let div1_pos = (hand_pos + PosC::new(-30,-5))?;
    alice.move_money(&mut a_pieces, tmoney, 99, "u010000000", div1_pos)?;
    // in hand has 300, current piece, aside, has 99
    
    alice.move_money(&mut a_pieces, tmoney, 9, "u020000000", hand_pos)?;
    alice.synchu(&mut a_pieces)?;
    // aside has 90, in hand has 309 (original pos)

    let tmoney = a_pieces.iter_enumerated().filter(|(_,p)| {
      ! p.info.is_null() &&
      p.pos == hand_pos &&
      dbg!(p).info["desc"].as_str().unwrap().contains("ƒ")
    }).collect_vec();
    let tmoney = match &*tmoney { [p] => p.0, x => panic!("{:?}", &x) };

    alice.move_money(&mut a_pieces, tmoney, 20, "u030000000", pubmoney_pos)?;
    alice.synchu(&mut a_pieces)?;
    // alice has 90 (aside) and 289.  Public got 20 and now has 21.

    let expected = [1, 399, 20, 21];
    let expected = expected.into_iter()
      .map(|s| s.to_string())
      .chain(iter::once("?".to_string()))
      .collect_vec();

    let mut qtys = vec![];
    bob.synchx::<PIB,_>(None, None, |_session, gen, _k, v| v.tree_walk(|k,v| {
      if let Some(s) = v.as_str() {
        for m in regex!(
          r#"(?x) ([0-9.?]*) (:?<tspan[^<>]*>)? ƒ "#
        ).captures_iter(s) {
          let qty = m.get(1).unwrap().as_str();
          dbg!(gen, qty, k, s);
          qtys.push(qty.to_string());
          assert!(expected.iter().map(|s| &**s).contains(&qty));
        }
      }
      Ok::<_,Void>(())
    }).void_unwrap())?;

    for exp in expected {
      assert!(qtys.contains(&exp), "{:?}", exp);
    }

    let _ = &mut bob;
    let _ = bob;
  }
}

#[throws(Explode)]
fn tests(mut c: Ctx) {
  test!(c, "multigrab",                     c.multigrab()              ?);
  test!(c, "occult",                        c.occult()                 ?);
}

#[throws(Explode)]
pub fn main() {
  tests(Ctx::setup()?)?;
}
