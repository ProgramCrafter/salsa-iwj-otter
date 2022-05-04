// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::*;

#[derive(Deref)]
struct Ctx {
  #[deref] su: Setup,
  alice: Window,
  bob: Window,
}
usual_wanted_tests!{Ctx, su}

const HAND: &str = "6v1";
const PAWN: &str = "7v1";
const PAWN2: &str = "8v1";
const ALICE: &str = "1#1";

#[throws(Explode)]
pub fn player_dasharray(player: &'static str) -> String {
  let player: PlayerId = player.try_into().context(player)?;
  let player: slotmap::KeyData = player.into();
  let (player,_) = player.get_idx_version();
  let player: usize = player.try_into().unwrap();
  let player = player.try_into().unwrap();
  let dasharray = player_num_dasharray(player);
  dasharray.into_html_string()
}

impl Ctx {
  #[throws(Explode)]
  fn claim(&mut self){
    let su = &mut self.su;

    let chk = |
        w: &mut WindowGuard<'_>, pc: &str,
        player: Option<&'static str>
    | {
      w.synch()?;

      let dasharray = player.map(player_dasharray).transpose()?;
      let euse = w.find_element(By::Id(&w.vpidelem("piece", pc)?))?;
      let epath = euse.find_element(By::Tag("path"))?;
      let attr = epath.get_attribute("stroke-dasharray")?;

      assert_eq!(attr, dasharray);
      Ok::<_,AE>(())
    };

    let hand_posg = {
      let mut w = su.w(&self.alice)?;
      w.synch()?;

      let hand = w.find_piece(HAND)?;
      let hand_pieceid = hand.pieceid.to_string();

      let hand_posg = hand.posg()?;
      w.action_chain()
        .move_pos(&hand)?
        .send_keys("W")
        .click()
        .send_keys("tW")
        .click()
        .perform()
        .did("select hand")?;
      w.synch()?;

      let top = w.execute_script(&format!(r##"
          return defs_marker.previousElementSibling.dataset.piece;
      "##))?;
      assert_eq!( top.value().as_str().unwrap(),
                  hand_pieceid );

      w.action_chain()
        .send_keys('C')
        .perform()
        .did("claim hand")?;
      w.synch()?;

      let top = w.execute_script(&format!(r##"
          return pieces_marker.nextElementSibling.dataset.piece;
      "##))?;
      assert_eq!( top.value().as_str().unwrap(),
                  hand_pieceid );

      w.action_chain()
        .click()
        .perform()
        .did("deselect")?;

      chk(&mut w, HAND, Some(ALICE))?;

      hand_posg
    };

    {
      let mut w = su.w(&self.bob)?;
      chk(&mut w, HAND, Some(ALICE))?;

      w.get(w.current_url()?)?;
      chk(&mut w, HAND, Some(ALICE))?;
    }

    {
      let mut w = su.w(&self.alice)?;
      let pawn = w.find_piece(PAWN)?;
      w.action_chain()
        .move_pos(&pawn)?
        .click_and_hold()
        .move_w(&w, (hand_posg + PosC::new(20,0))?)?
        .release()
        .perform()?;
      w.synch()?;
    }

    for side in &[&self.alice, &self.bob] {
      let mut w = su.w(side)?;
      w.synch()?;
      let log = w.retrieve_log((&mut |_: &'_ _| false) as LogIgnoreBeforeFn)?;
      log.assert_no_conflicts();
    }

    {
      let mut w = su.w(&self.alice)?;

      let hand = w.find_piece(HAND)?;
      w.action_chain()
        .move_pos(&hand)?
        .click()
        .perform()
        .did("select hand")?;
      w.synch()?;

      w.action_chain()
        .send_keys('C')
        .perform()
        .did("unclaim hand")?;

      w.action_chain()
        .click()
        .perform()
        .did("deselect")?;

      chk(&mut w, HAND, None)?;
    }
    {
      let mut w = su.w(&self.bob)?;
      chk(&mut w, HAND, None)?;
    }
  }

  #[throws(Explode)]
  fn ungrab_race(&mut self){
    let su = &mut self.su;

    const P_ALICE: &str = PAWN;
    const P_BOB:   &str = PAWN2;
    const DEST: Pos = PosC::new(50, 20);

    {
      let mut w = su.w(&self.alice)?;

      w.action_chain()
        .move_pc(&w, P_ALICE)?
        .click()

        .click_and_hold()
        .move_w(&w, DEST)?
        .release()

        .perform()
        .did("alice, drag pawn over target")?;
      w.synch()?;
    }

    {
      let mut w = su.w(&self.bob)?;

      w.action_chain()
        .move_pc(&w, P_BOB)?
        .click_and_hold()
        .move_w(&w, (DEST + PosC::new(2,0))?)?
        .release()
        .perform()
        .did("bob, drag pawn to target")?;
      w.synch()?;
    }

    {
      let mut w = su.w(&self.alice)?;

      w.action_chain()
        .move_pc(&w, P_ALICE)?
        .click()
        .perform()
        .did("alice, drop pawn on target")?;
      w.synch()?;
    }

    let mut chk_alice_on_top = |pl|{
      let mut w = su.w(pl)?;
      w.synch()?;
      let pcs = w.pieces()?;
      let find = |pc| {
        let vpid = w.piece_vpid(pc).unwrap();
        pcs.iter().enumerate()
          .find_map(|(ix, wp)| if wp.piece == vpid { Some(ix) } else { None })
          .unwrap()
      };
      assert!(
        dbgc!( find(P_ALICE) ) > dbgc!( find(P_BOB) )
      );
      Ok::<_,AE>(())
    };


    chk_alice_on_top(&self.alice).did("chk alice")?;
    chk_alice_on_top(&self.bob  ).did("chk bob"  )?;
  }

  #[throws(Explode)]
  fn regrab_race(&mut self){
    let su = &mut self.su;
    const MIDHAND: Pos = PosC::new(40, 40);
    const OUTHAND: Pos = PosC::new(20, 20);

    let bob_gen = {
      let mut w = su.w(&self.bob)?;

      w.action_chain()
        
        .move_pc(&w, PAWN)?
        .click_and_hold()
        .move_w(&w, (MIDHAND + Pos::new(-20,0))?)?
        .release()

        .move_w(&w, MIDHAND)?
        .click()

        .send_keys('C')

        .perform()
        .did("bob, setup")?;

      w.synch()?
    };

    let alice_gen = {
      let pauseable = su.otter_pauseable();
      
      let mut w = su.w(&self.alice)?;
      w.synch()?;
      let pawn = w.find_piece(PAWN)?;
      let start = pawn.posw()?;

      let paused = pauseable.pause()?;

      w.action_chain()
        .move_pos(start)?
        .click_and_hold()
        .move_w(&w, OUTHAND)?
        .release()

        .click()

        .perform()
        .did("alice, drag out, and re-select")?;

      paused.resume()?;

      w.synch()?
    };

    for (who, gen) in &[(&self.alice, alice_gen),
                        (&self.bob,   bob_gen  )] {
      dbgc!(&who.name);
      let w = su.w(who)?;

      let held = w.piece_held(PAWN)?;
      assert_eq!( held.unwrap(), ALICE );

      let log = w.retrieve_log(*gen)?;
      assert_eq!( log.find_conflicts(), Vec::<String>::new() );
    }

    {
      let mut w = su.w(&self.alice)?;
      w.action_chain()
        .click()
        .perform()
        .did("alice, ungrasp for tidy up")?;
      w.synch()?;
    }
  }
}

#[throws(Explode)]
fn tests(UsualSetup { su, alice, bob, ..}: UsualSetup) {
  let mut c = Ctx { su, alice, bob };

  test!(c, "claim",       c.claim()       ?);
  test!(c, "ungrab-race", c.ungrab_race() ?);
  test!(c, "regrab-race", c.regrab_race() ?);

  debug!("finishing");
}

#[throws(Explode)]
pub fn main() { as_usual(tests, module_path!())? }
