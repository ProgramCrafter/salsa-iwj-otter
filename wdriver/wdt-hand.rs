// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use otter_webdriver_tests::*;

struct Ctx {
  su: Setup,
  alice: Window,
  bob: Window,
}
ctx_with_setup!{Ctx}

const HAND: &str = "6.1";
const ALICE: &str = "1#1";

#[throws(AE)]
pub fn player_dasharray(player: &'static str) -> String {
  let player: PlayerId = player.try_into().context(player)?;
  let player: slotmap::KeyData = player.into();
  let (player,_) = player.get_idx_version();
  let player: usize = player.try_into().unwrap();
  let player = player.try_into().unwrap();
  let dasharray = player_num_dasharray(player).0;
  dasharray
}

impl Ctx {
  #[throws(AE)]
  fn claim(&mut self){
    let su = &mut self.su;

    let chk = |
        w: &mut WindowGuard<'_>, pc: &str,
        player: Option<&'static str>
    | {
      w.synch()?;

      let dasharray = player.map(player_dasharray).transpose()?;
      let euse = w.find_element(By::Id(&format!("piece{}", pc)))?;
      let epath = euse.find_element(By::Tag("path"))?;
      let attr = epath.get_attribute("stroke-dasharray")?;

      ensure_eq!(attr, dasharray);
      Ok::<_,AE>(())
    };

    {
      let mut w = su.w(&self.alice)?;
      w.synch()?;

      let hand = w.find_piece(HAND)?;
      w.action_chain()
        .move_pos(&hand)?
        .click()
        .perform()
        .context("select hand")?;
      w.synch()?;

      w.action_chain()
        .key_down('C')
        .key_up('C')
        .perform()
        .context("claim hand")?;

      chk(&mut w, HAND, Some(ALICE))?;
    }

    {
      let mut w = su.w(&self.bob)?;
      chk(&mut w, HAND, Some(ALICE))?;
    }

    {
      let mut w = su.w(&self.alice)?;
      w.action_chain()
        .key_down('C')
        .key_up('C')
        .perform()
        .context("unclaim hand")?;

      chk(&mut w, HAND, None)?;
    }
    {
      let mut w = su.w(&self.bob)?;
      chk(&mut w, HAND, None)?;
    }
  }
}

#[throws(AE)]
fn tests(UsualSetup { su, alice, bob, ..}: UsualSetup) {
  let mut c = Ctx { su, alice, bob };

  test!(c, "claim", c.claim()?);

  debug!("finishing");
}

#[throws(AE)]
fn main() { as_usual(tests)? }
