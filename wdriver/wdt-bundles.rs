// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::*;

struct Ctx {
  su: Setup,
  alice: Window,
  bob: Window,
}
usual_wanted_tests!{Ctx, su}

impl Ctx {
  #[throws(Explode)]
  fn otter(&mut self, verb: &[&str], args: &[&str]) {
    self.su.w(&self.alice)?.otter(verb, args)?
  }

  #[throws(Explode)]
  fn vatikan_with_deck(&mut self) {
    let game_spec = &self.su.ds.subst("@specs@/vatikan.game.toml")?;
    self.otter(&["reset"],&[&game_spec])?;

    {
      let mut alice = self.su.w(&self.alice)?;
      alice.synch()?;

      alice.action_chain()
        .key_down('0')
        .key_up('0')
        .move_w(&alice, VATIKAN_DECK)?
        .click()
        .release()
        .send_keys('A')
        .perform()
        .did("activate")?;
      alice.synch()?;
    }
  }
}

const VATIKAN_DECK: Pos = Pos::new(150,184);
const VATIKAN_HAND: Pos = Pos::new(68, 175);

#[throws(Explode)]
fn tests(UsualSetup { su, alice, bob, ..}: UsualSetup) {
  let mut c = Ctx { su, alice, bob };

  test!(c, "bundle", {
    let test_bundle = c.su.ds.example_bundle();
    let hash_exp = bundles::DigestWrite::of(
      &mut File::open(&test_bundle).unwrap()
    )?;

    c.otter(&["upload-bundle"],&[&test_bundle])?;
    let mut w = c.su.w(&c.alice)?;

    let check = |w: &mut WindowGuard<'_>| Ok::<_,Explode>({
      w.synch()?;
      let bundle_link = w.find_element(By::ClassName("b_link"))?;
      let bundle_id = bundle_link.find_element(By::ClassName("b_id"))?;
      let html = bundle_id.inner_html()?;
      assert_eq!(&html, "00000.zip");

      let download = dbgc!(bundle_link.get_attribute("href")?).unwrap();
      let client = reqwest::blocking::Client::new();
      let download = Url::parse(&w.current_url()?)?.join(&download)?;
      let mut resp = client.get(download).send()?;
      let st = resp.status();
      assert!(st.is_success(), "{:?}", &st);
      let mut digester = bundles::DigestWrite::sink();
      resp.copy_to(&mut digester)?;
      let got = digester.finish().0;
      assert_eq!(got, hash_exp);
    });

    check(&mut w)?;

    let url = w.current_url()?;
    w.get(url)?;
    check(&mut w)?;

  });

  test!(c, "hidden", {
    c.vatikan_with_deck()?;

    let deckg = VATIKAN_DECK;
    let handg = VATIKAN_HAND;

    {
      let mut alice = c.su.w(&c.alice)?;
      alice.synch()?;

      let deckp = alice.posg2posw(deckg)?;
      let handp = alice.posg2posw(handg)?;
      alice.action_chain()
        .move_pos(handp)?
        .click()
        .release()
        .send_keys('C')
        .perform()
        .did("claim")?;
      alice.synch()?;

      alice.action_chain()
        .move_pos(deckp)?
        .send_keys("5")
        .click_and_hold()
        .move_pos(handp)?
        .release()
        .perform()
        .did("draw")?;
      alice.synch()?;
    }

    {
      let mut bob = c.su.w(&c.bob)?;
      bob.synch()?;
      bob.get(bob.current_url()?)?;
      bob.synch()?;
    }

    {
      let mut alice = c.su.w(&c.alice)?;
      let cardsg = Pos::new(80,100);

      alice.action_chain()
        .move_w(&alice, (handg + Pos::new(   2, -15 ))?)?
        .click_and_hold()
        .move_w(&alice, (handg + Pos::new( -20,   5 ))?)?
        .release()
        .move_w(&alice, (handg + Pos::new(  -7,  -0 ))?)?
        .click_and_hold()
        .move_w(&alice, cardsg)?
        .release()
        .perform()
        .did("play")?;

      alice.synch()?;
      drop(alice);
      let mut alice = c.su.w(&c.alice)?;

      alice.action_chain()
        .move_w(&alice, cardsg)?
        .click().release()
        .move_w(&alice, deckg)?
        .move_w(&alice, cardsg)?
        .click().release()
        .send_keys('b')
        .perform()
        .did("select played")?;
      alice.synch()?;

    }

  });

  test!(c, "impossible", {
    // We want to test that the client can cope with an unpredictable
    // _impossible_ error, reported via an update.  A way to generate
    // such a thing is to try to make overlapping occultations, which
    // the client does not try to prevent.

    c.vatikan_with_deck()?;

    {
      let mut alice = c.su.w(&c.alice)?;

      let handp  = alice.posg2posw(VATIKAN_HAND)?;
      let newpos = alice.posg2posw((VATIKAN_HAND + Pos::new(50, 0))?)?;

      alice.action_chain()
        .send_keys("W")
        .move_pos(handp)?
        .click()
        .release()
        .click_and_hold()
        .move_pos(newpos)?
        .release()
        .send_keys("W")
        .perform()
        .did("move to overlap")?;
      let gen_before = alice.synch()?;

      alice.action_chain()
        .move_pos(newpos)?
        .click().release()
        .send_keys("C")
        .perform()
        .did("try to claim when overlapping")?;
      alice.synch()?;

      let log = alice.retrieve_log(gen_before)?;
      dbg!(&log);
      assert_eq!( 1, log.iter().filter(|s| s.contains(
        "Problem manipulating piece: overlapping occultation(s)"
      )).count());
    }

  });

  debug!("finishing");
}

#[throws(Explode)]
pub fn main() { as_usual(tests, module_path!())?; }
