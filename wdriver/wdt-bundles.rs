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
}

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
    let game_spec = &c.su.ds.subst("@specs@/vatikan.game.toml")?;
    c.otter(&["reset"],&[&game_spec])?;

    let deckg = Pos::new(150,184);
    let handg = Pos::new(68, 175);

    {
      let mut alice = c.su.w(&c.alice)?;
      alice.synch()?;

      let deckp = alice.posg2posw(deckg)?;
      let handp = alice.posg2posw(handg)?;
      alice.action_chain()
        .move_pos(handp)?
        .click()
        .release()
        .key_down(Keys::Shift)
        .send_keys('C')
        .key_up(Keys::Shift)
        .key_down('0')
        .key_up('0')
        .move_pos(deckp)?
        .click()
        .release()
        .key_down(Keys::Shift)
        .send_keys('A')
        .key_up(Keys::Shift)
        .perform()
        .did("activate")?;
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

  debug!("finishing");
}

#[throws(Explode)]
pub fn main() { as_usual(tests, module_path!())?; }
