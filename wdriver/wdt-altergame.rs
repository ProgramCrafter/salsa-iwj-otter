// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::*;

use otter::spec::LinkKind;

struct Ctx {
  su: Setup,
  alice: Window,
}
usual_wanted_tests!{Ctx, su}

impl Ctx {
  #[throws(AE)]
  fn check_link(&mut self, desc: &'static str, url: Option<&str>) {
    (||{
      let mut w = self.su.w(&self.alice)?;
      w.synch()?;
      let container = w.find_element(By::Id("links"))?;
      let relevant = container
        .find_elements(By::Tag("a"))?;
      let relevant: Vec<_> = relevant
        .iter()
        .map(|e| Ok::<_,AE>((e, e.text()?)))
        .collect::<Result<Vec<_>,AE>>()?;
      let relevant: Vec<_> = relevant
        .iter()
        .filter(|(_e, txt)| txt == desc)
        .collect();
      assert_eq!(relevant.len(), url.iter().len());
      if let Some(url) = url {
        assert_eq!(relevant[0].0.get_attribute("href")?
                   .as_ref().map(|s| s.as_str()), Some(url));
      }
      Ok::<_,AE>(())
    })()
      .context(desc).did("check link")?
  }

  #[throws(AE)]
  fn otter(&mut self, verb: &[&str], args: &[&str]) {
    self.su.w(&self.alice)?.otter(verb, args)?
  }

  #[throws(AE)]
  fn test_link(&mut self, kind: LinkKind, desc: &'static str, url: &str) {
    (||{
      self.otter(&["set-link"], &[&kind.to_string(), url])?;
      self.check_link(desc, Some(url))?;
      Ok::<_,AE>(())
    })()
      .context(desc).did("test link")?
  }

  #[throws(AE)]
  fn test_remove_link(&mut self, kind: LinkKind, desc: &'static str) {
    (||{
      self.otter(&["set-link"], &[&kind.to_string(), ""])?;
      self.check_link(desc, None)?;
      Ok::<_,AE>(())
    })()
      .context(desc).did("test remove link")?
  }
}

#[throws(AE)]
fn tests(UsualSetup { su, alice, ..}: UsualSetup) {
  let mut c = Ctx { su, alice };

  test!(c, "links", {
    c.check_link("Info", None)?;
    c.check_link("Voice", Some("https://jitsi.example.com/initial"))?;
    c.test_link(LinkKind::Info, "Info", "https://www.example.org/newinfo")?;
    c.test_remove_link(LinkKind::Info, "Info")?;
  });

  test!(c, "reset", {
    let game_spec = &c.su.ds.subst("@specs@/penultima.game.toml")?;
    let mut alice = c.su.w(&c.alice)?;
    alice.otter(&["reset"],&[&game_spec])?;
    alice.synch()?;
    let url = alice.current_url()?;
    alice.get(url)?;
    alice.synch()?;
  });

  test!(c, "reset-move", {
    let pauseable = c.su.otter_pauseable();
    let game_spec = &c.su.ds.subst("@specs@/mao.game.toml")?;
    let mut alice = c.su.w(&c.alice)?;
    alice.otter(&["reset"],&[&game_spec])?;
    alice.synch()?;

    let p1 = Pos::new(150,84);
    let p2 = Pos::new(73,31);

    let p1w = alice.posg2posw(p1)?;
    let got = alice.execute_script(
      &Subst::from(&[("xy", format!("{},{}", p1w.0, p1w.1))]).subst(r#"
        let elem = document.elementFromPoint(@xy@);
        for (;;) {
            let id = elem.getAttribute('id');
            if (id) return id;
            elem = elem.parentElement
            if (!elem) return null;
        }
    "#)?)?;
    let elem = got.value().as_str().unwrap();
    let piece = elem.strip_prefix("use").unwrap().to_owned();
    let p = alice.find_piece(&piece)?;
    
    let paused = pauseable.pause()?;
    alice.action_chain()
      .move_w(&alice, p1)?
      .click_and_hold()
      .move_w(&alice, p2)?
      .release()
      .perform()
      .did("click and hold while paused")?;

    let got_p2 = p.posg()?;
    assert_eq!(p2, got_p2);
    alice.fetch_js_log()?;

    let _pauseable = paused.resume()?;
    alice.synch()?;
    alice.get(alice.current_url()?)?;
    alice.synch()?;
    let p = alice.find_piece(&piece)?;
    let got_p2 = p.posg()?;
    assert_eq!(p2, got_p2);
  });

  debug!("finishing");
}

#[throws(AE)]
pub fn main() { as_usual(tests, module_path!())? }
