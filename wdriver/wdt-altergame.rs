// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use otter_webdriver_tests::*;

use otter::spec::LinkKind;

struct Ctx {
  su: Setup,
  alice: Window,
}

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
      .context(desc).context("check link")?
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
      .context(desc).context("test link")?
  }

  #[throws(AE)]
  fn test_remove_link(&mut self, kind: LinkKind, desc: &'static str) {
    (||{
      self.otter(&["set-link"], &[&kind.to_string(), ""])?;
      self.check_link(desc, None)?;
      Ok::<_,AE>(())
    })()
      .context(desc).context("test remove link")?
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
    alice.synch_ignore_js_errors()?;
    let url = alice.current_url()?;
    alice.get(url)?;
    alice.synch()?;
  });

  debug!("finishing");
}

#[throws(AE)]
fn main() { as_usual(tests, module_path!())? }
