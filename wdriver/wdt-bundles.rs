// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::*;

struct Ctx {
  su: Setup,
  alice: Window,
}
usual_wanted_tests!{Ctx, su}

impl Ctx {
  #[throws(Explode)]
  fn otter(&mut self, verb: &[&str], args: &[&str]) {
    self.su.w(&self.alice)?.otter(verb, args)?
  }
}

#[throws(Explode)]
fn tests(UsualSetup { su, alice, ..}: UsualSetup) {
  let mut c = Ctx { su, alice };

  test!(c, "bundle", {
    let test_bundle = c.su.ds.example_bundle();
    c.otter(&["upload-bundle"],&[&test_bundle])?;
    let mut w = c.su.w(&c.alice)?;

    let check = |w: &mut WindowGuard<'_>| Ok::<_,Explode>({
      w.synch()?;
      let bundle_id = w.find_element(By::ClassName("b_id"))?;
      let html = bundle_id.inner_html()?;
      assert_eq!(&html, "00000.zip");
    });

    check(&mut w)?;

    let url = w.current_url()?;
    w.get(url)?;
    check(&mut w)?;

  });

  debug!("finishing");
}

#[throws(Explode)]
pub fn main() { as_usual(tests, module_path!())?; }
