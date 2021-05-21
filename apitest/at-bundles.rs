// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::*;

type Ctx = UsualCtx;

impl Ctx {
  #[throws(Explode)]
  fn bundles(&mut self) {
    self.upload_and_check_bundle(
      "test-bundle","lemon", "example-lemon","a lemon",
      &mut |_|Ok(()))?;
  }
 
  #[throws(Explode)]
  fn big(&mut self) {
    self.upload_and_check_bundle(
      "big-bundle","duped-example", "chess-purple-cannon", "a purple cannon",
      &mut |ctx|
    {
      ctx.reset_game(&ctx.ds().gss("reset modded-spec")?)?;

      let alice = ctx.connect_player(&ctx.alice)?;
      let pieces = alice.pieces::<PIA>()?;
      dbgc!(&pieces);
      for expect in &["a purple knight", "a yellow bishop"] {
        pieces.iter().find(
          |p| p.info["desc"].as_str() == Some(expect)
        ).expect(expect);
      }

      Ok(())
    })?;
  }
 
  #[throws(Explode)]
  fn builtin_spec(&mut self) {
    self.su().mgmt_conn.borrow_mut().alter_game(
      vec![ MGI::ResetFromNamedSpec {
        spec: "demo".to_owned(),
      }],
      None,
    )?;
  }
 
  #[throws(Explode)]
  fn reset_with_bundles(&mut self) {
    self.clear_reset_to_demo()?;

    self.reset_game(&self.ds().gss(
      "reset demo-in-test-bundle @examples@/test-bundle.zip"
    )?)?;

    self.clear_reset_to_demo()?;
  }
}

#[throws(Explode)]
fn tests(mut c: Ctx) {
  test!(c, "bundles",                       c.bundles()                ?);
  test!(c, "reset-with-bundles",            c.reset_with_bundles()     ?);
  test!(c, "big",                           c.big()                    ?);
  test!(c, "builtin-spec",                  c.builtin_spec()           ?);
}

#[throws(Explode)]
pub fn main() {
  tests(Ctx::setup()?)?;
}
