// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::*;

type Ctx = UsualCtx;

impl Ctx {
  #[throws(Explode)]
  fn bundles(&mut self) {
    self.upload_and_check_bundle(
      "test-bundle","lemon", "example-lemon","a lemon"
    )?;
  }
 
  #[throws(Explode)]
  fn big(&mut self) {
    self.upload_and_check_bundle(
      "big-bundle","duped-example", "chess-purple-cannon", "a purple cannon"
    )?;
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
}

#[throws(Explode)]
fn tests(mut c: Ctx) {
  test!(c, "bundles",                       c.bundles()      ?);
  test!(c, "big",                           c.big()          ?);
  test!(c, "builtin-spec",                  c.builtin_spec() ?);
}

#[throws(Explode)]
pub fn main() {
  tests(Ctx::setup()?)?;
}
