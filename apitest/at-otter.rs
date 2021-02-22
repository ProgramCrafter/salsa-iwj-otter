// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(dead_code)]
#![allow(unused_variables)]

use otter_api_tests::*;

struct Ctx {
  opts: Opts,
  su: SetupCore,
  spec: GameSpec,
}
deref_to_field!{Ctx, SetupCore, su}

impl Ctx {
  #[throws(AE)]
  pub fn otter<S:AsRef<str>>(&mut self, args: &[S]) {
    let args: Vec<String> =
      ["--account", "server:"].iter().cloned().map(Into::into)
      .chain(args.iter().map(|s| s.as_ref().to_owned()))
      .collect();
    self.ds.otter(&args)?;
  }

  #[throws(AE)]
  fn library_load(&mut self) {
    prepare_game(&self.ds, TABLE)?;

    self.otter(&self.ds.ss("library-add @table@ wikimedia chess-blue-?")?)?;
  }
}

#[throws(AE)]
fn tests(mut c: Ctx) {
  test!(c, "library-load", c.library_load()?);
}

#[throws(AE)]
fn main() {
  {
    let (opts, _cln, instance, mut su) = setup_core(&[module_path!()])?;
    let spec = su.ds.game_spec_data()?;
    let users = su.ds.setup_static_users(default(), |_|Ok(()))?;
    tests(Ctx { opts, spec, su })?;
  }
  info!("ok");
}
