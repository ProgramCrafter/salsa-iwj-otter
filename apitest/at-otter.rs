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
}

#[throws(AE)]
fn tests(Ctx { opts, su, spec, ..}: Ctx) {
}

#[throws(AE)]
fn main() {
  {
    let (opts, _cln, instance, su) = setup_core(&[module_path!()])?;
    let spec = su.ds.game_spec_data()?;
    tests(Ctx { opts, spec, su })?;
  }
  info!("ok");
}
