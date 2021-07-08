// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use otter_nodejs_tests::*;

#[derive(StructOpt)]
struct Opts {
  pub nodejs: String,
  pub script: String,
}

#[throws(AE)]
fn main() {
  let opts = Opts::from_args();
  let mut cmd = Command::new(opts.nodejs);
  cmd.arg(opts.script);
  let status = cmd.status()?;
  assert!(status.success(), "{}", status);
}
