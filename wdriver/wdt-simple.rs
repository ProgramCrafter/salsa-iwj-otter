// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use otter_webdriver_tests::*;

#[throws(AE)]
fn main(){
  let s = setup(module_path!()).always_context("setup")?;

  debug!("ok");
  info!("hi! {:#?}", &s);
}
