// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use otter_webdriver_tests::*;

#[throws(AE)]
fn main(){
  let su = setup(module_path!()).always_context("setup")?;

  let w1 = su.new_window("alice")?;
  w1.get("/?zonk")?;
  w1.screenshot("test alice")?;

  debug!("ok");
  info!("hi! {:#?}", &su.ds);
}
