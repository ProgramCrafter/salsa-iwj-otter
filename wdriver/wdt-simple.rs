// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use otter_webdriver_tests::*;

#[throws(AE)]
fn main(){
  {
    let (mut su, inst) = setup(module_path!()).always_context("setup")?;
    let [alice, bob] : [Window; 2] =
      su.setup_static_users(&inst)?.try_into().unwrap();
    debug!("ok {:?} {:?}", alice, bob);

    su.w(&alice)?.synch()?;

    debug!("finishing");
  }
  info!("ok");
}
