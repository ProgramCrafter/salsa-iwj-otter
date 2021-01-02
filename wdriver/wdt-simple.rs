// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use otter_webdriver_tests::*;

// Reuse this setup, after a test:
//   target/debug/daemon-otter tmp/wdt-simple/server-config.toml
//   http://localhost:8000/?kmqAKPwK4TfReFjMor8MJhdRPBcwIBpe

#[throws(AE)]
fn main(){
  {
    let (mut su, inst) = setup(module_path!()).always_context("setup")?;
    let [alice, bob] : [Window; 2] =
      su.setup_static_users(&inst)?.try_into().unwrap();
    debug!("ok {:?} {:?}", alice, bob);

    {
      let mut w = su.w(&alice)?;
      w.synch()?;
      let p1 = w.find_piece("1.1")?;
      let p2 = w.find_element(By::Id("use2.1"))?;

      dbg!(
        p1.posg(),
      );
      dbg!(
        p1.posw(),
      );
      
      w.action_chain()
        .move_to_element_center(&p1)
        .click_and_hold()
        .move_to_element_with_offset(&p2, 5, 10)
        .release()
        .perform()
        .always_context("drag")?;

      w.synch()?;
    }

    debug!("finishing");
  }
  info!("ok");
}
