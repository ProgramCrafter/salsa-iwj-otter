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

    let alice_p1g = {
      let mut w = su.w(&alice)?;
      w.synch()?;
      let p1 = w.find_piece("1.1")?;
      let p2 = w.find_piece("2.1")?;
      let p1g_old = p1.posg()?;
      let (p1x,p1y) = p1.posw()?;
      let (p2x,p2y) = p2.posw()?;

      w.action_chain()
        .move_to(p1x, p1y)
        .click_and_hold()
        .move_to(p2x + 5, p2y + 10)
        .release()
        .perform()
        .always_context("drag")?;

      let p1g_new = p1.posg()?;
      dbg!(p1g_old, p1g_new);
      ensure!( p1g_new != p1g_old );

      w.synch()?;
      p1g_new
    };

    {
      let mut w = su.w(&bob)?;
      w.synch()?;
      let p1 = w.find_piece("1.1")?;
      ensure!( p1.posg()? == alice_p1g );
    }

    {
      let mut w = su.w(&alice)?;
      let p = w.find_piece("4.1")?;
      let (px,py) = p.posw()?;
      w.action_chain()
        .move_to(px,py)
        .click()
        .release()
        .key_down('l')
        .key_up('l')
        .perform()
        .always_context("rotate")?;

      let transform = format!("rotate(-90");
      let pd = w.find_element(By::Id("piece4.1"))?;
      ensure!(pd.get_attribute("transform")? == Some(transform));
      w.synch()?;
    }

    debug!("finishing");
  }
  info!("ok");
}
