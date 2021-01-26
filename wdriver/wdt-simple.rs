// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use otter_webdriver_tests::*;

struct Ctx {
  su: Setup,
  alice: Window,
  bob: Window,
  spec: otter::spec::GameSpec,
}

impl Ctx {
  #[throws(AE)]
  fn drag(&mut self){
    let su = &mut self.su;

    let alice_p1g = {
      let mut w = su.w(&self.alice)?;
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
        .perform()?;

      let p1g_new = p1.posg()?;
      dbg!(p1g_old, p1g_new);
      ensure!( p1g_new != p1g_old );

      w.synch()?;
      p1g_new
    };

    {
      let mut w = su.w(&self.bob)?;
      w.synch()?;
      let p1 = w.find_piece("1.1")?;
      ensure_eq!(p1.posg()?, alice_p1g);
    }
  }

  #[throws(AE)]
  fn rotate(&mut self) -> &'static str {
    let pc = "4.1";
    let su = &mut self.su;

    let chk = |w: &WindowGuard<'_>| {
      let transform = format!("rotate(-90)");
      let pd = w.find_element(By::Id(&format!("piece{}",pc)))?;
      ensure_eq!(pd.get_attribute("transform")?, Some(transform));
      Ok::<_,AE>(())
    };

    {
      let mut w = su.w(&self.alice)?;
      let p = w.find_piece(pc)?;
      let (px,py) = p.posw()?;
      w.action_chain()
        .move_to(px,py)
        .click()
        .release()
        .key_down('l')
        .key_up('l')
        .perform()
        .always_context("rotate")?;

      chk(&w)?;
      w.synch()?;
    }

    {
      let mut w = su.w(&self.bob)?;
      w.synch()?;
      chk(&w)?;
    }

    pc
  }

  #[throws(AE)]
  fn drag_off(&mut self, pc: &'static str) {
    let su = &mut self.su;

    let chk = |w: &WindowGuard<'_>| {
      Ok::<_,AE>(())
    };

    let table_size = self.spec.table_size
      .ok_or(anyhow!("table size missing from spec"))?;

    {
      let mut w = su.w(&self.alice)?;
      let p = w.find_piece(pc)?;
      let start = p.posg()?;
      let (sx,sy) = w.posg2posw(start)?;
      let end = |d| { let mut e = start; e.0[1] = table_size.0[1] + d; e };
      let try_end = end(10);
      let exp_end = end(0);
      let (ex,ey) = w.posg2posw(try_end)?;
      w.action_chain()
        .move_to(sx,sy)
        .click_and_hold()
        .move_to(ex,ey)
        .release()
        .perform()
        .always_context("drag off")?;

      chk(&w)?;
      w.synch()?;
    }

    {
      let mut w = su.w(&self.bob)?;
      w.synch()?;
      chk(&w)?;
    }

    pc
  }

  #[throws(AE)]
  fn unselect(&mut self, pc: &'static str) {
    let su = &mut self.su;

    let chk = |w: &WindowGuard<'_>| {
      let held = w.execute_script(&format!(r##"
        let pc = pieces['{}'];
        pc.held;
                       "##, &pc))?;
      let held = held.value();
      dbg!(held);
      ensure_eq!(held, &serde_json::Value::Null);
      Ok::<_,AE>(())
    };

    {
      let mut w = su.w(&self.alice)?;
      w.action_chain()
        .move_to(10,10)
        .click()
        .release()
        .perform()
        .always_context("unselect by clicking elsewhere")?;

      chk(&w)?;
      w.synch()?;
    }

    {
      let mut w = su.w(&self.bob)?;
      w.synch()?;
      chk(&w)?;
    }
  }
}

#[throws(AE)]
fn main(){
  {
    let (mut su, inst) = setup(module_path!()).always_context("setup")?;
    let [alice, bob] : [Window; 2] =
      su.setup_static_users(&inst)?.try_into().unwrap();
    let spec = su.ds.game_spec_data()?;
    debug!("ok {:?} {:?}", alice, bob);

    let mut c = Ctx { su, alice, bob, spec };

    c.drag().always_context("drag")?;
    let pc = c.rotate().always_context("rotate")?;
    c.drag_off(pc).always_context("drag off")?;
    c.unselect(pc).always_context("unselect")?;

    debug!("finishing");
  }
  info!("ok");
}
