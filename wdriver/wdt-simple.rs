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
ctx_with_setup!{Ctx}

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
      let (p2x,p2y) = p2.posw()?;

      w.action_chain()
        .move_pos(&p1)?
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
      w.action_chain()
        .move_pos(&p)?
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

    let chk = |w: &WindowGuard<'_>, exp_end| {
      let got_end = w.find_piece(pc)?.posg()?;
      ensure_eq!(got_end, exp_end);
      Ok::<_,AE>(())
    };

    let table_size = self.spec.table_size
      .ok_or(anyhow!("table size missing from spec"))?;

    let exp_end = {
      let mut w = su.w(&self.alice)?;
      let p = w.find_piece(pc)?;
      let start = p.posg()?;
      let end = |d| { let mut e = start; e.0[1] = table_size.0[1] + d; e };
      let try_end = end(10);
      let exp_end = end(0);
      w.action_chain()
        .move_w(&w, start)?
        .click_and_hold()
        .move_w(&w, try_end)?
        .release()
        .perform()
        .always_context("drag off")?;

      w.synch()?;
      chk(&w, exp_end)?;

      exp_end
    };

    {
      let mut w = su.w(&self.bob)?;
      w.synch()?;
      chk(&w, exp_end)?;
    }

    pc
  }

  #[throws(AE)]
  fn unselect(&mut self, pc: &'static str) {
    let su = &mut self.su;

    let chk = |w: &WindowGuard<'_>| {
      let held = w.piece_held(pc)?;
      ensure_eq!(held, None);
      Ok::<_,AE>(())
    };

    {
      let mut w = su.w(&self.alice)?;
      w.action_chain()
        .move_w(&w, PosC([10,10]))?
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

  #[throws(AE)]
  fn conflict(&mut self) {
    let pc = "1.1";
    let su = &mut self.su;

    #[derive(Debug)]
    struct Side<'s> {
      window: &'s Window,
      start: Pos,
      try_end: Pos,
    }

    let mut mk_side = |window, dx| {
      let w = su.w(window)?;
      let p = w.find_piece(pc)?;
      let start = p.posg()?;
      let try_end = start + PosC([dx, 0]);
      Ok::<_,AE>(Side { window, start, try_end })
    };

    let sides = [
      mk_side(&self.alice, -20)?,
      mk_side(&self.bob,    20)?,
    ];

    dbg!(&sides);

    let pid = nix::unistd::Pid::from_raw(su.server_child.id() as nix::libc::pid_t);
    nix::sys::signal::kill(pid, nix::sys::signal::SIGSTOP)?;

    for side in &sides {
      let w = su.w(side.window)?;
      let p = w.find_piece(pc)?;

      w.action_chain()
        .move_w(&w, side.start)?
        .click_and_hold()
        .move_w(&w, side.try_end)?
        .release()
        .perform()
        .context("conflicting drag")?;

//      let mut pause = Duration::from_millis(1);
//      loop {
//        ensure!( pause < Duration::from_secs(1) );
//        dbg!(pause);
//        sleep(pause);
        let now = p.posg()?;
ensure_eq!(now, side.try_end);
//        if now == side.try_end { break }
//        pause *= 2;
//      }

      
    }

    nix::sys::signal::kill(pid, nix::sys::signal::SIGCONT)?;
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

    test!(c, "drag", c.drag().always_context("drag")?);

    test!(c, "drag-rotate-unselect", {
      let pc = c.rotate().always_context("rotate")?;
      c.drag_off(pc).always_context("drag off")?;
      c.unselect(pc).always_context("unselect")?;
    });

    c.conflict().always_context("conflict handling")?;

    debug!("finishing");
  }
  info!("ok");
}
