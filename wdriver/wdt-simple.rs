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

    {
      let pc = "4.1";
      let w = su.w(&self.alice)?;
      w.action_chain()
        .move_pc(&w, pc)?
        .click();
    }

    #[derive(Debug)]
    struct Side<'s> {
      window: &'s Window,
      start: Pos,
      try_end: Pos,
    }

    let mut mk_side = |window, dx| {
      let mut w = su.w(window)?;
      let p = w.find_piece(pc)?;
      let start = p.posg()?;
      let try_end = start + PosC([dx, 0]);

      let (sx,sy) = w.posg2posw(start)?;
      w.action_chain()
        .move_to(sx,sy)
        .click()
        .click()
        .perform()
        .context("select and release")?;

      w.synch()?;

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

      w.action_chain()
        .move_w(&w, side.start)?
        .click()
        .release()

        .click_and_hold()
        .move_w(&w, side.try_end)?
        .release()

        .perform()
        .context("conflicting drag")?;
    }

    nix::sys::signal::kill(pid, nix::sys::signal::SIGCONT)?;

    #[derive(Debug)]
    struct Got<'s> {
      side: &'s Side<'s>,
      yes: bool,
      now: Pos,
      log: Vec<String>,
      held: Option<String>,
      client: String,
    }
    impl<'s> Deref for Got<'s> {
      type Target = Side<'s>;
      fn deref<'t>(&'t self) -> &'t Side<'s> { &self.side }
    }

    let gots = sides.iter().map(|side|{
      let mut w = su.w(side.window)?;
      w.synch()?;
      let p = w.find_piece(pc)?;
      let now = p.posg()?;
      let log = w.find_elements(By::ClassName("logmsg"))?;
      let log = log.iter()
        .rev()
        .map(|e| e.inner_html())
        .take_while(|h| {
          h.as_ref().ok()
            .map(|s| s.contains("black knight"))
            != Some(true)
        })
        .collect::<Result<Vec<String>,_>>()?;

      let held = w.piece_held(&pc)?;
      let client = w.client()?;
      let yes = held.as_ref() == Some(&client);

      Ok::<_,AE>(Got { side, now, log, held, client, yes })
    }).collect::<Result<Vec<_>,AE>>()?;

    dbg!(&gots);

    let y = gots.iter().filter(|got|  got.yes).next().expect("y");
    let n = gots.iter().filter(|got| !got.yes).next().expect("n");
    ensure_eq!(y.now, y.try_end);
    ensure_eq!(n.now, y.try_end);
    ensure_eq!(n.now, y.try_end);
    ensure_eq!(n.held, y.held);

    for got in &gots {
      let conflict = got.log.iter().any(|m| m.starts_with("Conflict!"));
      ensure_eq!(conflict, !got.yes);
    }
  }
}

#[throws(AE)]
fn tests(UsualSetup { su, alice, bob, spec, ..}: UsualSetup) {
  let mut c = Ctx { su, alice, bob, spec };

  test!(c, "drag", c.drag()?);

  test!(c, "drag-rotate-unselect", {
    let pc = c.rotate().always_context("rotate")?;
    c.drag_off(pc).always_context("drag off")?;
    c.unselect(pc).always_context("unselect")?;
  });

  test!(c, "conflict", c.conflict()?);

  debug!("finishing");
}

#[throws(AE)]
fn main() { as_usual(tests)? }
