// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::*;

struct Ctx {
  su: Setup,
  alice: Window,
  bob: Window,
  spec: GameSpec,
}
deref_to_field!{Ctx, Setup, su}
usual_wanted_tests!{Ctx, su}

impl Ctx {
  #[throws(AE)]
  fn drag(&mut self){
    let su = &mut self.su;

    let alice_p1g = {
      let mut w = su.w(&self.alice)?;
      w.synch()?;
      let p1 = w.find_piece("1v1")?;
      let p2 = w.find_piece("2v1")?;
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
      assert!( p1g_new != p1g_old );

      w.synch()?;
      p1g_new
    };

    {
      let mut w = su.w(&self.bob)?;
      w.synch()?;
      let p1 = w.find_piece("1v1")?;
      assert_eq!(p1.posg()?, alice_p1g);
    }
  }

  #[throws(AE)]
  fn rotate(&mut self) -> &'static str {
    let pc = "4v1";
    let su = &mut self.su;

    let chk = |w: &WindowGuard<'_>| {
      let transform = format!("rotate(-90)");
      let pd = w.find_element(By::Id(&w.vpidelem("piece",pc)?))?;
      assert_eq!(pd.get_attribute("transform")?, Some(transform));
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
        .did("rotate")?;

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
      assert_eq!(got_end, exp_end);
      Ok::<_,AE>(())
    };

    let table_size = self.spec.table_size;

    let exp_end = {
      let mut w = su.w(&self.alice)?;
      let p = w.find_piece(pc)?;
      let start = p.posg()?;
      let end = |d| { let mut e = start; e.coords[1] = table_size.y() + d; e };
      let try_end = end(10);
      let exp_end = end(0);
      w.action_chain()
        .move_w(&w, start)?
        .click_and_hold()
        .move_w(&w, try_end)?
        .release()
        .perform()
        .did("drag off")?;

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
      assert_eq!(held, None);
      Ok::<_,AE>(())
    };

    {
      let mut w = su.w(&self.alice)?;
      w.action_chain()
        .move_w(&w, PosC::new(10,10))?
        .click()
        .release()
        .perform()
        .did("unselect by clicking elsewhere")?;

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
    let pc = "1v1";
    let su = &mut self.su;

    {
      let pc = "4v1";
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
      let try_end = (start + PosC::new(dx, 0))?;

      let (sx,sy) = w.posg2posw(start)?;
      w.action_chain()
        .move_to(sx,sy)
        .click()
        .click()
        .perform()
        .did("select and release")?;

      w.synch()?;

      Ok::<_,AE>(Side { window, start, try_end })
    };

    let sides = [
      mk_side(&self.alice, -20)?,
      mk_side(&self.bob,    20)?,
    ];

    dbg!(&sides);

    let paused = su.pause_otter()?;

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
        .did("conflicting drag")?;
    }

    paused.resume()?;

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

      let log = w.retrieve_log(Html::lit("black knight"))?;
      let held = w.piece_held(&pc)?;
      let client = w.client()?;
      let yes = held.as_ref() == Some(&client);

      Ok::<_,AE>(Got { side, now, log, held, client, yes })
    }).collect::<Result<Vec<_>,AE>>()?;

    dbg!(&gots);

    let y = gots.iter().filter(|got|  got.yes).next().expect("y");
    let n = gots.iter().filter(|got| !got.yes).next().expect("n");
    assert_eq!(y.now, y.try_end);
    assert_eq!(n.now, y.try_end);
    assert_eq!(n.now, y.try_end);
    assert_eq!(n.held, y.held);

    for got in &gots {
      let conflict = got.log.find_conflict().is_some();
      assert_eq!(conflict, !got.yes);
    }
  }
}

#[throws(AE)]
fn tests(UsualSetup { su, alice, bob, spec, ..}: UsualSetup) {
  let mut c = Ctx { su, alice, bob, spec };

  test!(c, "drag", c.drag()?);

  test!(c, "drag-rotate-unselect", {
    let pc = c.rotate().did("rotate")?;
    c.drag_off(pc).did("drag off")?;
    c.unselect(pc).did("unselect")?;
  });

  test!(c, "conflict", c.conflict()?);

  debug!("finishing");
}

#[throws(AE)]
pub fn main() { as_usual(tests, module_path!())? }
