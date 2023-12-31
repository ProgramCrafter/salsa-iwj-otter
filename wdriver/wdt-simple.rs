// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::*;

#[derive(Deref)]
struct Ctx {
  #[deref] su: Setup,
  alice: Window,
  bob: Window,
  spec: GameSpec,
}
usual_wanted_tests!{Ctx, su}

impl Ctx {
  #[throws(Explode)]
  fn drag(&mut self){
    let su = &mut self.su;

    let p1_vpid = su.initial_vpid_by_desc_glob("a red disc")?;
    let p2_vpid = su.initial_vpid_by_desc_glob("a blue square")?;

    let alice_p1g = {
      let mut w = su.w(&self.alice)?;
      w.synch()?;
      let p1 = w.find_piece(&p1_vpid)?;
      let p2 = w.find_piece(&p2_vpid)?;
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
      let p1 = w.find_piece(&p1_vpid)?;
      assert_eq!(p1.posg()?, alice_p1g);
    }
  }

  #[throws(Explode)]
  fn rotate(&mut self) -> String {
    let su = &mut self.su;
    let pc = su.initial_vpid_by_desc_glob("a black knight")?;

    let chk = |w: &WindowGuard<'_>| {
      let transform = format!("rotate(-90)");
      let pd = w.find_element(By::Id(&w.vpidelem("piece",&pc)?))?;
      assert_eq!(pd.get_attribute("transform")?, Some(transform));
      Ok::<_,AE>(())
    };

    {
      let mut w = su.w(&self.alice)?;
      let p = w.find_piece(&pc)?;
      w.action_chain()
        .move_pos(&p)?
        .click()
        .send_keys('l')
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

  #[throws(Explode)]
  fn drag_off(&mut self, pc: &str) {
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

  #[throws(Explode)]
  fn unselect(&mut self, pc: &str) {
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

  #[throws(Explode)]
  fn conflict(&mut self) {
    let su = &mut self.su;
    let pc = su.initial_vpid_by_desc_glob("a red disc")?;

    {
      let pc = su.initial_vpid_by_desc_glob("a black knight")?;
      let w = su.w(&self.alice)?;
      w.action_chain()
        .move_pc(&w, &pc)?
        .click();
    }

    #[derive(Debug)]
    struct Side<'s> {
      window: &'s Window,
      start: Pos,
      try_end: Pos,
      before_gen: Generation,
    }

    let mut mk_side = |window, dx| {
      let mut w = su.w(window)?;
      let p = w.find_piece(&pc)?;
      let start = p.posg()?;
      let try_end = (start + PosC::new(dx, 0))?;

      let (sx,sy) = w.posg2posw(start)?;
      w.action_chain()
        .move_to(sx,sy)
        .click()
        .click()
        .perform()
        .did("select and release")?;

      let gen = w.synch()?;

      Ok::<_,AE>(Side { window, start, try_end, before_gen: gen })
    };

    let sides = [
      mk_side(&self.alice, -20)?,
      mk_side(&self.bob,    20)?,
    ];

    dbg!(&sides);


    #[derive(Debug)]
    struct Got<'s> {
      side: &'s Side<'s>,
      yes: bool,
      now: Pos,
      log: Vec<String>,
      held: Option<String>,
      #[allow(dead_code)] /* just for Debug */ client: String,
    }
    impl<'s> Deref for Got<'s> {
      type Target = Side<'s>;
      fn deref<'t>(&'t self) -> &'t Side<'s> { self.side }
    }

    let check = |su: &mut Setup, before_gen, check_end_pos|{

      let gots = sides.iter().map(|side|{
        let mut w = su.w(side.window)?;
        w.synch()?;
        let p = w.find_piece(&pc)?;
        let now = p.posg()?;

        let log = w.retrieve_log(before_gen)?;
        let held = w.piece_held(&pc)?;
        let client = w.client()?;
        let yes = held.as_ref() == Some(&client);

        Ok::<_,AE>(Got { side, now, log, held, client, yes })
      }).collect::<Result<Vec<_>,AE>>()?;

      dbg!(&gots);

      let y = gots.iter().find(|got|  got.yes).expect("y");
      let n = gots.iter().find(|got| !got.yes).expect("n");

      if check_end_pos {
        assert_eq!(y.now, y.try_end);
        assert_eq!(n.now, y.try_end);
        assert_eq!(n.now, y.try_end);
      }
      assert_eq!(n.held, y.held);

      for got in &gots {
        let conflicts = got.log.find_conflicts();
        let helderrs = got.log.iter()
          .filter(|m| m.contains("piece held by another player"))
          .cloned().collect_vec();

        dbg!( &conflicts, &helderrs );
        assert!( conflicts.len() <= 1 );
        assert!( helderrs .len() <= 1 );
        if got.yes {
          assert_eq!( conflicts.len(), 0 );
          assert_eq!( helderrs .len(), 0 );
        } else {
          assert!( conflicts.len() == 1 ||
                   helderrs .len() == 1 );
        }
      }

      let mut yw = su.w(y.window)?;
      yw.action_chain()
        .move_w(&yw, y.now)?
        .click()
        .perform()
        .did("ungrab to tidy")?;

      let gen = yw.synch()?;

      Ok::<_,AE>((gen, y.now))
    };


    let paused = su.pause_otter()?;
    for side in &sides {
      let w = su.w(side.window)?;
      w.action_chain()

        .move_w(&w, side.start)?
        .click()

        .click_and_hold()
        .move_w(&w, side.try_end)?
        .release()

        .perform()
        .did("conflicting drag")?;
    }
    let pauseable = paused.resume()?;

    let (gen_before, pos_now) = check(su, sides[0].before_gen, true)
      .did("conflicting drag, check")?;

    let paused = pauseable.pause()?;
    for side in &sides {
      let w = su.w(side.window)?;
      w.action_chain()

        .move_w(&w, pos_now)?
        .click()

        .perform()
        .did("conflicting grasp")?;
    }
    let _pauseable = paused.resume()?;

    check(su, gen_before, false)
      .did("conflicting grasp, check")?;
  }
}

#[throws(Explode)]
fn tests(UsualSetup { su, alice, bob, spec, ..}: UsualSetup) {
  let mut c = Ctx { su, alice, bob, spec };

  test!(c, "drag", c.drag()?);

  test!(c, "drag-rotate-unselect", {
    let pc = c.rotate().did("rotate")?;
    c.drag_off(&pc).did("drag off")?;
    c.unselect(&pc).did("unselect")?;
  });

  test!(c, "conflict", c.conflict()?);

  debug!("finishing");
}

#[throws(Explode)]
pub fn main() { as_usual(tests, module_path!())? }
