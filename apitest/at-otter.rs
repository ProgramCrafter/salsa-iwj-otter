// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::*;

type Ctx = UsualCtx;

impl Ctx {
  #[throws(Explode)]
  fn library_load(&mut self) {
    self.prepare_game()?;

    let command = self.su().ds.ss(
      "library-list @table@ chess-yellow-?"
    )?;
    let output: String = self.otter(&command)?.into();
    assert!( Regex::new("(?m)^wikimedia  *chess-yellow-K  *the yellow king$")?
             .find(&output)
             .is_some(),
             "got: {}", &output);

    let command = self.su().ds.ss(
      "library-add --lib wikimedia @table@ chess-blue-?"
    )?;
    let added = self.some_library_add(&command)?;
    assert_eq!(added.len(), 6);
  }

  #[throws(Explode)]
  fn hidden_hand(&mut self) {
    self.prepare_game()?;
    let mut alice = self.connect_player(&self.alice)?;
    let mut bob = self.connect_player(&self.bob)?;
    self.su_mut().mgmt_conn().fakerng_load(&[&"1",&"0"])?;

    let mut a_pieces = alice.pieces::<PIA>()?;
    let mut b_pieces = alice.pieces::<PIB>()?;

    // ----- alice: claim alices' hand -----

    let [hand] = a_pieces.iter_enumerated()
      .filter(|(_i,p)| {
        p.info["desc"] == otter::hand::UNCLAIMED_HAND_DESC
      })
      .map(|(i,_)| i)
      .collect::<ArrayVec<[_;1]>>()
      .into_inner().unwrap();
    dbgc!(&hand);

    alice.api_piece(GH::With, PuSynch((&mut a_pieces, hand)), ("k", json!({
      "opname": "claim",
      "wrc": "Unpredictable",
    })))?;

    // ----- find the pawns -----

    fn find_pawns<PI:Idx>(pieces: &PiecesSlice<PI>) -> [PI; 2] {
      let mut pawns = pieces.iter_enumerated()
        .filter(|(_i,p)| p.info["desc"].as_str().unwrap().ends_with(" pawn"))
        .map(|(i,_)| i)
        .take(2)
        .collect::<ArrayVec<[_;2]>>()
        .into_inner().unwrap();

      pawns.sort_by_key(|&p| -pieces[p].pos.x());
      dbgc!(pawns)
    }

    let a_pawns = find_pawns(a_pieces.as_slice());
    let b_pawns = find_pawns(b_pieces.as_slice());
    // at this point the indices correspond

    bob.synch()?;

    // ----- alice: move pawns into alice's hand -----

    for (&pawn, &xoffset) in izip!(&a_pawns, [10,20].iter()) {
      let pos = (a_pieces[hand].pos + PosC::new(xoffset, 0))?;
      alice.api_piece(GH::With, (&mut a_pieces, pawn), pos)?;
    }

    alice.synchu(&mut a_pieces)?;
    bob.synchu(&mut b_pieces)?;

    for &p in &b_pawns {
      let b_pos = &b_pieces[p].pos;
      let got = a_pawns.iter().find(|&&p| &a_pieces[p].pos == b_pos);
      assert_eq!(got, None);
    }

    // ----- alice: move one pawn within alice's hand -----

    {
      let p = a_pawns[0];
      let alice_move_to = (a_pieces[p].pos + PosC::new(5,5))?;
      let mut a_p = (&mut a_pieces, p);

      alice.api_piece(GH::Grab, PuSynch(&mut a_p), ())?;
      bob.synchx(Some(&mut b_pieces), None, |_sess, gen, k, v| {
        dbg!(gen, k, v);
        if k == "Log" {
          let m = v["logent"]["html"].as_str().unwrap();
          for bad in &["black","white"] {
            dbgc!(m);
            assert!(! m.contains(bad));
          }
        }
      })?;

      alice.api_piece(GH::Raw, &mut a_p, alice_move_to)?;
      bob.synchx(Some(&mut b_pieces), None, |_sess, gen, k, v| {
        dbg!(gen, k, v);

        if_chain! {
          if k == "Log";
          if let Some(html) = (|| Some({
            v
              .as_object()?
              .get("logent")?
              .as_object()?
              .get("html")?
              .as_str()?
          }))();
          if html.starts_with(SYNCH_LOGENTRY_PREFIX.as_html_str());
          then { return; }
        }

        panic!("bob saw something when alice moved displaced occulted");
      })?;

      alice.api_piece(GH::Ungrab, a_p, ())?;
      alice.synchu(&mut a_pieces)?;
      bob.synchu(&mut b_pieces)?;
    }

    // ----- alice: move one pawn out of alice's hand -----

    alice.api_piece(GH::With,
                    (&mut a_pieces, a_pawns[0]),
                    PosC::new( 15, 20 ))?;

    alice.synchu(&mut a_pieces)?;
    bob.synchu(&mut b_pieces)?;
    assert_eq!(b_pieces[b_pawns[1]].pos,
               a_pieces[a_pawns[0]].pos);

    let command = self.su().ds.ss("reset @table@ demo")?;
    self.otter(&command)?;
  }

  #[throws(Explode)]
  fn specs(&mut self) {
    struct Specs {
      def: String,
      ents: Box<dyn Iterator<Item=String>>,
    }
    impl Specs {
      fn next(&mut self) -> (bool, String) {
        if let Some(y) = self.ents.next() { (true, y) }
        else { (false, self.def.clone()) }
      }
    }
    let specs = |mid, def| {
      let sv = self.su().ds.also(&[("mid",mid),("def",def)]);
      let def = sv.subst("@specs@/@def@.@mid@.toml").unwrap();
      let pat = sv.subst("@specs@/*.@mid@.toml").unwrap();
      let ents = glob::glob(&pat).unwrap()
        .map(|s| s.unwrap().to_str().unwrap().to_owned());
      let ents = Box::new(ents);
      Specs { def, ents }
    };
    let mut perms = specs("table", "private");
    let mut games = specs("game",  "demo");
    loop {
      let (py, perm) = perms.next();
      let (gy, game) = games.next();
      if !(py || gy) { break }
      let command = self.su().ds.also(&[("game",&game),("perm",&perm)])
        .ss("reset --reset-table @perm@ @table@ @game@")?;
      self.otter(&command).context(perm).context(game)?;
    }
  }

  #[throws(Explode)]
  fn put_back(&mut self) {
    // Put things back for the ad-hoc human tester
    self.prepare_game()?;
    let su = self.su();
    su.ds.setup_static_users(&mut *su.mgmt_conn.borrow_mut(), default())?;
  }

  #[throws(Explode)]
  fn save_load(&mut self) {
    {
      let mut su = self.su_rc.borrow_mut();
      let old_pid = su.server_child.id() as nix::libc::pid_t;
      nix::sys::signal::kill(nix::unistd::Pid::from_raw(old_pid),
                             nix::sys::signal::SIGTERM)?;
      let st = dbgc!(su.server_child.wait()?);
      assert_eq!(st.signal(), Some(nix::sys::signal::SIGTERM as i32));
      su.restart_gameserver()?;
    }
    let alice = self.connect_player(&self.alice)?;
    let pieces = alice.pieces::<PIA>()?;
    dbgc!(pieces);
  }
}

#[throws(Explode)]
fn tests(mut c: Ctx) {
  test!(c, "library-load", c.chdir_root(|c| c.library_load() ));
  test!(c, "hidden-hand",                   c.hidden_hand()  ?);
  test!(c, "specs",        c.chdir_root(|c| c.specs()        ));
  test!(c, "put-back",                      c.put_back()     ?);
  test!(c, "save-load",                     c.save_load()    ?);
}

#[throws(Explode)]
pub fn main() {
  tests(UsualCtx::setup()?)?;
}
