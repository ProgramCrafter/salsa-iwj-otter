// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(dead_code)]
#![allow(unused_variables)]

use otter_api_tests::*;

pub use std::cell::{RefCell, RefMut};
pub use std::rc::Rc;

type Setup = Rc<RefCell<SetupCore>>;

struct Ctx {
  opts: Opts,
  su_rc: Setup,
  spec: GameSpec,
  alice: Player,
  bob: Player,
}

impl Ctx {
  fn su(&self) -> std::cell::Ref<SetupCore> { RefCell::borrow(&self.su_rc) }
  fn su_mut(&self) -> RefMut<SetupCore> { self.su_rc.borrow_mut() }

  fn wanted_tests(&self) -> TrackWantedTestsGuard {
    TrackWantedTestsGuard(self.su_mut())
  }
}
struct TrackWantedTestsGuard<'m>(RefMut<'m, SetupCore>);
deref_to_field_mut!{TrackWantedTestsGuard<'_>,
                    TrackWantedTests,
                    0.wanted_tests}

#[derive(Debug)]
struct Player {
  url: String,
}

struct Session {
  pub su_rc: Setup,
  pub ctoken: RawToken,
  pub gen: Generation,
  pub cseq: RawClientSequence,
  pub dom: scraper::Html,
  pub updates: mpsc::Receiver<Update>,
  pub client: reqwest::blocking::Client,
}

mod scraper_ext {
  use super::*;
  use scraper::*;
  use scraper::html::{*, Html};

  #[ext(pub)]
  impl Html {
    fn select<'a,'b>(&'a self, selector: &'b Selector) -> Select<'a, 'b> {
      self.select(selector)
    }

    #[throws(as Option)]
    fn element<S>(&self, sel: S) -> ElementRef
    where S: TryInto<Selector>,
          <S as TryInto<Selector>>::Error: Debug,
    {
      self
        .select(&sel.try_into().unwrap())
        .next()?
    }

    #[throws(as Option)]
    fn e_attr<S>(&self, sel: S, attr: &str) -> &str
    where S: TryInto<Selector>,
          <S as TryInto<Selector>>::Error: Debug,
    {
      self
        .element(sel).unwrap()
        .value().attr(attr)?
    }
  }

  #[throws(AE)]
  pub fn parse_html(resp: reqwest::blocking::Response) -> Html {
    assert_eq!(resp.status(), 200);
    let body = resp.text()?;
    let dom = scraper::Html::parse_document(&body);
    //dbg!(&&dom);
    dom
  }

  #[ext(pub, name=RequestBuilderExt)]
  impl reqwest::blocking::RequestBuilder {
    #[throws(AE)]
    fn send(self) -> reqwest::blocking::Response { self.send()? }

    #[throws(AE)]
    fn send_parse_html(self) -> Html {
      let resp = self.send()?;
      parse_html(resp)?
    }
  }
}

use scraper_ext::{HtmlExt, RequestBuilderExt};

type Update = JsV;

#[throws(AE)]
fn updates_parser<R:Read>(input: R, out: &mut mpsc::Sender<Update>) {
  let mut accum: HashMap<String, String> = default();
  for l in BufReader::new(input).lines() {
    let l = l?;
    if ! l.is_empty() {
      let mut l = l.splitn(2, ':');
      let lhs = l.next().unwrap();
      let rhs = l.next().unwrap();
      let rhs = rhs.trim_start();
      let ins = accum.insert(lhs.to_string(), rhs.to_string())
        .is_none().expect("duplicate field");
      continue;
    }
    let entry = mem::take(&mut accum);
    let accum = (); // stops accidental use of accum
    if entry.get("event").map(String::as_str) == Some("commsworking") {
      eprintln!("commsworking: {}", entry["data"]);
    } else if let Some(event) = entry.get("event") {
      panic!("unexpected event: {}", event);
    } else {
      let update = &entry["data"];
      let update = serde_json::from_str(update).unwrap();
      dbgc!(&update);
      if out.send(update).is_err() { break }
    }
  }
}

impl Ctx {
  #[throws(AE)]
  fn connect_player<'su>(&self, player: &Player) -> Session {
    let client = reqwest::blocking::Client::new();
    let loading = client.get(&player.url).send_parse_html()?;
    let ptoken = loading.e_attr("#loading_token", "data-ptoken").unwrap();
    dbg!(&ptoken);

    let session = client.post(&self.su().ds.subst("@url@/_/session/Portrait")?)
      .json(&json!({ "ptoken": ptoken }))
      .send_parse_html()?;

    let ctoken = session.e_attr("#main-body", "data-ctoken").unwrap();
    dbg!(&ctoken);

    let gen: Generation = Generation(
      session.e_attr("#main-body", "data-gen").unwrap()
        .parse().unwrap()
    );
    dbg!(gen);

    let mut sse = client.get(
      &self.su().ds
        .also(&[("ctoken", ctoken),
                ("gen",    &gen.to_string())])
        .subst("@url@/_/updates?ctoken=@ctoken@&gen=@gen@")?
    ).send()?;

    let (mut wpipe, rpipe) = UnixStream::pair()?;
    thread::spawn(move ||{
      eprintln!("copy_to'ing");
      match sse.copy_to(&mut wpipe) {
        Err(re) => match (||{
          // reqwest::Error won't give us the underlying io::Error :-/
          wpipe.write_all(b"\n")?;
          wpipe.flush()?;
          Ok::<_,io::Error>(())
        })() {
          Err(pe) if pe.kind() == ErrorKind::BrokenPipe => { Ok(()) }
          Err(pe) => Err(AE::from(pe)),
          Ok(_) => Err(AE::from(re)),
        }
        Ok(_n) => Ok(()),
      }.unwrap();
      eprintln!("copy_to'd!"); 
    });

    let (mut csend, crecv) = mpsc::channel();
    thread::spawn(move ||{
      updates_parser(rpipe, &mut csend).expect("udpates parser failed")
    });

    Session {
      client, gen,
      cseq: 42,
      ctoken: RawToken(ctoken.to_string()),
      dom: session,
      updates: crecv,
      su_rc: self.su_rc.clone(),
    }
  }
}

#[derive(Debug,Clone)]
struct PieceInfo<I> {
  id: String,
  pos: Pos,
  info: I,
}

impl Session {
  #[throws(AE)]
  fn pieces(&self) -> Vec<PieceInfo<JsV>> {
    self.dom
      .element("#pieces_marker")
      .unwrap().next_siblings()
      .map_loop(|puse: ego_tree::NodeRef<scraper::Node>| {
        let puse = puse.value();
        let puse = puse.as_element().ok_or(Loop::Continue)?;
        let attr = puse.attr("data-info").ok_or(Loop::Break)?;
        let pos = Pos::from_iter(["x","y"].iter().map(|attr|{
          puse
            .attr(attr).unwrap()
            .parse().unwrap()
        })).unwrap();
        let id = puse.id.as_ref().unwrap();
        let id = id.strip_prefix("use").unwrap().to_string();
        let info = serde_json::from_str(attr).unwrap();
        Loop::ok(PieceInfo { id, pos, info })
      })
      .collect()
  }

  #[throws(AE)]
  fn api_piece_op(&mut self, piece: &str, opname: &str,
                  op: JsV) {
    self.cseq += 1;
    let cseq = self.cseq;

    let su = self.su_rc.borrow_mut();
    let resp = self.client.post(&su.ds.also(&[("opname",opname)])
                                .subst("@url@/_/api/@opname@")?)
      .json(&json!({
        "ctoken": self.ctoken,
        "piece": piece,
        "gen": self.gen,
        "cseq": cseq,
        "op": op,
      }))
      .send()?;
    assert_eq!(resp.status(), 200);
  }

  #[throws(AE)]
  fn api_with_piece_op(&mut self, piece: &str,
                       pathfrag: &str, op: JsV) {
    self.api_piece_op(piece, "grab", json!({}))?;
    self.api_piece_op(piece, pathfrag, op)?;
    self.api_piece_op(piece, "ungrab", json!({}))?;
  }

  #[throws(AE)]
  fn api_with_piece_op_synch(&mut self, piece: &str,
                             pathfrag: &str, op: JsV) {
    self.api_piece_op(piece, "grab", json!({}))?;
    self.api_piece_op(piece, pathfrag, op)?;
    self.synch()?;
    self.api_piece_op(piece, "ungrab", json!({}))?;
  }

  #[throws(AE)]
  fn await_update<
    R,
    F: FnMut(&mut Session, Generation, &str, &JsV) -> Option<R>,
    G: FnMut(&mut Session, Generation) -> Option<R>,
    E: FnMut(&mut Session, Generation, &JsV)
             -> Result<Option<R>, AE>
   > (&mut self, mut g: G, mut f: F, mut ef: Option<E>) -> R {
    'overall: loop {
      let update = self.updates.recv()?;
      let update = update.as_array().unwrap();
      let new_gen = Generation(
        update[0]
          .as_i64().unwrap()
          .try_into().unwrap()
      );
      self.gen = new_gen;
      if let Some(y) = g(self, new_gen) { break 'overall y }
      for ue in update[1].as_array().unwrap() {
        let (k,v) = ue.as_object().unwrap().iter().next().unwrap();
        if let Some(y) = {
          if k != "Error" {
            f(self, new_gen, k, v)
          } else if let Some(ef) = &mut ef {
            ef(self, new_gen, v)?
          } else {
            panic!("synch error: {:?}", &v);
          }
        } { break 'overall y }
      }
    }
  }

  #[throws(AE)]
  fn synchx<
    F: FnMut(&mut Session, Generation, &str, &JsV),
  > (&mut self,
     ef: Option<&mut dyn FnMut(&mut Session, Generation, &JsV)
                               -> Result<(), AE>>,
     mut f: F)
  {
    let exp = {
      let mut su = self.su_rc.borrow_mut();
      su.mgmt_conn.game_synch(TABLE.parse().unwrap())?
    };
    let efwrap = ef.map(|ef| {
      move |s: &mut _, g, v: &_| { ef(s,g,v)?; Ok::<_,AE>(None) }
    });
    self.await_update(
      |session, gen      | (gen == exp).as_option(),
      |session, gen, k, v| { f(session,gen,k,v); None },
      efwrap,
    )?;
  }

  #[throws(AE)]
  fn synch(&mut self) {
    self.synchx(None, |_session, _gen, _k, _v|())?;
  }
}

impl Ctx {
  #[throws(AE)]
  pub fn otter<S:AsRef<str>>(&mut self, args: &[S]) {
    let args: Vec<String> =
      ["--account", "server:"].iter().cloned().map(Into::into)
      .chain(args.iter().map(|s| s.as_ref().to_owned()))
      .collect();
    self.su().ds.otter(&args)?;
  }

  #[throws(AE)]
  fn library_load(&mut self) {
    prepare_game(&self.su().ds, TABLE)?;

    let command = self.su().ds.ss(
      "library-add @table@ wikimedia chess-blue-?"
    )?;
    let add_err = self.otter(&command)
      .expect_err("library-add succeeded after reset!");
    assert_eq!(add_err.downcast::<ExitStatusError>()?.0.code(),
               Some(EXIT_NOTFOUND));

    let mut session = self.connect_player(&self.alice)?;
    let pieces = session.pieces()?;
    dbg!(&pieces);
    let llm = pieces.into_iter()
      .filter(|pi| pi.info["desc"] == "a library load area marker")
      .collect::<ArrayVec<_>>();
    let llm: [_;2] = llm.into_inner().unwrap();
    dbg!(&llm);

    for (llm, pos) in izip!(&llm, [PosC([5,5]), PosC([50,25])].iter()) {
      session.api_with_piece_op(&llm.id, "m", json![pos.0])?;
    }

    session.synch()?;

    self.otter(&command)
      .expect("library-add failed after place!");

    let mut added = vec![];
    session.synchx(None,
      |session, gen, k, v| if_chain! {
        if k == "Piece";
        let piece = v["piece"].as_str().unwrap().to_string();
        let op = v["op"].as_object().unwrap();
        if let Some(_) = op.get("Insert");
        then { added.push(piece); }
      }
    )?;
    dbg!(&added);
    assert_eq!(added.len(), 6);
  }

  #[throws(AE)]
  fn hidden_hand(&mut self) {
    prepare_game(&self.su().ds, TABLE)?;
    let mut alice = self.connect_player(&self.alice)?;
    let mut bob = self.connect_player(&self.bob)?;
    self.su_mut().mgmt_conn.fakerng_load(&[&"1"])?;

    let mut a_pieces = alice.pieces()?;

    let [hand] = a_pieces.iter().enumerate()
      .filter(|(i,p)| p.info["desc"] == otter::hand::UNCLAIMED_DESC)
      .map(|(i,_)| i)
      .collect::<ArrayVec<[_;1]>>()
      .into_inner().unwrap();
    dbg!(&hand);

    alice.api_with_piece_op_synch(&a_pieces[hand].id, "k", json!({
      "opname": "claim",
      "wrc": "Unpredictable",
    }))?;

    fn find_pawns(pieces: &[PieceInfo<JsV>]) -> [usize; 2] {
      let pawns = pieces.iter().enumerate()
        .filter(|(i,p)| p.info["desc"].as_str().unwrap().ends_with(" pawn"))
        .map(|(i,_)| i)
        .take(2)
        .collect::<ArrayVec<[_;2]>>()
        .into_inner().unwrap();
      dbg!(pawns)
    }

    let a_pawns = find_pawns(&mut a_pieces);

    bob.synch()?;

    for (&pawn, &xoffset) in izip!(&a_pawns, [10,20].iter()) {
      alice.api_with_piece_op(&a_pieces[pawn].id, "m", json![
        (a_pieces[hand].pos + PosC([xoffset, 0]))?.0
      ])?;
    }

    alice.synch()?;
    bob.synchx(None, |session, gen, k, v| {
      dbgc!(k,v);
    })?;

    // to repro a bug, have Bob move the RHS pawn out again

    self.su_mut().mgmt_conn.fakerng_unfake()?;
  }
}

#[throws(AE)]
fn tests(mut c: Ctx) {
  test!(c, "library-load", c.library_load()?);
  test!(c, "hidden-hand", c.hidden_hand()?);
}

#[throws(AE)]
fn main() {
  {
    let (opts, _cln, instance, mut su) = setup_core(&[module_path!()])?;
    let spec = su.ds.game_spec_data()?;
    let [alice, bob]: [Player; 2] = su.ds.setup_static_users(
      default(),
      |sus| Ok(Player { url: sus.url })
    )?
      .try_into().unwrap();
    
    let su_rc = Rc::new(RefCell::new(su));
    tests(Ctx { opts, spec, su_rc, alice, bob })?;
  }
  info!("ok");
}
