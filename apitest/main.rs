// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use otter_api_tests::*;

pub use std::cell::{RefCell, RefMut};
pub use std::rc::Rc;

pub type Setup = Rc<RefCell<SetupCore>>;

pub use index_vec::Idx;

struct TrackWantedTestsGuard<'m>(RefMut<'m, SetupCore>);
deref_to_field_mut!{TrackWantedTestsGuard<'_>,
                    TrackWantedTests,
                    0.wanted_tests}

#[allow(dead_code)]
struct UsualCtx {
  opts: Opts,
  su_rc: Setup,
  spec: GameSpec,
  alice: Player,
  bob: Player,
  prctx: PathResolveContext,
  has_lib_markers: bool,
}

impl UsualCtx {
  pub fn su(&self) -> std::cell::Ref<SetupCore>{ RefCell::borrow(&self.su_rc) }
  pub fn su_mut(&self) -> RefMut<SetupCore> { self.su_rc.borrow_mut() }

  pub fn wanted_tests(&self) -> TrackWantedTestsGuard {
    TrackWantedTestsGuard(self.su_mut())
  }
}

#[derive(Debug)]
struct Player {
  pub nick: &'static str,
  url: String,
}

struct Session {
  pub nick: &'static str,
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

  #[throws(Explode)]
  pub fn parse_html(resp: reqwest::blocking::Response) -> Html {
    assert_eq!(resp.status(), 200);
    let body = resp.text()?;
    let dom = scraper::Html::parse_document(&body);
    //dbgc!(&&dom);
    dom
  }

  #[ext(pub, name=RequestBuilderExt)]
  impl reqwest::blocking::RequestBuilder {
    #[throws(Explode)]
    fn send(self) -> reqwest::blocking::Response { self.send()? }

    #[throws(Explode)]
    fn send_parse_html(self) -> Html {
      let resp = self.send()?;
      parse_html(resp)?
    }
  }
}

use scraper_ext::{HtmlExt, RequestBuilderExt};

type Update = JsV;

#[throws(Explode)]
fn updates_parser<R:Read>(input: R, out: &mut mpsc::Sender<Update>) {
  let mut accum: HashMap<String, String> = default();
  for l in BufReader::new(input).lines() {
    let l = l?;
    if ! l.is_empty() {
      let mut l = l.splitn(2, ':');
      let lhs = l.next().unwrap();
      let rhs = l.next().unwrap();
      let rhs = rhs.trim_start();
      let () = accum.insert(lhs.to_string(), rhs.to_string())
        .is_none().expect("duplicate field");
      continue;
    }
    let entry = mem::take(&mut accum);
    #[allow(unused_variables)] let accum = (); // stops accidental use of accum
    if entry.get("event").map(String::as_str) == Some("commsworking") {
      eprintln!("commsworking: {}", entry["data"]);
    } else if let Some(event) = entry.get("event") {
      panic!("unexpected event: {}", event);
    } else {
      let update = &entry["data"];
      let update = serde_json::from_str(update).unwrap();
      if out.send(update).is_err() { break }
    }
  }
}

impl UsualCtx {
  #[throws(Explode)]
  fn connect_player<'su>(&self, player: &Player) -> Session {
    let client = reqwest::blocking::Client::new();
    let loading = client.get(&player.url).send_parse_html()?;
    let ptoken = loading.e_attr("#loading_token", "data-ptoken").unwrap();
    dbgc!(&ptoken);

    let session = client.post(&self.su().ds.subst("@url@/_/session/Portrait")?)
      .json(&json!({ "ptoken": ptoken }))
      .send_parse_html()?;

    let ctoken = session.e_attr("#main-body", "data-ctoken").unwrap();
    dbgc!(&ctoken);

    let gen: Generation = Generation(
      session.e_attr("#main-body", "data-gen").unwrap()
        .parse().unwrap()
    );
    dbgc!(gen);

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
      nick: player.nick,
      client, gen,
      cseq: 42,
      ctoken: RawToken(ctoken.to_string()),
      dom: session,
      updates: crecv,
      su_rc: self.su_rc.clone(),
    }
  }

  pub fn chdir_root<F>(&mut self, f: F)
  where F: FnOnce(&mut Self) -> Result<(),Explode>
  {
    let tmp = self.su().ds.abstmp.clone();
    env::set_current_dir("/").expect("cd /");
    self.prctx = PathResolveContext::RelativeTo(tmp.clone());
    f(self).expect("run test");
    env::set_current_dir(&tmp).expect("cd back");
    self.prctx = default();
  }
}

mod pi {
  use otter::prelude::define_index_type;
  define_index_type!{ pub struct PIA = usize; }
  define_index_type!{ pub struct PIB = usize; }
}
pub use pi::*;

type Pieces<PI> = IndexVec<PI, PieceInfo<JsV>>;
type PiecesSlice<PI> = IndexSlice<PI,[PieceInfo<JsV>]>;

#[derive(Debug,Clone)]
pub struct PieceInfo<I> {
  id: String,
  pos: Pos,
  info: I,
}

impl Session {
  #[throws(Explode)]
  fn pieces<PI:Idx>(&self) -> Pieces<PI> {
    let pieces = self.dom
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
      .collect();
    let nick = self.nick;
    dbgc!(nick, &pieces);
    pieces
  }

  #[throws(Explode)]
  fn api_piece_op_single<O:PieceOp>(&mut self, piece: &str, o: O) {
    let (opname, payload) = if let Some(o) = o.api() { o } else { return };

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
        "op": payload,
      }))
      .send()?;
    assert_eq!(resp.status(), 200);
  }

  #[throws(Explode)]
  fn api_piece<P:PieceSpecForOp, O:PieceOp>(
    &mut self, g: GrabHow, mut p: P, o: O
  ) {
    if let GH::With | GH::Grab = g {
      self.api_piece_op_single(p.id(), ("grab", json!({})))?;
    }
    if let Some(u) = p.for_update() {
      o.update(u);
    }
    {
      self.api_piece_op_single(p.id(), o)?;
    }
    if let Some(s) = p.for_synch() {
      self.synchu(s)?;
    }
    if let GH::With | GH::Ungrab = g {
      self.api_piece_op_single(p.id(), ("ungrab", json!({})))?;
    }
  }

  #[throws(Explode)]
  fn await_update<
    R,
    F: FnMut(&mut Session, Generation, &str, &JsV) -> Option<R>,
    G: FnMut(&mut Session, Generation) -> Option<R>,
    E: FnMut(&mut Session, Generation, &JsV)
             -> Result<Option<R>, AE>
   > (&mut self, mut g: G, mut f: F, mut ef: Option<E>) -> R {
    let nick = self.nick;
    'overall: loop {
      let update = self.updates.recv()?;
      let update = update.as_array().unwrap();
      let new_gen = Generation(
        update[0]
          .as_i64().unwrap()
          .try_into().unwrap()
      );
      self.gen = new_gen;
      dbgc!(nick, new_gen);
      if let Some(y) = g(self, new_gen) { break 'overall y }
      for ue in update[1].as_array().unwrap() {
        let (k,v) = ue.as_object().unwrap().iter().next().unwrap();
        dbgc!(nick, k, &v);
        if let Some(y) = {
          if k != "Error" {
            f(self, new_gen, k, v)
          } else if let Some(ef) = &mut ef {
            ef(self, new_gen, v)?
          } else {
            panic!("synch error: {:?}", &(k, v));
          }
        } { break 'overall y }
      }
    }
  }

  #[throws(Explode)]
  fn synchx<
    PI: Idx,
    F: FnMut(&mut Session, Generation, &str, &JsV),
  > (&mut self,
     mut pieces: Option<&mut Pieces<PI>>,
     ef: Option<&mut dyn FnMut(&mut Session, Generation, &JsV)
                               -> Result<(), AE>>,
     mut f: F
  ) {
    let exp = {
      self.su_rc.borrow_mut().mgmt_conn()
        .game_synch(TABLE.parse().unwrap())?
    };
    let efwrap = ef.map(|ef| {
      move |s: &mut _, g, v: &_| { ef(s,g,v)?; Ok::<_,AE>(None) }
    });
    self.await_update(
      |_session, gen      | (gen == exp).as_option(),
      | session, gen, k, v| {
        if let Some(pieces) = pieces.as_mut() {
          update_update_pieces(&session.nick, pieces, k, v);
        }
        f(session,gen,k,v);
        None
      },
      efwrap,
    )?;
  }

  #[throws(Explode)]
  fn synchu<PI:Idx>(&mut self, pieces: &mut Pieces<PI>) {
    self.synchx(Some(pieces), None, |_session, _gen, _k, _v| ())?;
  }

  #[throws(Explode)]
  fn synch(&mut self) {
    self.synchx::<PIA,_>(None, None, |_session, _gen, _k, _v|())?;
  }
}

pub fn update_update_pieces<PI:Idx>(
  nick: &str,
  pieces: &mut Pieces<PI>,
  k: &str, v: &JsV
) {
  if k != "Piece" { return }
  let v = v.as_object().unwrap();
  let piece = v["piece"].as_str().unwrap();
  let p = pieces.iter_mut().find(|p| p.id == piece);
  if_let!{ Some(p) = p; else return }
  let (op, d) = v["op"].as_object().unwrap().iter().next().unwrap();

  fn coord(j: &JsV) -> Pos {
    PosC::from_iter_2(
      j.as_array().unwrap().iter()
        .map(|n| n.as_i64().unwrap().try_into().unwrap())
    )
  }

  match op.as_str() {
    "Move" => {
      p.pos = coord(d);
    },
    "Modify" | "ModifyQuiet" => {
      let d = d.as_object().unwrap();
      p.pos = coord(&d["pos"]);
      for (k,v) in d {
        p.info
          .as_object_mut().unwrap()
          .insert(k.to_string(), v.clone());
      }
    },
    _ => {
      panic!("unknown op {:?} {:?}", &op, &d);
    },
  };
  dbgc!(nick, k,v,p);
}

pub type PieceOpData = (&'static str, JsV);
pub trait PieceOp: Debug {
  fn api(&self) -> Option<PieceOpData>;
  fn update(&self, _pi: &mut PieceInfo<JsV>) { info!("no update {:?}", self) }
}
impl PieceOp for PieceOpData {
  fn api(&self) -> Option<PieceOpData> { Some((self.0, self.1.clone())) }
}
impl PieceOp for Pos {
  fn api(&self) -> Option<PieceOpData> { Some(("m", json![self.coords])) }
  fn update(&self, pi: &mut PieceInfo<JsV>) { pi.pos = *self }
}
impl PieceOp for () {
  fn api(&self) -> Option<PieceOpData> { None }
  fn update(&self, _pi: &mut PieceInfo<JsV>) {  }
}

pub trait PieceSpecForOp: Debug {
  fn id(&self) -> &str;
  type PI: Idx;
  fn for_update(&mut self) -> Option<&mut PieceInfo<JsV>> { None }
  fn for_synch(&mut self) -> Option<&mut Pieces<Self::PI>> { None }
}

impl PieceSpecForOp for str {
  type PI = PIA;
  fn id(&self) -> &str { self }
}
impl PieceSpecForOp for &String {
  type PI = PIA;
  fn id(&self) -> &str { self }
}

type PuUp<'pcs, PI> = (&'pcs mut Pieces<PI>, PI);
#[derive(Debug)]
/// Synchronise after op but before any ungrab.
pub struct PuSynch<T>(T);

macro_rules! impl_PieceSpecForOp {
  ($($amp:tt $mut:tt)?) => {

    impl<PI:Idx> PieceSpecForOp for $($amp $mut)? PuUp<'_, PI> {
      type PI = PI;
      fn id(&self) -> &str { &self.0[self.1].id }
      fn for_update(&mut self) -> Option<&mut PieceInfo<JsV>> {
        Some(&mut self.0[self.1])
      }
    }

    impl<PI:Idx> PieceSpecForOp for PuSynch<$($amp $mut)? PuUp<'_,PI>> {
      type PI = PI;
      fn id(&self) -> &str { self.0.id() }
      fn for_update(&mut self) -> Option<&mut PieceInfo<JsV>> {
        self.0.for_update()
      }
      fn for_synch(&mut self) -> Option<&mut Pieces<PI>> {
        Some(self.0.0)
      }
    }
  }
}
impl_PieceSpecForOp!{}
impl_PieceSpecForOp!{&mut}

#[derive(Debug,Copy,Clone)]
pub enum GrabHow { Raw, Grab, Ungrab, With }
pub use GrabHow as GH;


impl UsualCtx {
  #[throws(AE)]
  pub fn otter<S:AsRef<str>>(&mut self, args: &[S]) -> OtterOutput {
    let args: Vec<String> =
      ["--account", "server:"].iter().cloned().map(Into::into)
      .chain(args.iter().map(|s| s.as_ref().to_owned()))
      .collect();
    self.su().ds.otter_prctx(&self.prctx, &args)?
  }

  #[throws(Explode)]
  pub fn prepare_game(&mut self) {
    prepare_game(&self.su().ds, &self.prctx, TABLE)?;
    self.has_lib_markers = false;
  }

  #[throws(AE)]
  pub fn reset_game<S:AsRef<str>>(&mut self, args: &[S]) -> OtterOutput {
    self.has_lib_markers = false;
    self.otter(args)?
  }

  #[throws(Explode)]
  fn some_library_add(&mut self, command: &[String]) -> Vec<String> {
    let mut session = if ! dbgc!(self.has_lib_markers) {
      let add_err = self.otter(command)
        .expect_err("library-add succeeded after reset!");
      assert_eq!(add_err.downcast::<ExitStatusError>()?.0.code(),
                 Some(EXIT_NOTFOUND));

      let mut session = self.connect_player(&self.alice)?;
      let pieces = session.pieces::<PIA>()?;
      let llm = pieces.into_iter()
        .filter(|pi| pi.info["desc"] == "a library load area marker")
        .collect::<ArrayVec<_>>();
      let llm: [_;2] = llm.into_inner().unwrap();
      dbgc!(&llm);

      for (llm, &pos) in izip!(&llm, [PosC::new(5,5), PosC::new(50,25)].iter())
      {
        session.api_piece(GH::With, &llm.id, pos)?;
      }
      self.has_lib_markers = true;

      session.synch()?;
      session
    } else {
      self.connect_player(&self.alice)?
    };

    self.otter(&command)
      .expect("library-add failed after place!");

    let mut added = vec![];
    session.synchx::<PIA,_>(None, None,
      |_session, _gen, k, v| if_chain! {
        if k == "Piece";
        let piece = v["piece"].as_str().unwrap().to_string();
        let op = v["op"].as_object().unwrap();
        if let Some(_) = op.get("Insert");
        then { added.push(piece); }
      }
    )?;

    dbgc!(&added);
    added
  }

  #[throws(Explode)]
  fn stop_and_restart_server(&mut self) {
    let mut su = self.su_rc.borrow_mut();
    let old_pid = su.server_child.id() as nix::libc::pid_t;
    nix::sys::signal::kill(nix::unistd::Pid::from_raw(old_pid),
                           nix::sys::signal::SIGTERM)?;
    let st = dbgc!(su.server_child.wait()?);
    assert_eq!(st.signal(), Some(nix::sys::signal::SIGTERM as i32));
    su.restart_gameserver()?;
  }

  #[throws(Explode)]
  pub fn check_library_item(&mut self, itemlib: &str, item: &str,
                        desc: &str) {
    let ds = self.su().ds.also(&[
      ("itemlib", itemlib),
      ("item",    item   ),
    ]);
    let command = ds.ss("library-add --lib @itemlib@ @table@ @item@")?;
    let added = self.some_library_add(&command)?;
    assert_eq!( added.len(), 1 );

    let output: String = self.otter(&ds.ss("list-pieces @table@")?)?.into();
    assert_eq!( Regex::new(
      &format!(
        r#"(?m)(?:[^\w-]|^){}[^\w-].*\W{}(?:\W|$)"#,
        item, desc,
      )
    )?
                .find_iter(&output).count(),
                1,
                "got: {}", &output);
  }

  #[throws(Explode)]
  pub fn upload_and_check_bundle(&mut self, bundle_stem: &str,
                                 libname: &str, item: &str,
                                 desc: &str)
  {
    let ds = self.su().ds.also(&[("bundle_stem", &bundle_stem)]);
    let bundle_file = ds.subst("@examples@/@bundle_stem@.zip")?;
    let ds = ds.also(&[("bundle", &bundle_file)]);
    self.otter(&ds.ss("upload-bundle @table@ @bundle@")?)?;
    let mut bundles = self.otter(&ds.ss("list-bundles @table@")?)?;
    let bundles = String::from(&mut bundles);
    assert!(bundles.starts_with("00000.zip Loaded"));
    self.otter(&ds.ss("download-bundle @table@ 0")?)?;
    let st = Command::new("cmp").args(&[&bundle_file, "00000.zip"]).status()?;
    if ! st.success() { panic!("cmp failed {}", st) }

    self.check_library_item(libname,item,desc)?;

    self.stop_and_restart_server()?;

    let id =
      self.su().mgmt_conn().list_pieces()?
      .0.iter()
      .find(|pi| pi.itemname.as_str() == item)
      .unwrap()
      .piece;
    self.su().mgmt_conn().alter_game(vec![MGI::DeletePiece(id)], None)?;

    self.check_library_item(libname,item,desc)?;

    self.otter(&ds.ss("clear-game @table@")?)?;
    self.reset_game(&ds.ss("reset @table@ demo")?)?;
  }
}

impl UsualCtx {
  #[throws(AE)]
  pub fn setup() -> Self {
    let (opts, _instance, su) = setup_core(
      &[module_path!()],
    )?;
    let spec = su.ds.game_spec_data()?;
    let mut mc = su.mgmt_conn();
    let [alice, bob]: [Player; 2] =
      su.ds.setup_static_users(&mut mc, default())?
      .into_iter().map(|sus| Player { nick: sus.nick, url: sus.url })
      .collect::<ArrayVec<_>>().into_inner().unwrap();
    drop(mc);

    let su_rc = Rc::new(RefCell::new(su));
    UsualCtx {
      opts, spec, su_rc, alice, bob,
      has_lib_markers: false,
      prctx: default(),
    }
  }
}

portmanteau_has!("at-otter.rs",   at_otter);
portmanteau_has!("at-bundles.rs", at_bundles);

#[throws(AE)]
fn main() { portmanteau_main("at")? }
