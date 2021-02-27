// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(dead_code)]
#![allow(unused_variables)]

use otter_api_tests::*;

struct Ctx {
  opts: Opts,
  su: SetupCore,
  spec: GameSpec,
  alice: Player,
  bob: Player,
}
deref_to_field!{Ctx, SetupCore, su}

#[derive(Debug)]
struct Player {
  url: String,
}

struct Session {
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
  use scraper::html::*;

  pub trait HtmlExt {
    fn select<'a,'b>(&'a self, selector: &'b Selector) -> Select<'a, 'b>;

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

  impl HtmlExt for Html {
    fn select<'a,'b>(&'a self, selector: &'b Selector) -> Select<'a, 'b> {
      self.select(selector)
    }
  }

  #[throws(AE)]
  pub fn parse_html(resp: reqwest::blocking::Response) -> Html {
    ensure_eq!(resp.status(), 200);
    let body = resp.text()?;
    let dom = scraper::Html::parse_document(&body);
    //dbg!(&&dom);
    dom
  }

  pub trait RequestBuilderExt: Sized {
    fn send(self) -> Result<reqwest::blocking::Response, AE>;

    #[throws(AE)]
    fn send_parse_html(self) -> Html {
      let resp = self.send()?;
      parse_html(resp)?
    }
  }

  impl RequestBuilderExt for reqwest::blocking::RequestBuilder {
    #[throws(AE)]
    fn send(self) -> reqwest::blocking::Response { self.send()? }
  }
}

use scraper_ext::{HtmlExt, RequestBuilderExt};

type Update = serde_json::Value;

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
      dbg!(&update);
      if out.send(update).is_err() { break }
    }
  }
}

impl Ctx {
  #[throws(AE)]
  fn connect_player(&self, player: &Player) -> Session {
    let client = reqwest::blocking::Client::new();
    let loading = client.get(&player.url).send_parse_html()?;
    let ptoken = loading.e_attr("#loading_token", "data-ptoken").unwrap();
    dbg!(&ptoken);

    let session = client.post(&self.ds.subst("@url@/_/session/Portrait")?)
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
      &self.ds
        .also(&[("ctoken", ctoken),
                ("gen",    &gen.to_string())])
        .subst("@url@/_/updates?ctoken=@ctoken@&gen=@gen@")?
    ).send()?;

    let (mut wpipe, rpipe) = UnixStream::pair()?;
    thread::spawn(move ||{
      eprintln!("copy_to'ing");
      sse.copy_to(&mut wpipe).unwrap();
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
  fn pieces(&self) -> Vec<PieceInfo<serde_json::Value>> {
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
  fn api_piece_op(&mut self, su: &SetupCore, piece: &str,
                  opname: &str, op: serde_json::Value) {
    self.cseq += 1;
    let cseq = self.cseq;
    
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
    ensure_eq!(resp.status(), 200);
  }

  #[throws(AE)]
  fn await_update<
    R,
    F: FnMut(&mut Session, Generation, &str, &serde_json::Value) -> Option<R>,
    G: FnMut(&mut Session, Generation) -> Option<R>,
   > (&mut self, mut g: G, mut f: F) -> R {
    'overall: loop {
      let update = self.updates.recv()?;
      let update = update.as_array().unwrap();
      let new_gen = Generation(
        update[0]
          .as_i64().unwrap()
          .try_into().unwrap()
      );
      if let Some(y) = g(self, new_gen) { break 'overall y }
      for ue in update[1].as_array().unwrap() {
        let (k,v) = ue.as_object().unwrap().iter().next().unwrap();
        if let Some(y) = f(self, new_gen, k, v) { break 'overall y }
      }
    }
  }

  #[throws(AE)]
  fn synch(&mut self, su: &mut SetupCore) {
    let exp = mgmt_game_synch(&mut su.mgmt_conn, TABLE.parse().unwrap())?;
    self.await_update(
      |session, gen        | (gen == exp).as_option(),
      |_session, _gen, _k, _v| None,
    )?;
  }
}

impl Ctx {
  #[throws(AE)]
  pub fn otter<S:AsRef<str>>(&mut self, args: &[S]) {
    let args: Vec<String> =
      ["--account", "server:"].iter().cloned().map(Into::into)
      .chain(args.iter().map(|s| s.as_ref().to_owned()))
      .collect();
    self.ds.otter(&args)?;
  }

  #[throws(AE)]
  fn library_load(&mut self) {
    prepare_game(&self.ds, TABLE)?;

    let command = self.ds.ss(
      "library-add @table@ wikimedia chess-blue-?"
    )?;
    let add_err = self.otter(&command)
      .expect_err("library-add succeeded after reset!");
    ensure_eq!(add_err.downcast::<ExitStatusError>()?.0.code(),
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
      session.api_piece_op(&self.su, &llm.id, "grab", json!({}))?;
      session.api_piece_op(&self.su, &llm.id, "m", json![pos.0])?;
      session.api_piece_op(&self.su, &llm.id, "ungrab", json!({}))?;
    }

    session.synch(&mut self.su)?;

    self.otter(&command)
      .expect("library-add failed after place!");

    session.synch(&mut self.su)?;
    // xxx send api requests to move markers
    // run library-add again
  }
}

#[throws(AE)]
fn tests(mut c: Ctx) {
  test!(c, "library-load", c.library_load()?);
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
    
    tests(Ctx { opts, spec, su, alice, bob })?;
  }
  info!("ok");
}
