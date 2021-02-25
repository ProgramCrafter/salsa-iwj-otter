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
  pub dom: scraper::html::Html,
  pub updates: UnixStream,
}

impl Ctx {
  #[throws(AE)]
  fn connect_player(&self, player: &Player) -> Session {
    let client = reqwest::blocking::Client::new();

    let resp = client.get(&player.url).send()?;
    ensure_eq!(resp.status(), 200);
    let body = resp.text()?;
    let loading = scraper::Html::parse_document(&body);
    //dbg!(&body, &dom);
    let ptoken = loading
      .select(&"#loading_token".try_into().unwrap())
      .next().unwrap()
      .value().attr("data-ptoken")
      .unwrap();
    dbg!(&ptoken);

    let resp = client.post(&self.ds.subst("@url@/_/session/Portrait")?)
      .json(&json!({ "ptoken": ptoken }))
      .send()?;
    ensure_eq!(resp.status(), 200);
    let body = resp.text()?;
    let session = scraper::Html::parse_document(&body);
    //dbg!(&body, &dom);

    let ctoken = session
      .select(&"#main-body".try_into().unwrap())
      .next().unwrap()
      .value().attr("data-ctoken")
      .unwrap();
    dbg!(&ctoken);

    let gen: Generation = Generation(session
      .select(&"#main-body".try_into().unwrap())
      .next().unwrap()
      .value().attr("data-gen")
      .unwrap()
      .parse().unwrap());
    dbg!(gen);

    let mut sse = client.get(
      &self.ds
        .also(&[("ctoken", ctoken),
                ("gen",    &gen.to_string())])
        .subst("@url@/_/updates?ctoken=@ctoken@&gen=@gen@")?
    ).send()?;

    let (mut writer, reader) = UnixStream::pair()?;
    thread::spawn(move ||{
      eprintln!("copy_to'ing");
      sse.copy_to(&mut writer).unwrap();
      eprintln!("copy_to'd!"); 
    });

    Session { dom: session, updates: reader }
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

    self.otter(&self.ds.ss("library-add @table@ wikimedia chess-blue-?")?)?;

    self.connect_player(&self.alice)?;
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
