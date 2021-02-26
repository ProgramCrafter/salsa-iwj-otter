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
  pub dom: scraper::Html,
  pub updates: UnixStream,
}

mod scraper_ext {
  use super::*;
  use scraper::*;
  use scraper::html::*;

  pub trait HtmlExt {
    fn select<'a,'b>(&'a self, selector: &'b Selector) -> Select<'a, 'b>;

    #[throws(as Option)]
    fn e_attr<S>(&self, sel: S, attr: &str) -> &str
    where S: TryInto<Selector>,
          <S as TryInto<Selector>>::Error: Debug,
    {
      self
        .select(&sel.try_into().unwrap())
        .next()?
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

    let add_err = self.otter(&self.ds.ss(
      "library-add @table@ wikimedia chess-blue-?"
    )?)
      .expect_err("library-add succeeded after reset!");
    ensure_eq!(add_err.downcast::<ExitStatusError>()?.0.code(),
               Some(EXIT_NOTFOUND));
    // xxx find load markers ids

    let _session = self.connect_player(&self.alice)?;
    // xxx find load markers' locations
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
