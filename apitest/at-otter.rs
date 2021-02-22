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

impl Player {
  #[throws(AE)]
  fn connect(&self) {
    let body = reqwest::blocking::get(&self.url)?.text()?;
    let dom = scraper::Html::parse_document(&body);
    dbg!(&body, &dom);
    let clid = dom
      .select(&"#loading_token".try_into().unwrap())
      .next().unwrap()
      .value().attr("data-ptoken");
    dbg!(&clid);
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

    self.alice.connect()?;
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
