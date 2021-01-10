// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use otter::imports::*;

use structopt::StructOpt;

#[derive(Debug,Clone)]
#[derive(StructOpt)]
pub struct Opts {
  #[structopt(long="--libs", default_value="library/*.toml")]
  libs: String,

  #[structopt(long="--items", default_value="*")]
  items: String,
}

#[throws(anyhow::Error)]
fn main() {
  let opts = Opts::from_args();
  let libs = shapelib::Config1::PathGlob(opts.libs.clone());
  shapelib::load(&vec![libs])?;
/*
  let name = a.next().unwrap();
  let dirname = a.next().unwrap();
  let catalogue = format!("{}.toml", &dirname);
  let e = shapelib::Explicit1 {
    name, dirname, catalogue
  };
  shapelib::load1(&e).unwrap();
*/
}
