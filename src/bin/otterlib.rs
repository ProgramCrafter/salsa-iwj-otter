// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use otter::imports::*;

pub use shapelib::*;

use structopt::StructOpt;

#[derive(Debug,Clone)]
#[derive(StructOpt)]
pub struct Opts {
  #[structopt(long="--libs", default_value="library/*.toml")]
  libs: String,

  #[structopt(long="--items", default_value="*")]
  items: String,

  #[structopt(flatten)]
  output: OutputKind,
}

#[derive(StructOpt,Debug,Clone,Copy)]
pub enum OutputKind {
  List,
}

#[throws(anyhow::Error)]
fn main() {
  let opts = Opts::from_args();

  env_logger::Builder::new()
//    .format_timestamp_micros()
//    .format_level(true)
//    .filter_module(exe_module_path, log::LevelFilter::Debug)
    .filter_level(log::LevelFilter::Info)
    .parse_env("OTTERLIB_LOG")
    .init();

  let libs = Config1::PathGlob(opts.libs.clone());
  load(&vec![libs.clone()])?;
  let libnames = libs_list();
  for lib in libnames {
    let contents = libs_lookup(&lib)?;
    let items = contents.list_glob(&opts.items)?;
    for item in items {
      println!("{}", item.line_for_list());
    }
  }
}
  
/* 
  let name = a.next().unwrap();
  let dirname = a.next().unwrap();
  let catalogue = format!("{}.toml", &dirname);
  let e = shapelib::Explicit1 {
    name, dirname, catalogue
  };
  shapelib::load1(&e).unwrap();
*/

