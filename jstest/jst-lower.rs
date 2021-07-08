// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use otter_nodejs_tests::*;

#[derive(StructOpt,Debug,Clone)]
pub struct Opts {
  pub nodejs: String,
  pub script: String,
}

#[derive(Debug,Clone)]
pub struct StartPieceSpec {
  id: &'static str,
  pinned: bool,
  moveable: PieceMoveable,
}

#[macro_export]
macro_rules! sp {
  { $id:expr, $pinned:expr, $moveable:ident } => {
    StartPieceSpec { id: $id, pinned: $pinned,
                     moveable: PieceMoveable::$moveable }
  };
}

#[derive(Debug,Clone)]
#[derive(Eq,PartialEq,Ord,PartialOrd)]
pub struct StartPiece {
  id: String,
  pinned: bool,
  moveable: PieceMoveable,
  z: ZCoord,
}

#[derive(Debug,Clone,Default)]
pub struct Tests {
  tests: IndexMap<String, Vec<StartPiece>>,
}

impl Tests {
  pub fn add_test(&mut self, name: &'static str, pieces: Vec<StartPieceSpec>) {
    let mut zm = ZCoord::default().clone_mut();
    let pieces = pieces.into_iter().map(
      |StartPieceSpec { id, pinned, moveable }| {
        let id = id.into();
        let z = zm.increment().unwrap();
        StartPiece { id, pinned, moveable, z }
      }
    ).collect_vec();
    let already = self.tests.insert(name.to_owned(), pieces);
    assert_eq!(already, None);
  }
}

#[throws(AE)]
fn main() {
  let opts = Opts::from_args();

  let mut tests = Tests::default();
  tests.add_test("simple", vec![
    sp!("1.1", false, Yes),
    sp!("2.1", false, Yes),
  ]);

  let mut cmd = Command::new(opts.nodejs);
  cmd.arg(opts.script);
  let status = cmd.status()?;
  assert!(status.success(), "{}", status);
}
