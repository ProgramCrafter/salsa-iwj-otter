// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;
use shapelib::Rectangle;

const W: Coord = 50;
const H: Coord = 20;
const OUTLINE: Rectangle = Rectangle { xy: PosC([W as f64, H as f64]) };

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct ChessClock { // spec
}

#[derive(Debug,Serialize,Deserialize)]
struct Clock { // state
}

#[typetag::serde]
impl PieceSpec for ChessClock {
  #[throws(SpecError)]
  fn load(&self, _: usize, _gpc: &mut GPiece) -> PieceSpecLoaded {
    let clock = Clock {

    };
    PieceSpecLoaded {
      p: Box::new(clock),
      occultable: None,
    }
  }
}

#[dyn_upcast]
impl OutlineTrait for Clock {
  delegate!{
    to OUTLINE {
      fn outline_path(&self, scale: f64) -> Result<Html, IE>;
      fn thresh_dragraise(&self) -> Result<Option<Coord>, IE>;
      fn bbox_approx(&self) -> Result<[Pos;2], IE>;
    }
  }
}

#[typetag::serde(name="ChessClock")]
impl PieceTrait for Clock {
  fn nfaces(&self) -> RawFaceId { 1 }

  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, _gpc: &GPiece, _gs: &GameState, id: VisiblePieceId) {
    dbgc!("rendering", id);
    write!( &mut f.0, r##"
        <rect fill="black" width="10" height="10"/>
    "##)?;
  }

  #[throws(IE)]
  fn describe_html(&self, _gpc: &GPiece) -> Html {
    Html::lit("the chess clock")
  }

  fn itemname(&self) -> &str { "chess-clock" }
}
