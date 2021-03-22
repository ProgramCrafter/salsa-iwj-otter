// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// This is otter::hidden::magic

use crate::prelude::*;

pub const ENABLED_DESC: &str = "a pickup deck (enabled)";
pub const DISABLED_DESC: &str = "a pickup deck (disabled)";

#[derive(Debug,Serialize,Deserialize)]
struct Deck {
  shape: GenericSimpleShape<(), shapelib::Rectangle>,
}

#[dyn_upcast]
impl OutlineTrait for Deck {
  delegate!{
    to self.shape {
      fn outline_path(&self, scale: f64) -> Result<Html,IE>;
      fn thresh_dragraise(&self) -> Result<Option<Coord>,IE>;
      fn bbox_approx(&self) -> Result<[Pos;2], IE>;
    }
  }
}

#[typetag::serde(name="PickupDeck")]
impl PieceSpec for piece_specs::Deck {
  #[throws(SpecError)]
  fn load(&self, _: usize, _: &mut GPiece, _ir: &InstanceRef)
          -> PieceSpecLoaded {
    let common = SimpleCommon {
      itemname: None,
      faces: self.faces.clone(),
      edges: self.edges.clone(),
      edge_width: self.edge_width,
    };
    let shape = match self.shape {
      Outline::Rectangle(r) => r,
      _ => throw!(SpecError::UnsupportedShape),
    };
    let shape = GenericSimpleShape::new(
      (),
      shape.clone(),
      "magic-pickupdeck",
      &common)?;
    if shape.count_faces() != 2 {
      throw!(SpE::WrongNumberOfFaces);
    }
    let p = Box::new(Deck {
      shape,
    }) as Box<dyn PieceTrait>;
    PieceSpecLoaded { p, occultable: None }
  }
}

impl Deck {
  fn enabled(&self, gpc: &GPiece) -> bool {
    gpc.occult.is_active()
  }

  fn current_face(&self, gpc: &GPiece) -> FaceId {
    (self.enabled(gpc) as RawFaceId).into()
  }
}

#[typetag::serde]
impl PieceTrait for Deck {
  fn nfaces(&self) -> RawFaceId { 1 }
  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, gpc: &GPiece,
               _gs: &GameState, _vpid: VisiblePieceId) {
    let face = self.current_face(gpc);
    self.shape.svg_piece_raw(f, face, &mut |_|Ok::<_,IE>(()))?;
  }

  #[throws(IE)]
  fn describe_html(&self, gpc: &GPiece) -> Html {
    Html::lit(
      if self.enabled(gpc) { ENABLED_DESC } else { DISABLED_DESC }
    )
  }

  delegate!{
    to self.shape {
      fn itemname(&self) -> &str;
    }
  }

  #[throws(InternalError)]
  fn add_ui_operations(&self, upd: &mut Vec<UoDescription>,
                       _gs: &GameState, gpc: &GPiece) {
    upd.push(
      if self.enabled(gpc) {
        UoDescription {
          kind: UoKind::Piece,
          def_key: 'A',
          opname: "activate".to_owned(),
          desc: Html::lit("Enable pickup deck"),
          wrc: WRC::Unpredictable,
        }
      } else {
        UoDescription {
          kind: UoKind::Piece,
          def_key: 'S',
          opname: "deactivate".to_owned(),
          desc: Html::lit("Disable pickup deck"),
          wrc: WRC::Unpredictable,
        }
      }
    )
  }
}
