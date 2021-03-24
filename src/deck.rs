// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

pub const CORE_DESC: &str = "a pickup deck";
pub const ENABLED_DESC: &str = "a pickup deck (enabled)";
pub const DISABLED_DESC: &str = "a pickup deck (disabled)";

#[derive(Debug,Serialize,Deserialize)]
struct Deck {
  shape: GenericSimpleShape<(), shapelib::Rectangle>,
  label: Option<piece_specs::PieceLabel>,
}

#[derive(Debug,Clone,Copy,Ord,PartialOrd,Eq,PartialEq)]
enum State {
  Disabled,
  Enabled,
}
use State::*;

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
      label: self.label.clone(),
    }) as Box<dyn PieceTrait>;
    PieceSpecLoaded { p, occultable: None }
  }
}

impl Deck {
  #[throws(IE)]
  fn state(&self, gpc: &GPiece, _goccults: &GameOccults) -> State {
    if gpc.occult.is_active() { Enabled } else { Disabled }
  }

  #[throws(IE)]
  fn current_face(&self, gpc: &GPiece, goccults: &GameOccults) -> FaceId {
    RawFaceId::from(match self.state(gpc, goccults)? {
      Disabled => 0,
      Enabled => 1,
    }).into()
  }
}

#[typetag::serde]
impl PieceTrait for Deck {
  fn nfaces(&self) -> RawFaceId { 1 }
  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, gpc: &GPiece,
               gs: &GameState, _vpid: VisiblePieceId) {
    let face = self.current_face(gpc, &gs.occults)?;
    self.shape.svg_piece_raw(f, face, &mut |_|Ok::<_,IE>(()))?;
    if_chain! {
      if let Some(label) = &self.label;
      if let Some(count) = gpc.occult.active_total_ppieces(&gs.occults)?;
      then {
        label.svg(f,
                  &self.shape.outline,
                  self.shape.edges.get(0), 
                  &Html(count.to_string()))?;
      }
    }
  }

  #[throws(IE)]
  fn describe_html(&self, gpc: &GPiece, goccults: &GameOccults) -> Html {
    Html::lit(
      match self.state(gpc, goccults)? {
        Disabled => DISABLED_DESC,
        Enabled => ENABLED_DESC,
      }
    )
  }

  delegate!{
    to self.shape {
      fn itemname(&self) -> &str;
    }
  }

  #[throws(InternalError)]
  fn add_ui_operations(&self, upd: &mut Vec<UoDescription>,
                       gs: &GameState, gpc: &GPiece) {
    let state = self.state(gpc, &gs.occults)?;
    if state != Enabled {
      upd.push(UoDescription {
        kind: UoKind::Piece,
        def_key: 'A',
        opname: "activate".to_owned(),
        desc: Html::lit("Enable pickup deck"),
        wrc: WRC::Unpredictable,
      });
    }
    if state != Disabled {
      upd.push(UoDescription {
        kind: UoKind::Piece,
        def_key: 'S',
        opname: "deactivate".to_owned(),
        desc: Html::lit("Disable pickup deck"),
        wrc: WRC::Unpredictable,
      });
    }
  }

  #[throws(ApiPieceOpError)]
  fn ui_operation(&self, a: ApiPieceOpArgs<'_>,
                  opname: &str, wrc: WhatResponseToClientOp)
                  -> UpdateFromOpComplex {
    let ApiPieceOpArgs { gs,player,piece,ipieces,ioccults,to_permute,.. } = a;
    let gen = &mut gs.gen;
    let gplayers = &mut gs.players;
    let gpieces = &mut gs.pieces;

    let goccults = &mut gs.occults;
    let gpc = gpieces.byid_mut(piece)?;

    let gpl = gplayers.byid_mut(player)?;
    let nick = Html(htmlescape::encode_minimal(&gpl.nick));

    dbgc!("ui op k entry", &opname);
    
    let err_via_response =
      |ie:IE| ApiPieceOpError::ReportViaResponse(ie.into());

    let old_state = self.state(gpc, &goccults).map_err(err_via_response)?;
  
    let (new_state, did) = match opname {
      "activate" =>   (Enabled,  format!("enabled {}",  CORE_DESC)),
      "deactivate" => (Disabled, format!("disabled {}", CORE_DESC)),
      _ => throw!(OE::BadOperation),
    };
  
    let new_view = match new_state {
      Disabled => None,
      Enabled  => Some(OccKG::Scrambled),
    };
    let region_views = new_view.map(|new_view| {
      let region = self.shape.outline.region(gpc.pos)?;
      let views = UniformOccultationView(new_view).views()?;
      Ok::<_,IE>((region, views))
    })
      .transpose().map_err(err_via_response)?;

    let mut xupdates = vec![];

    if new_state == old_state {
        throw!(OE::PieceHeld);
    }      

    if old_state != Disabled {
      xupdates.extend(
        remove_occultation(&mut gen.unique_gen(),
                           gplayers, gpieces, goccults, ipieces, ioccults,
                           to_permute, piece)
          .map_err(|ie| ApiPieceOpError::ReportViaResponse(ie.into()))?
      );
    }

    if let Some((region, views)) = region_views {
      dbgc!("creating occ");
      xupdates.extend(
        create_occultation(&mut gen.unique_gen(), &mut gs.max_z,
                           gplayers, gpieces, goccults, ipieces, ioccults,
                           to_permute,
                           region, piece, views)?
      );
      dbgc!("creating occ done", &xupdates);
    }

    let log = vec![ LogEntry { html: Html(format!("{} {}", nick.0, did)) }];

    dbgc!("ui op k did main");
    
    (PieceUpdate {
      wrc, log,
      ops: PUOs::Simple(PUO::Modify(())),
    }, xupdates, None)
  }
}
