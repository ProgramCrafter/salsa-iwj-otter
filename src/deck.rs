// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

pub const CORE_DESC    : HtmlLit = Html::lit("a play/pickup deck");
pub const DISABLED_DESC: HtmlLit = Html::lit("a play/pickup deck (disabled)");
pub const COUNTING_DESC: HtmlLit = Html::lit("a play pile (counting)");
pub const ENABLED_DESC : HtmlLit = Html::lit("a pickup deck (enabled)");

#[derive(Debug,Serialize,Deserialize)]
struct Deck {
  shape: GenericSimpleShape<(), RectShape>,
  label: Option<PieceLabelLoaded>,
}

#[derive(Debug,Clone,Copy,Ord,PartialOrd,Eq,PartialEq)]
enum State {
  Disabled,
  Counting,
  Enabled,
}
use State::*;

#[dyn_upcast]
impl OutlineTrait for Deck {
  delegate!{
    to self.shape {
      fn outline_path(&self, scale: f64) -> Result<Html,IE>;
      fn thresh_dragraise(&self) -> Result<Option<Coord>,IE>;
      fn bbox_approx(&self) -> Result<Rect, IE>;
    }
  }
}

#[typetag::serde(name="PickupDeck")]
impl PieceSpec for piece_specs::Deck {
  #[throws(SpecError)]
  fn load(&self, _: usize, _: &mut GPiece,
          _pcaliases: &PieceAliases, _ir: &InstanceRef)
          -> PieceSpecLoaded {
    let common = SimpleCommon {
      itemname: None,
      faces: self.faces.clone(),
      edges: self.edges.clone(),
      edge_width: self.edge_width,
    };
    let shape = match self.shape {
      Outline::RectShape(r) => r,
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
      label: self.label.load()?,
    }) as Box<dyn PieceTrait>;
    PieceSpecLoaded { p, occultable: None }
  }
}

impl Deck {
  #[throws(IE)]
  fn state(&self, gpc: &GPiece, goccults: &GameOccults) -> State {
    match gpc.occult.active_views(&goccults)? {
      None                                                       => Disabled,
      Some(OccultationViews { defview: OccK::Visible,..       }) => Counting,
      Some(OccultationViews { defview: OccK::Scrambled /*old*/,.. }) |
      Some(OccultationViews { defview: OccK::Displaced(..),.. }) => Enabled, 
      x => throw!(internal_error_bydebug(&x)),
    }
  }

  #[throws(IE)]
  fn current_face(&self, gpc: &GPiece, goccults: &GameOccults) -> FaceId {
    RawFaceId::from(match self.state(gpc, goccults)? {
      Disabled | Counting => 0,
      Enabled             => 1,
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
                  &count.to_html())?;
      }
    }
  }

  #[throws(IE)]
  fn describe_html(&self, gpc: &GPiece, goccults: &GameOccults) -> Html {
    match self.state(gpc, goccults)? {
      Disabled => DISABLED_DESC,
      Counting => ENABLED_DESC,
      Enabled => ENABLED_DESC,
    }.into()
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
        desc: Html::lit("Enable pickup deck").into(),
        wrc: WRC::Unpredictable,
      });
    }
    if state != Counting {
      upd.push(UoDescription {
        kind: UoKind::Piece,
        def_key: 'C',
        opname: "counting".to_owned(),
        desc: Html::lit("Make into counting play pile").into(),
        wrc: WRC::Unpredictable,
      });
    }
    if state != Disabled {
      upd.push(UoDescription {
        kind: UoKind::Piece,
        def_key: 'S',
        opname: "deactivate".to_owned(),
        desc: Html::lit("Disable pickup deck").into(),
        wrc: WRC::Unpredictable,
      });
    }
  }

  #[throws(ApiPieceOpError)]
  fn ui_operation(&self, a: ApiPieceOpArgs<'_>,
                  opname: &str, wrc: WhatResponseToClientOp)
                  -> UpdateFromOpComplex {
    let ApiPieceOpArgs { gs,player,piece,ipieces,ioccults,to_recalculate,.. } = a;
    let gen = &mut gs.gen;
    let gplayers = &mut gs.players;
    let gpieces = &mut gs.pieces;

    let goccults = &mut gs.occults;
    let gpc = gpieces.byid_mut(piece)?;

    let gpl = gplayers.byid_mut(player)?;
    let nick = gpl.nick.to_html();

    dbgc!("ui op k entry", &opname);
    
    let old_state = self.state(gpc, &goccults)?;
  
    let (new_state, did) = match opname {
      "activate"   => (Enabled,  hformat!("enabled {}",         CORE_DESC)),
      "counting"   => (Counting, hformat!("set {} to counting", CORE_DESC)),
      "deactivate" => (Disabled, hformat!("disabled {}",        CORE_DESC)),
      _ => throw!(OE::BadOperation),
    };
  
    let new_view = match new_state {
      Disabled => None,
      Counting => Some(OccKG::Visible),
      Enabled  => {
        let displace = OccDisplacement::Stack { pos: gpc.pos };
        let displace = (displace, gpc.zlevel.z.clone());
                  Some(OccKG::Displaced(displace))
      },
    };
    let region_views = new_view.map(|new_view| {
      let region = self.shape.outline.region(gpc.pos)?;
      let views = UniformOccultationView(new_view).views()?;
      Ok::<_,IE>((region, views))
    })
      .transpose()?;

    let mut xupdates = vec![];

    if new_state == old_state {
        throw!(OE::PieceHeld);
    }      

    if old_state != Disabled {
      xupdates.extend(
        remove_occultation(&mut gen.unique_gen(),
                           gplayers, gpieces, goccults, ipieces, ioccults,
                           to_recalculate, piece)?
      );
    }

    if let Some((region, views)) = region_views {
      dbgc!("creating occ");
      xupdates.extend(
        create_occultation(&mut gen.unique_gen(), &mut gs.max_z,
                           gplayers, gpieces, goccults, ipieces, ioccults,
                           to_recalculate,
                           region, piece, views)?
      );
      dbgc!("creating occ done", &xupdates);
    }

    let log = vec![ LogEntry { html: hformat!("{} {}", nick, did) }];

    dbgc!("ui op k did main");
    
    (PieceUpdate {
      wrc, log,
      ops: PUOs::Simple(PUO::Modify(())),
    },
     xupdates.into_unprepared_nc())
  }

  fn occultation_notify_hook(&self, piece: PieceId) -> UnpreparedUpdates {
    Some(Box::new(
      move |updates: &mut PrepareUpdatesBuffer| {
        updates.piece_update_image(piece, &None)
          .unwrap_or_else(|e| error!("unable to send update! {:?}", e))
      }
    ))
  }
}
