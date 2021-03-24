// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;
use piece_specs::PieceLabel;

pub const UNCLAIMED_DESC: &str = "a hand repository";

#[derive(Debug,Clone,Serialize,Deserialize)]
struct MagicOwner {
  player: PlayerId,
  desc: Html,
  dasharray: Html,
}

#[derive(Debug,Serialize,Deserialize)]
struct Hand {
  shape: GenericSimpleShape<(), shapelib::Rectangle>,
  label: Option<PieceLabel>,
}

#[derive(Debug,Clone,Default,Serialize,Deserialize)]
struct HandState {
  owner: Option<MagicOwner>,
}

impl HandState {
  fn player(&self) -> Option<PlayerId> {
    self.owner.as_ref().map(|o| o.player)
  }
}

#[typetag::serde(name="Hand")]
impl PieceXData for HandState {
  fn dummy() -> Self { default() }
}

#[dyn_upcast]
impl OutlineTrait for Hand {
  delegate!{
    to self.shape {
      fn outline_path(&self, scale: f64) -> Result<Html,IE>;
      fn thresh_dragraise(&self) -> Result<Option<Coord>,IE>;
      fn bbox_approx(&self) -> Result<[Pos;2], IE>;
    }
  }
}

#[typetag::serde]
impl PieceSpec for piece_specs::Hand {
  #[throws(SpecError)]
  fn load(&self, _: usize, _: &mut GPiece, _ir: &InstanceRef)
          -> PieceSpecLoaded {
    let common = SimpleCommon {
      itemname: None,
      faces: index_vec![self.colour.clone()],
      edges: self.edge.iter().cloned().collect(),
      edge_width: self.edge_width,
    };
    let shape = match self.shape {
      Outline::Rectangle(r) => r,
      _ => throw!(SpecError::UnsupportedShape),
    };
    let shape = GenericSimpleShape::new(
      (),
      shape,
      "magic-hand",
      &common)?;
    let p = Box::new(Hand {
      shape,
      label: self.label.clone(),
    }) as Box<dyn PieceTrait>;
    PieceSpecLoaded { p, occultable: None }
  }
}

impl Hand {
  fn describe_html_inner(&self, xdata: Option<&HandState>) -> Html {
    if_chain! {
      if let Some(xdata) = xdata;
      if let Some(owner) = &xdata.owner;
      then { owner.desc.clone() }
      else { Html(UNCLAIMED_DESC.into()) }
    }
  }
}

#[typetag::serde]
impl PieceTrait for Hand {
  fn nfaces(&self) -> RawFaceId { 1 }
  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, gpc: &GPiece,
               gs: &GameState, _vpid: VisiblePieceId) {
    let owned = if_chain!{
      if let Some(xdata) = gpc.xdata.get::<HandState>()?;
      if let Some(owned) = &xdata.owner;
      then { Some(owned) }
      else { None }
    };
    self.shape.svg_piece_raw(f, gpc.face, &mut |f: &mut String| {
      if let Some(owned) = owned {
        write!(f, r##" stroke-dasharray="{}" "##, &owned.dasharray.0)?;
      }
      Ok(())
    })?;
    if_chain! {
      if let Some(owned) = owned;
      if let Some(gpl) = gs.players.get(owned.player);
      if let Some(label) = &self.label;
      then {
        label.svg(f,
                  &self.shape.outline,
                  self.shape.edges.get(0), 
                  &Html(htmlescape::encode_minimal(&gpl.nick)))?;
      }
    }
  }

  #[throws(IE)]
  fn describe_html(&self, gpc: &GPiece, _goccults: &GameOccults) -> Html {
    let xdata = gpc.xdata.get()?;
    self.describe_html_inner(xdata)
  }

  delegate!{
    to self.shape {
      fn itemname(&self) -> &str;
    }
  }

  #[throws(InternalError)]
  fn add_ui_operations(&self, upd: &mut Vec<UoDescription>,
                       _gs: &GameState, gpc: &GPiece) {
    upd.push(if_chain! {
      if let Some(xdata) = gpc.xdata.get::<HandState>()?;
      if let Some(_owner) = &xdata.owner;
      then { UoDescription {
        kind: UoKind::Piece,
        def_key: 'C',
        opname: "deactivate".to_owned(),
        desc: Html::lit("Deactivate hand"),
        wrc: WRC::Unpredictable,
      }}
      else { UoDescription {
        kind: UoKind::Piece,
        def_key: 'C',
        opname: "claim".to_owned(),
        desc: Html::lit("Claim this as your hand"),
        wrc: WRC::Unpredictable,
      }}
    })
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
    let xdata = gpc.xdata.get_mut::<HandState,_>(default)
      .map_err(|e| APOE::ReportViaResponse(e.into()))?;
    let old_desc = self.describe_html_inner(Some(xdata));
    let old_player = xdata.player();

    let dasharray = player_dasharray(gplayers, player);
    let gpl = gplayers.byid_mut(player)?;
    let nick = Html(htmlescape::encode_minimal(&gpl.nick));

    dbgc!("ui op k entry", &opname, &xdata);

    let (new_owner, xupdates, did) =
      match (opname, xdata.owner.is_some())
    {
      ("claim", false) => {
        dbgc!("claiming");
        let new_desc = Html(format!("{}'s hand", &nick.0));
        let new_owner = Some(MagicOwner {
          player,
          dasharray,
          desc: new_desc,
        });
        let (region, views) = (||{
          dbgc!("claiming region");
          let region = self.shape.outline.region(gpc.pos)?;
          let displace = OccDisplacement::Rect { area: region };
          let views = OwnerOccultationView {
            owner: player,
            owner_view: OccK::Visible,
            defview: OccK::Displaced((displace, gpc.zlevel.z.clone())),
          }.views()?;
          dbgc!("claiming got region", &region, &views);
          Ok::<_,IE>((region, views))
        })().map_err(|ie| ApiPieceOpError::ReportViaResponse(ie.into()))?;
        
        // actually do things:
        dbgc!("creating occ");
        let xupdates =
          create_occultation(&mut gen.unique_gen(), &mut gs.max_z,
                             gplayers, gpieces, goccults, ipieces, ioccults,
                             to_recalculate,
                             region, piece, views)?;

        dbgc!("creating occ done", &new_owner, &xupdates);
        (new_owner, xupdates, format!("claimed {}", &old_desc.0))
      }
      ("deactivate", true) => {
        let xupdates =
          remove_occultation(&mut gen.unique_gen(),
                             gplayers, gpieces, goccults, ipieces, ioccults,
                             to_recalculate, piece)
          .map_err(|ie| ApiPieceOpError::ReportViaResponse(ie.into()))?;

        (None, xupdates, format!("deactivated {}", &old_desc.0))
      }
      ("claim", true) |
      ("deactivate", false) => {
        throw!(OE::PieceHeld);
      }
      _ => {
        throw!(OE::BadOperation);
      }
    };

    let log = vec![ LogEntry { html: Html(format!("{} {}", nick.0, did)) }];

    dbgc!("ui op k did main");
    
    // We need to reaquire mut references because create_occultation etc.
    // need mut access to gpieces.
    let gpc = gpieces.byid_mut(piece).expect("piece disappeared");
    let xdata = gpc.xdata.get_mut::<HandState,_>(default)
      .expect("xdata disappeared!");
    assert_eq!(xdata.player(), old_player);

    dbgc!("thinging done", &xdata, &new_owner);
    xdata.owner = new_owner;

    (PieceUpdate {
      wrc, log,
      ops: PUOs::Simple(PUO::Modify(())),
    }, xupdates, None)
  }
}
