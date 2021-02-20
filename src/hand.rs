// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// This is otter::hidden::magic

use crate::prelude::*;
//use super::*;

#[derive(Debug,Clone,Serialize,Deserialize)]
struct MagicOwner {
  player: PlayerId,
  desc: Html,
  dasharray: Html,
}

#[derive(Debug,Serialize,Deserialize)]
struct Hand {
  shape: SimpleShape,
}

#[derive(Debug,Clone,Default,Serialize,Deserialize)]
struct HandState {
  owner: Option<MagicOwner>,
}

#[typetag::serde(name="Hand")]
impl PieceXData for HandState { }

impl Outline for Hand {
  delegate!{
    to self.shape {
      fn surround_path(&self, _pri: &PieceRenderInstructions)
                       -> Result<Html,IE>;
      fn thresh_dragraise(&self, _pri: &PieceRenderInstructions)
                          -> Result<Option<Coord>,IE>;
      fn bbox_approx(&self) -> [Pos;2];
    }
  }
}

#[typetag::serde]
impl PieceSpec for piece_specs::Hand {
  #[throws(SpecError)]
  fn load(&self, _: usize) -> Box<dyn Piece> {
    let (mut shape, common) = self.shape.load_raw()?;
    if common.itemname.is_some() {
      throw!(SpecError::ItemnameSpecifiedWhereForbidden);
    }
    if shape.nfaces() != 1 {
      throw!(SpecError::MultifacetedMagic);
    }
    shape.itemname = "magic-hand".to_string();
    Box::new(Hand {
      shape,
    }) as Box<dyn Piece>
  }
}

impl Hand {
  fn describe_html_inner(&self, xdata: Option<&HandState>) -> Html {
    if_chain! {
      if let Some(xdata) = xdata;
      if let Some(owner) = &xdata.owner;
      then { owner.desc.clone() }
      else { Html(format!("a hand repository")) }
    }
  }
}

#[typetag::serde]
impl Piece for Hand {
  fn nfaces(&self) -> RawFaceId { 1 }
  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, gpc: &PieceState,
               pri: &PieceRenderInstructions) {
    self.shape.svg_piece_raw(f, pri, &mut |f: &mut String| {
      if_chain!{
        if let Some(xdata) = gpc.xdata.get::<HandState>()?;
        if let Some(owned) = &xdata.owner;
        then { write!(f, r##" stroke-dasharray="{}" "##,
                      &owned.dasharray.0)?; }
      }
      Ok(())
    })?;
  }

  #[throws(IE)]
  fn describe_html(&self, _face: Option<FaceId>, gpc: &PieceState) -> Html {
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
                       gpc: &PieceState) {
    upd.push(if_chain! {
      if let Some(xdata) = gpc.xdata.get::<HandState>()?;
      if let Some(_owner) = &xdata.owner;
      then { UoDescription {
        kind: UoKind:: Piece,
        def_key: 'C',
        opname: "deactivate".to_owned(),
        desc: Html::lit("Deactivate hand"),
        wrc: WRC::Unpredictable,
      }}
      else { UoDescription {
        kind: UoKind:: Piece,
        def_key: 'C',
        opname: "claim".to_owned(),
        desc: Html::lit("Claim this as your hand"),
        wrc: WRC::Unpredictable,
      }}
    })
  }

  fn ui_operation(&self, gs: &mut GameState, player: PlayerId,
                  piece: PieceId, opname: &str, wrc: WhatResponseToClientOp)
                  -> PieceUpdateResult {
    let gplayers = &mut gs.players;
    let gpc = gs.pieces.byid_mut(piece)?;
    let xdata = gpc.xdata.get_mut::<HandState>()
      .map_err(|e| APOE::ReportViaResponse(e.into()))?;
    let old_desc = self.describe_html_inner(Some(xdata));

    let dasharray = player_dasharray(gplayers, player);
    let gpl = gplayers.byid_mut(player)?;
    let _occults = &mut gs.occults;

    let did;
    let new_owner;
    match (opname, xdata.owner.is_some()) {
      ("claim", false) => {
        let nick = Html(htmlescape::encode_minimal(&gpl.nick));
        let new_desc = Html(format!("{}'s hand", &nick.0));
        new_owner = Some(MagicOwner {
          player,
          dasharray,
          desc: new_desc,
        });
        // xxx recalculate occultations
        did = format!("claimed {}", &old_desc.0);
      }
      ("deactivate", true) => {
        new_owner = None;
        // xxx recalculate occultations
        did = format!("deactivated {}", &old_desc.0);
      }
      ("claim", true) |
      ("deactivate", false) => {
        throw!(OE::PieceHeld);
      }
      _ => {
        throw!(OE::BadOperation);
      }
    }

    let who_by = Html(htmlescape::encode_minimal(&gpl.nick));
    let log = vec![ LogEntry { html: Html(format!("{} {}", who_by.0, did)) }];

    xdata.owner = new_owner;

    Ok(PieceUpdate {
      wrc, log,
      ops: PUOs::Simple(PUO::Modify(())), // xxx
      // xxx want PUU::RecalculateOccultations
    })
  }
}
