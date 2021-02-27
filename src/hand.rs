// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// This is otter::hidden::magic

use crate::prelude::*;
//use super::*;

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
impl PieceXData for HandState { }

impl OutlineTrait for Hand {
  delegate!{
    to self.shape {
      fn outline_path(&self, _pri: &PieceRenderInstructions, scale: f64)
                       -> Result<Html,IE>;
      fn thresh_dragraise(&self, _pri: &PieceRenderInstructions)
                          -> Result<Option<Coord>,IE>;
      fn bbox_approx(&self) -> Result<[Pos;2], IE>;
    }
  }
}

#[typetag::serde]
impl PieceSpec for piece_specs::Hand {
  #[throws(SpecError)]
  fn load(&self, _: usize) -> Box<dyn PieceTrait> {
    let common = SimpleCommon {
      itemname: None,
      faces: index_vec![ColourSpec(self.colour.clone())],
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
    Box::new(Hand {
      shape,
    }) as Box<dyn PieceTrait>
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
  fn describe_html(&self, _face: Option<FaceId>, gpc: &GPiece) -> Html {
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
                       gpc: &GPiece) {
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
    let ApiPieceOpArgs { gs,player,piece,ipieces,.. } = a;
    let gplayers = &mut gs.players;
    let gpieces = &mut gs.pieces;

    let goccults = &mut gs.occults;
    let gpc = gpieces.byid_mut(piece)?;
    let xdata = gpc.xdata.get_mut::<HandState>()
      .map_err(|e| APOE::ReportViaResponse(e.into()))?;
    let old_desc = self.describe_html_inner(Some(xdata));
    let old_player = xdata.player();

    let dasharray = player_dasharray(gplayers, player);
    let gpl = gplayers.byid_mut(player)?;
    let nick = Html(htmlescape::encode_minimal(&gpl.nick));

    let (new_owner, xupdates, did) =
      match (opname, xdata.owner.is_some())
    {
      ("claim", false) => {
        let new_desc = Html(format!("{}'s hand", &nick.0));
        let new_owner = Some(MagicOwner {
          player,
          dasharray,
          desc: new_desc,
        });
        let pos = gpc.pos;
        let (region, views) = (||{
          let region = AreaC(
            [-1,1].iter().map(|&signum| Ok::<_,IE>({
              let xy = self.shape.outline.xy.try_map(
                |c| c.floor().to_i32().ok_or(CoordinateOverflow)
              )?;
              (pos + (xy * signum)?)?
            }))
              .collect::<Result<ArrayVec<_>,_>>()?
              .into_inner().unwrap()
          );
          let views = OwnerOccultationView {
            owner: player,
            owner_view: OccK::Visible,
            defview: OccK::Displaced { within: region },
          }.views()?;
          Ok::<_,IE>((region, views))
        })().map_err(|ie| ApiPieceOpError::ReportViaResponse(ie.into()))?;
        
        // actually do things:
        let xupdates =
          create_occultation(gplayers, gpieces, goccults, ipieces,
                             region, piece, views)?;
        // xxx recalculate occultations

        (new_owner, xupdates, format!("claimed {}", &old_desc.0))
      }
      ("deactivate", true) => {
        // xxx recalculate occultations
        (None, vec![], format!("deactivated {}", &old_desc.0))
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

    // We need to reaquire mut references because create_occultation etc.
    // need mut access to gpieces.
    let gpc = gpieces.byid_mut(piece).expect("piece disappeared");
    let xdata = gpc.xdata.get_mut::<HandState>().expect("xdata disappeared!");
    assert_eq!(xdata.player(), old_player);

    xdata.owner = new_owner;

    (PieceUpdate {
      wrc, log,
      ops: PUOs::Simple(PUO::Modify(())), // xxx
      // xxx want PUU::RecalculateOccultations
    }, xupdates)
  }
}
