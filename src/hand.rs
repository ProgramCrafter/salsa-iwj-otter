// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

pub const UNCLAIMED_HAND_DESC: &'static str = "a hand repository";

#[derive(Debug,Clone,Serialize,Deserialize)]
struct MagicOwner {
  player: PlayerId,
  desc: Html,
  dasharray: Html,
}

#[derive(Debug,Serialize,Deserialize)]
struct Hand {
  shape: GenericSimpleShape<(), RectShape>,
  label: Option<PieceLabelLoaded>,
  #[serde(default="Sort::backcompat_upgrade")] sort: Sort,
}

#[derive(Debug,Clone,Default,Serialize,Deserialize)]
struct HandState {
  owner: Option<MagicOwner>,
}

#[derive(Debug,Copy,Clone,Serialize,Deserialize)]
enum Sort {
  Hand,
  PlayerLabel,
}

type MkOccCA = fn(&OccDisplacement, &ZCoord) -> OccKA;

impl Sort {
  fn backcompat_upgrade() -> Self { Sort::Hand }
  fn itemname(self) -> &'static str { use Sort::*; match self {
    Hand => "magic-hand",
    PlayerLabel => "player-label",
  } }
  fn unclaimed_desc(self) -> HtmlLit { use Sort::*; Html::lit(match self {
    Hand => UNCLAIMED_HAND_DESC,
    PlayerLabel => "an unclaimed player label",
  }) }
  fn deact_desc(self) -> HtmlLit { use Sort::*; Html::lit(match self {
    Hand => "Deactivate hand",
    PlayerLabel => "Relinquish player label",
  }) }
  fn claim_desc(self) -> HtmlLit { use Sort::*; Html::lit(match self {
    Hand => "Claim this as your hand",
    PlayerLabel => "Claim player label",
  }) }
  fn owned_desc(self, nick: &HtmlStr) -> Html { use Sort::*; match self {
    Hand => hformat!("{}'s hand", nick),
    PlayerLabel => hformat!("{}'s player label", nick),
  } }
  fn views(self) -> Option<(MkOccCA, MkOccCA)> { use Sort::*; match self {
    Hand => Some((|_,_| OccKA::Visible,
                  |d,z| OccKA::Displaced((d.clone(), z.clone())))),
    PlayerLabel => None,
  } }
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
      fn bbox_approx(&self) -> Result<Rect, IE>;
    }
  }
}

impl piece_specs::OwnedCommon {
  #[throws(SpecError)]
  fn load(&self, sort: Sort) -> PieceSpecLoaded {
    let common = SimpleCommon {
      itemname: None,
      faces: index_vec![self.colour.clone()],
      edges: self.edge.iter().cloned().collect(),
      edge_width: self.edge_width,
    };
    let shape = match self.shape {
      Outline::RectShape(r) => r,
      _ => throw!(SpecError::UnsupportedShape),
    };
    let shape = GenericSimpleShape::new(
      (),
      shape,
      sort.itemname(),
      &common)?;
    let p = Box::new(Hand {
      shape, sort,
      label: self.label.load()?,
    }) as Box<dyn PieceTrait>;
    PieceSpecLoaded { p, occultable: None }
  }
}

#[typetag::serde]
impl PieceSpec for piece_specs::Hand {
  #[throws(SpecError)]
  fn load(&self, _: usize, _: &mut GPiece,
          _pcaliases: &PieceAliases, _ir: &InstanceRef)
          -> PieceSpecLoaded {
    self.c.load(Sort::Hand)?
  }
}

#[typetag::serde]
impl PieceSpec for piece_specs::PlayerLabel {
  #[throws(SpecError)]
  fn load(&self, _: usize, _: &mut GPiece,
          _pcaliases: &PieceAliases, _ir: &InstanceRef)
          -> PieceSpecLoaded {
    self.c.load(Sort::PlayerLabel)?
  }
}

impl Sort {
  fn describe_html_inner(self, xdata: Option<&HandState>) -> Html {
    if_chain! {
      if let Some(xdata) = xdata;
      if let Some(owner) = &xdata.owner;
      then { owner.desc.clone() }
      else { self.unclaimed_desc().into() }
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
    self.shape.svg_piece_raw(f, gpc.face, &mut |f: &mut Html| {
      if let Some(owned) = owned {
        hwrite!(f, r##" stroke-dasharray="{}" "##, &owned.dasharray)?;
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
                  &gpl.nick.to_html())?
      }
    }
  }

  #[throws(IE)]
  fn describe_html(&self, gpc: &GPiece, _goccults: &GameOccults) -> Html {
    let xdata = gpc.xdata.get()?;
    self.sort.describe_html_inner(xdata)
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
        desc: self.sort.deact_desc().into(),
        wrc: WRC::Unpredictable,
      }}
      else { UoDescription {
        kind: UoKind::Piece,
        def_key: 'C',
        opname: "claim".to_owned(),
        desc: self.sort.claim_desc().into(),
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
    let old_desc = self.sort.describe_html_inner(Some(xdata));
    let old_player = xdata.player();

    let dasharray = player_dasharray(gplayers, player);
    let gpl = gplayers.byid_mut(player)?;
    let nick = gpl.nick.to_html();

    dbgc!("ui op k entry", &opname, &xdata);

    let (new_owner, xupdates, did) =
      match (opname, xdata.owner.is_some())
    {
      ("claim", false) => {
        dbgc!("claiming");
        let new_desc = self.sort.owned_desc(&nick);
        let new_owner = Some(MagicOwner {
          player,
          dasharray,
          desc: new_desc,
        });
        let xupdates = match self.sort.views() {
          None => default(),
          Some((mk_owner, mk_defview)) => {
            let (region, views) = (||{
              dbgc!("claiming region");
              let rect   = self.shape.outline.rect  (gpc.pos)?;
              let region = self.shape.outline.region(gpc.pos)?;
              let displace = OccDisplacement::Rect { rect };
              let views = OwnerOccultationView {
                owner: player,
                owner_view: mk_owner(&displace, &gpc.zlevel.z),
                defview: mk_defview(&displace, &gpc.zlevel.z),
              }.views()?;
              dbgc!("claiming got region", &region, &views);
              Ok::<_,IE>((region, views))
            })().map_err(|ie| ApiPieceOpError::ReportViaResponse(ie.into()))?;
            
            // actually do things:
            dbgc!("creating occ");
            let xupdates =
              create_occultation(&mut gen.unique_gen(), &mut gs.max_z,
                                 gplayers, gpieces, goccults,
                                 ipieces, ioccults,
                                 to_recalculate,
                                 region, piece, views)?;
            xupdates
          }
        };
          
        dbgc!("creating occ done", &new_owner, &xupdates);
        (new_owner, xupdates, hformat!("claimed {}", &old_desc))
      }
      ("deactivate", true) => {
        let xupdates = match self.sort.views() {
          None => default(),
          Some(_) =>
            remove_occultation(&mut gen.unique_gen(),
                               gplayers, gpieces, goccults, ipieces, ioccults,
                               to_recalculate, piece)
            .map_err(|ie| ApiPieceOpError::ReportViaResponse(ie.into()))?,
        };
        (None, xupdates, hformat!("deactivated {}", &old_desc))
      }
      ("claim", true) |
      ("deactivate", false) => {
        throw!(OE::PieceHeld);
      }
      _ => {
        throw!(OE::BadOperation);
      }
    };

    let log = vec![ LogEntry { html: hformat!("{} {}", nick, did) }];

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
