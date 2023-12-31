// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;
use crate::*; // to get ambassador_impls, macro resolution trouble

pub const UNCLAIMED_HAND_DESC: &str = "a hand repository";

#[derive(Debug,Clone,Serialize,Deserialize)]
struct MagicOwner {
  player: PlayerId,
  desc: Html,
  dasharray: Html,
}

#[derive(Debug,Serialize,Deserialize)]
struct Hand {
  shape: GenericSimpleShape<(), RectOutline>,
  label: Option<PieceLabelLoaded>,
  #[serde(default="Behaviour::backcompat_upgrade")]
  #[serde(alias="sort")]
  behaviour: Behaviour,
}

#[derive(Debug,Clone,Default,Serialize,Deserialize)]
struct HandState {
  owner: Option<MagicOwner>,
}

#[derive(Debug,Copy,Clone,Serialize,Deserialize)]
enum Behaviour {
  Hand,
  PlayerLabel,
}

type MkOccCA = fn(&OccDisplacement, &ZCoord) -> OccKA;

impl Behaviour {
  fn backcompat_upgrade() -> Self { Behaviour::Hand }
  fn itemname(self) -> &'static str { use Behaviour::*; match self {
    Hand => "magic-hand",
    PlayerLabel => "player-label",
  } }
  fn unclaimed_desc(self) -> HtmlLit { use Behaviour::*; Html::lit(match self {
    Hand => UNCLAIMED_HAND_DESC,
    PlayerLabel => "an unclaimed player label",
  }) }
  fn deact_desc(self) -> HtmlLit { use Behaviour::*; Html::lit(match self {
    Hand => "Deactivate hand",
    PlayerLabel => "Relinquish player label",
  }) }
  fn claim_desc(self) -> HtmlLit { use Behaviour::*; Html::lit(match self {
    Hand => "Claim this as your hand",
    PlayerLabel => "Claim player label",
  }) }
  fn owned_desc(self, nick: &HtmlStr) -> Html { use Behaviour::*; match self {
    Hand => hformat!("{}'s hand", nick),
    PlayerLabel => hformat!("{}'s player label", nick),
  } }
  fn views(self) -> Option<(MkOccCA, MkOccCA)> { use Behaviour::*; match self {
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

impl_via_ambassador!{
  #[dyn_upcast]
  impl OutlineTrait for Hand { shape }
}

impl piece_specs::OwnedCommon {
  #[throws(SpecError)]
  fn load(&self, behaviour: Behaviour) -> SpecLoaded {
    let common = SimpleCommon {
      itemname: None,
      faces: index_vec![self.colour.clone()],
      edges: self.edge.iter().cloned().collect(),
      edge_width: self.edge_width,
    };
    let shape = match self.shape {
      Outline::RectOutline(r) => r,
      _ => throw!(SpecError::UnsupportedShape),
    };
    let shape = GenericSimpleShape::new(
      (),
      shape,
      behaviour.itemname(),
      &common)?;
    let p = Box::new(Hand {
      shape, behaviour,
      label: self.label.load()?,
    }) as Box<dyn PieceTrait>;
    SpecLoaded {
      p,
      occultable: None,
      special: default(),
    }
  }
}

#[typetag::serde]
impl PieceSpec for piece_specs::Hand {
  #[throws(SpecError)]
  fn load(&self, PLA { gpc,..}: PLA) -> SpecLoaded {
    gpc.moveable = PieceMoveable::IfWresting;
    gpc.rotateable = false;
    self.c.load(Behaviour::Hand)?
  }
}

#[typetag::serde]
impl PieceSpec for piece_specs::PlayerLabel {
  #[throws(SpecError)]
  fn load(&self, PLA { .. }: PLA) -> SpecLoaded {
    self.c.load(Behaviour::PlayerLabel)?
  }
}

impl Behaviour {
  fn describe_html_inner(self, xdata: Option<&HandState>) -> Html {
    if_chain! {
      if let Some(xdata) = xdata;
      if let Some(owner) = &xdata.owner;
      then { owner.desc.clone() }
      else { self.unclaimed_desc().into() }
    }
  }
}

#[dyn_upcast]
impl PieceBaseTrait for Hand {
  fn nfaces(&self) -> RawFaceId { 1 }

  delegate!{
    to self.shape {
      fn itemname(&self) -> &str;
    }
  }
}

#[typetag::serde]
impl PieceTrait for Hand {
  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, gpc: &GPiece,
               gs: &GameState, _vpid: VisiblePieceId) {
    let owned = if_chain!{
      if let Some(xdata) = gpc.xdata.get::<HandState>()?;
      if let count = gpc.occult.active_total_ppieces(&gs.occults)?;
      if let Some(owned) = &xdata.owner;
      then { Some((owned, count)) }
      else { None }
    };
    self.shape.svg_piece_raw(f, gpc.face, &mut |f: &mut Html| {
      if let Some((owned,_)) = owned {
        hwrite!(f, r##" stroke-dasharray="{}" "##, &owned.dasharray)?;
      }
      Ok(())
    })?;
    if_chain! {
      if let Some((owned, count)) = owned;
      if let Some(gpl) = gs.players.get(owned.player);
      if let Some(label) = &self.label;
      then {
        label.svg(f,
                  &self.shape.outline,
                  self.shape.edges.get(0),
                  &if let Some(count) = count {
                    hformat!(r#"<tspan {}>{}</tspan> {}"#,
                             monospace_font(5),
                             count,
                             &gpl.nick)
                  } else {
                    hformat!("{}",
                             &gpl.nick)
                  })?
      }
    }
  }

  #[throws(IE)]
  fn describe_html(&self, gpc: &GPiece, _goccults: &GOccults) -> Html {
    let xdata = gpc.xdata.get()?;
    self.behaviour.describe_html_inner(xdata)
  }

  #[throws(InternalError)]
  fn add_ui_operations(&self, _: ShowUnocculted, upd: &mut Vec<UoDescription>,
                       _gs: &GameState, gpc: &GPiece) {
    upd.push(if_chain! {
      if let Some(xdata) = gpc.xdata.get::<HandState>()?;
      if let Some(_owner) = &xdata.owner;
      then { UoDescription {
        kind: UoKind::Piece,
        def_key: 'C',
        opname: "deactivate".to_owned(),
        desc: self.behaviour.deact_desc().into(),
        wrc: WRC::Unpredictable,
      }}
      else { UoDescription {
        kind: UoKind::Piece,
        def_key: 'C',
        opname: "claim".to_owned(),
        desc: self.behaviour.claim_desc().into(),
        wrc: WRC::Unpredictable,
      }}
    });

    organise::add_ui_operations(upd, &self.shape.outline.rect(gpc.pos)?)?;
  }

  #[throws(ApiPieceOpError)]
  fn ui_operation(&self, vis: ShowUnocculted, mut a: ApiPieceOpArgs<'_>,
                  opname: &str, wrc: WhatResponseToClientOp)
                  -> OpOutcomeThunk {
    if let Some(r) = {
      let gpc = a.gs.pieces.byid_mut(a.piece)?;
      let rot_checked = gpc.occulter_check_unrotated(vis)?;
      let rect = self.shape.outline.rect(gpc.pos)
        .map_err(|CoordinateOverflow|
                 internal_error_bydebug(&(&gpc.pos, &self.shape)))?;
      organise::ui_operation(&mut a, rot_checked, opname, wrc, &rect)?                             
    } {
      return r.into();
    }

    let ApiPieceOpArgs { gs,player,piece,ipieces,ioccults,to_recalculate,.. } = a;
    let gen = &mut gs.gen;
    let gplayers = &mut gs.players;
    let gpieces = &mut gs.pieces;

    let goccults = &mut gs.occults;
    let gpc = gpieces.byid_mut(piece)?;
    let rot_checked = gpc.occulter_check_unrotated(vis);
    let xdata = gpc.xdata.get_mut::<HandState,_>(default)?;
    let old_desc = self.behaviour.describe_html_inner(Some(xdata));
    let old_player = xdata.player();

    let dasharray = player_dasharray(gplayers, player);
    let gpl = gplayers.byid_mut(player)?;
    let nick = gpl.nick.to_html();

    trace_dbg!("ui op k entry", &opname, &xdata);

    let puos = PUOs_Simple_Modify;

    let (new_owner, xupdates, did) =
      match (opname, xdata.owner.is_some())
    {
      ("claim", false) => {
        trace_dbg!("claiming");
        let new_desc = self.behaviour.owned_desc(&nick);
        let new_owner = Some(MagicOwner {
          player,
          dasharray,
          desc: new_desc,
        });
        let xupdates = match self.behaviour.views() {
          None => default(),
          Some((mk_owner, mk_defview)) => {
            let rot_checked = rot_checked?;
            let (region, views) = (||{
              trace_dbg!("claiming region");
              let rect   = self.shape.outline.rect  (gpc.pos)?;
              let region = self.shape.outline.region(gpc.pos)?;
              let displace = OccDisplacement::Rect { rect };
              let views = OwnerOccultationView {
                owner: player,
                owner_view: mk_owner(&displace, &gpc.zlevel.z),
                defview: mk_defview(&displace, &gpc.zlevel.z),
              }.views()?;
              trace_dbg!("claiming got region", &region, &views);
              Ok::<_,IE>((region, views))
            })()?;
            
            // actually do things:
            trace_dbg!("creating occ");
            let xupdates =
              create_occultation(&mut gen.unique_gen(), &mut gs.max_z,
                                 gplayers, gpieces, goccults,
                                 ipieces, ioccults,
                                 to_recalculate, rot_checked,
                                 region, piece, views, &puos)?;
            xupdates
          }
        };
          
        trace_dbg!("creating occ done", &new_owner, &xupdates);
        (new_owner, xupdates, hformat!("claimed {}", &old_desc))
      }
      ("deactivate", true) => {
        let xupdates = match self.behaviour.views() {
          None => default(),
          Some(_) =>
            remove_occultation(&mut gen.unique_gen(),
                               gplayers, gpieces, goccults, ipieces, ioccults,
                               to_recalculate, piece)?,
        };
        (None, xupdates, hformat!("deactivated {}", &old_desc))
      }
      ("claim", true) |
      ("deactivate", false) => {
        throw!(Ia::BadUiOperation);
      }
      _ => {
        throw!(Ia::BadUiOperation);
      }
    };

    let log = vec![ LogEntry { html: hformat!("{} {}", nick, did) }];

    trace_dbg!("ui op k did main");
    
    // We need to reaquire mut references because create_occultation etc.
    // need mut access to gpieces.
    let gpc = gpieces.byid_mut(piece).expect("piece disappeared");
    let xdata = gpc.xdata.get_mut::<HandState,_>(default)
      .expect("xdata disappeared!");
    assert_eq!(xdata.player(), old_player);

    trace_dbg!("thinging done", &xdata, &new_owner);
    xdata.owner = new_owner;

    (PieceUpdate {
      wrc, log,
      ops: puos.into(),
    }, xupdates.into_unprepared(None))
      .into()
  }

  fn occultation_notify_hook(&self, piece: PieceId) -> UnpreparedUpdates {
    occultation_notify_update_image(piece)
  }
}
