// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

//! Currency
//!
//! A "Currency" piece
//!  - has an image, which is another piece which it displays
//!  - has special counting behaviour on drag and drop
//!  - represents a *quanity*

// Future plans
//  - occultable, to hide the quantity
//  - can have a back face which is less manipulable (if image has 2 faces)

use crate::prelude::*;

const QTY_FONT_SIZE: f64 = 6.;

type Qty = MultigrabQty;

#[derive(Debug,Serialize,Deserialize)]
pub struct Spec {
  image: Box<dyn PieceSpec>,
  qty: Qty,
  currency: String,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct Banknote {
  itemname: String,
  image: Arc<dyn InertPieceTrait>,
  currency: String,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct Value {
  qty: Qty,
}

#[typetag::serde(name="Currency")]
impl PieceXData for Value { fn dummy() -> Self { Value { qty: 0 } } }

#[typetag::serde(name="Currency")]
impl PieceSpec for Spec {
  #[throws(SpecError)]
  fn load(&self, PLA { gpc,ig,depth,.. }: PLA) -> SpecLoaded {
    gpc.rotateable = false;

    let Spec { ref image, ref currency, qty } = *self;

    let SpecLoadedInert { p: image, occultable:_ } =
      image.load_inert(ig, depth)?;

    let itemname = format!("currency-{}", image.itemname());

    if image.nfaces() != 1 {
      throw!(SpecError::WrongNumberOfFaces {
        got: image.nfaces(), got_why: "image".into(),
        exp: 1,              exp_why: "needed".into(),
      });
    }

    let _value: &mut Value = gpc.xdata_mut(|| Value { qty })?;
        
    let bnote = Banknote {
      image: image.into(),
      currency: currency.clone(),
      itemname,
    };

    gpc.fastsplit = FastSplitId::new_placeholder();

    let special = PieceSpecialProperties {
      multigrab: true,
      ..default()
    };
    SpecLoaded { p: Box::new(bnote) as _, occultable: None, special }
  }
}

#[dyn_upcast]
impl OutlineTrait for Banknote {
  delegate!{
    to self.image {
      fn outline_path(&self, scale: f64) -> Result<Html, IE>;
      fn thresh_dragraise(&self) -> Result<Option<Coord>, IE>;
      fn bbox_approx(&self) -> Result<Rect, IE>;
    }
  }
}

#[dyn_upcast]
impl PieceBaseTrait for Banknote {
  fn nfaces(&self) -> RawFaceId { self.image.nfaces() }
  fn itemname(&self) -> &str { &self.itemname }
}

#[typetag::serde(name="Currency")]
impl PieceTrait for Banknote {
  #[throws(IE)]
  fn describe_html(&self, gpc: &GPiece, _: &GameOccults) -> Html {
    let value: &Value = gpc.xdata.get_exp()?;
    hformat!("{}, {}{}",
             self.image.describe_html(gpc.face)?,
             value.qty, &self.currency)
  }

  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, gpc: &GPiece, _gs: &GameState,
               vpid: VisiblePieceId) {
    self.image.svg(f, vpid, gpc.face, &gpc.xdata)?;
    
    let value: &Value = gpc.xdata.get_exp()?;
    let label_font_size = QTY_FONT_SIZE;
    let label_y_adj = label_font_size * SVG_FONT_Y_ADJUST_OF_FONT_SIZE;

    hwrite!(f,
            r##"<{} text-align="center" text-anchor="middle" x="0" y="{}" font-size="{}">{}{}</text>"##,
            HTML_TEXT_LABEL_ELEM_START,
            label_y_adj, label_font_size,
            value.qty, &self.currency)?;
  }

  #[throws(ApiPieceOpError)]
  fn op_multigrab(&self, _: ApiPieceOpArgs, show: ShowUnocculted,
                  take: MultigrabQty, new_z: ShouldSetZLevel) -> OpOutcomeThunk {
    let currency = self.currency.clone();
    OpOutcomeThunk::Reborrow(Box::new(
      move |ig: &mut InstanceGuard, player: PlayerId, tpiece: PieceId|
  {
    ig.fastsplit_split(player, tpiece, show, new_z,
      move |_: &IOccults, _: &GameOccults, gpl: &GPlayer,
            tgpc: &mut GPiece, _tipc: &IPiece, tipc_p: &dyn PieceTrait,
            ngpc: &mut GPiece|
  {
    let self_: &Banknote = tipc_p.downcast_piece()?;

    let tgpc_value: &mut Value = tgpc.xdata.get_mut_exp()?;
    let remaining = tgpc_value.qty.checked_sub(take)
      .ok_or(Ia::CurrencyShortfall)?;

    tgpc_value.qty = take;
    ngpc.xdata_init(Value { qty: remaining })?;

    tgpc.held = Some(player);
    ngpc.held = None;
    
    tgpc.pinned = false;

    let logents = vec![ LogEntry { html: hformat!(
      "{} took {} {}{}, leaving {}{}",
      gpl.nick.to_html(), self_.image.describe_html(tgpc.face)?,
      take, &currency,
      remaining, &currency,
    )}];

    let update = PieceUpdateOp::ModifyQuiet(());

    Ok((
      (WhatResponseToClientOp::UpdateSvg,
       update,
       logents).into(),
      default()
    ))
  })}))}
}
