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
use crate::*; // to get ambassador_impls, macro resolution trouble

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

impl_via_ambassador!{
  #[dyn_upcast]
  impl OutlineTrait for Banknote { image }
}

#[dyn_upcast]
impl PieceBaseTrait for Banknote {
  fn nfaces(&self) -> RawFaceId { self.image.nfaces() }
  fn itemname(&self) -> &str { &self.itemname }
}

#[typetag::serde(name="Currency")]
impl PieceTrait for Banknote {
  #[throws(IE)]
  fn describe_html(&self, gpc: &GPiece, _: &GOccults) -> Html {
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
      move |ig: &mut InstanceGuard, (player, tpiece)|
  {
    ig.fastsplit_split(player, tpiece, show, new_z,
      move |_: &IOccults, _: &GOccults, gpl: &GPlayer,
            tgpc: &mut GPiece, tipc: &IPiece,
            ngpc: &mut GPiece|
  {
    let self_: &Banknote = tipc.p.show(show).downcast_piece_fastsplit()?;

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

  #[throws(IE)]
  fn held_change_hook(&self,
                      _ig: &InstanceRef,
                      gplayers: &GPlayers,
                      ipieces: &IPieces,
                      goccults: &GOccults,
                      gpieces: &mut GPieces,
                      tpiece: PieceId,
                      was_held: Option<PlayerId>)
                      -> OpHookThunk {
    let missing_e = || internal_error_bydebug(&(was_held, tpiece));

    let tself = self;
    let tgpc = gpieces.get(tpiece).ok_or_else(missing_e)?;
    let tipc = ipieces.get(tpiece).ok_or_else(missing_e)?;

    if_let!{ Some(player) = was_held; else return Ok(default()) }
    if tgpc.held.is_some() { /*wat*/ return default(); }
    let gpl = gplayers.get(player);

    // Occultation is not yet supported here.  When implementing
    // occultation, delete this and fix all the things.
    let show = ShowUnocculted::new_visible();

    let merge_with = gpieces.iter().filter_map(|(mpiece, mgpc)|{
      if mpiece == tpiece { throw!() }
      let mipc = ipieces.get(mpiece)?;

      // Our position is within its bbox
      if ! mipc.show(show).abs_bbox(mgpc).ok()?.contains(tgpc.pos) { throw!() }

      // It's a banknote
      let mself: &Banknote = mipc.p.show(show)
        .downcast_piece_fastsplit().ok()?;

      // Of our currency
      if mself.currency != tself.currency { throw!() }
      let currency = &mself.currency;

      if mgpc.occult.passive_occid().is_some() {
        // We don't do occultation yet.  But, anyway, we don't want to
        // deal with this since it might mean we're totally invisible
        // to our player!  When we do support this, call
        // Occultation::get_kind ?
        throw!();
      }

      // We are in the ellipse inscribed in its bbox
      let delta = (tgpc.pos - mgpc.pos).ok()?.promote();
      let bbox_sz = mipc.show(show).bbox_approx().ok()?;
      let dist2: f64 = (0..2).map(|i| {
        // The bbox may not be centred.  We imagine a quarter ellipse
        // inscribed in each corner, with the centre at the nominal position.
        let delta = delta.coords[i];
        let cnr = if delta < 0. { bbox_sz.tl() } else { bbox_sz.br() };
        let rel = delta / (cnr.coords[i] as f64);
        rel*rel
      }).sum();
      if ! (dist2 <= 1.) { throw!() }

      Some((mpiece,mgpc,currency))
    });

    if_let!{
      Some((mpiece,mgpc,currency)) =
        merge_with.at_most_one().ok().flatten();
      else return Ok(default());
    }

    let tqty = tgpc.xdata_exp::<Value>()?.qty;
    let mqty = mgpc.xdata_exp::<Value>()?.qty;
    if_let!{
      Some(new_qty) = mqty.checked_add(tqty);
      else return Ok(default()); // arithmetic overflow!
    }

    let logents = vec![ LogEntry { html: hformat!(
      "{} deposited {}, giving {}{}",
      match gpl {
        Some(gpl) => gpl.nick.to_html(),
        None => Html::lit("Departing player").into(),
      },
      tipc.p.show(show).describe_html(tgpc, goccults)?,
      new_qty, currency,
    )}];

  OpHookThunk::Reborrow(Box::new(move |igg: &mut InstanceGuard, (_player,)| {

    let (puo, uu_d) = igg.fastsplit_delete(show, tpiece, &logents)?;
    // commitment point
  Ok(((move ||{
    let ig = &mut **igg;

    let () = (||{
      // None of these situations ought to happen, really, but the
      // callback structure means it isn't 100% possible to rule them out.
      let mgpc = ig.gs.pieces.get_mut(mpiece).ok_or("tpiece vanished")?;
      let mvalue = mgpc.xdata_mut_exp::<Value>().map_err(|_|"xdata vanished")?;
      mvalue.qty = mvalue.qty.checked_add(tqty).ok_or("overflow")?;
      if mvalue.qty != new_qty { throw!("modified value") }
      Ok::<_,&'static str>(())
    })().unwrap_or_else(|m|{
      warn!("during dorp-and-merge of currency {tpiece:?} into {mpiece:?}: {m}");
    });

    vec![Box::new(move |prepub: &mut PrepareUpdatesBuffer| {
      prepub.piece_update_image(mpiece, &None).unwrap_or_else(
        |e| error!("currency image update failed: {} {:?}", &e, &e));
      prepub.piece_update(tpiece, &None, puo.into());
      prepub.log_updates(logents);
      prepub.add_unprepared(uu_d);
    }) as _]

  }))()) // <- no ?
  }))}
}
