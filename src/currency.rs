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

const DEFAULT_QTY_FONT_SIZE: f64 = 6.;

type Qty = MultigrabQty;

#[derive(Debug,Serialize,Deserialize)]
pub struct Spec {
  image: Box<dyn PieceSpec>,
  qty: Qty,
  currency: String,
  #[serde(default)] label: LabelSpec,
}

#[derive(Debug,Default,Clone,Serialize,Deserialize)]
pub struct LabelSpec {
  pub unit_rel_size: Option<f64>,

  #[serde(flatten,default)]
  pub options: TextOptionsSpec,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct Banknote {
  itemname: String,
  image: Arc<dyn InertPieceTrait>,
  currency: String,
  unit_size: f64,
  label_options: TextOptions,
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

    let Spec { ref image, ref currency, qty, label: LabelSpec {
      options: ref label_options, unit_rel_size,
    } } = *self;

    let label_options = label_options.resolve(DEFAULT_QTY_FONT_SIZE)?;
    let unit_size = label_options.size * unit_rel_size.unwrap_or(1.);

    let SpecLoadedInert { p: image, occultable: image_occultable } =
      image.load_inert(ig, depth)?;

    let itemname = format!("currency-{}", image.itemname());

    if image.nfaces() != 1 {
      throw!(SpecError::WrongNumberOfFaces {
        got: image.nfaces(), got_why: "image".into(),
        exp: 1,              exp_why: "needed".into(),
      });
    }

    let _value: &mut Value = gpc.xdata_mut(|| Value { qty })?;
    let image: Arc<dyn InertPieceTrait> = image.into();

    let occultable = Some({
      let image = image_occultable
        .map(|(_,o)| o)
        .unwrap_or_else(|| image.clone());

      (
        LOI::Distinct,
        Arc::new(Banknote {
          image,
          currency: currency.clone(),
          itemname: itemname.clone(),
          label_options: label_options.clone(),
          unit_size,
        }) as _
      )
    });
        
    let bnote = Banknote {
      image,
      currency: currency.clone(),
      itemname, label_options, unit_size,
    };

    gpc.fastsplit = FastSplitId::new_placeholder();

    let special = PieceSpecialProperties {
      multigrab: true,
      ..default()
    };
    SpecLoaded { p: Box::new(bnote) as _, occultable, special }
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
    let show = ShowUnocculted::new_visible(); // we are in PieceTrait, so ok
    let value: &Value = gpc.xdata.get_exp()?;
    self.describe(gpc.face, &value.html(Some(show)))?
  }

  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, gpc: &GPiece, _gs: &GameState,
               vpid: VisiblePieceId) {
    let show = ShowUnocculted::new_visible(); // we are in PieceTrait, so ok
    let value: &Value = gpc.xdata.get_exp()?;
    self.render(f, vpid, gpc.face, &gpc.xdata, &value.html(Some(show)))?
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
      "{} took {}, leaving {}{}",
      gpl.nick.to_html(),
      self_.describe(tgpc.face, &tgpc_value.html(Some(show)))?,
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

    if_let!{ Some(player) = was_held; else return Ok(default()) }
    if tgpc.held.is_some() { /*wat*/ return default(); }
    let gpl = gplayers.get(player);

    let merge_with = gpieces.iter().filter_map(|(mpiece, mgpc)|{
      if mpiece == tpiece { throw!() }
      let mipc = ipieces.get(mpiece)?;

      // We're to merge with something the moving player can see
      let show_to_player = mgpc.fully_visible_to(goccults, player)?;

      // Our position is within its bbox
      if ! mipc.show(show_to_player).abs_bbox(mgpc).ok()?
        .contains(tgpc.pos) { throw!() }

      // It's a banknote
      let mself: &Banknote = mipc.p.show(show_to_player)
        .downcast_piece_fastsplit().ok()?;

      // Of our currency
      if mself.currency != tself.currency { throw!() }
      let currency = &mself.currency;

      // We are in the ellipse inscribed in its bbox
      let delta = (tgpc.pos - mgpc.pos).ok()?.promote();
      let bbox_sz = mipc.show(show_to_player).bbox_approx().ok()?;
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

    let tvalue = tgpc.xdata_exp::<Value>()?;
    let tqty = tvalue.qty;
    let mqty = mgpc.xdata_exp::<Value>()?.qty;
    let new_value = match mqty.checked_add(tqty) {
      Some(qty) => Value { qty },
      None => return default(), // arithmetic overflow!
    };

    let show_tqty    = tgpc.fully_visible_to_everyone();
    let show_new_qty = mgpc.fully_visible_to_everyone();

    let logent = hformat!(
      "{} deposited {}, giving {}{}",
      match gpl {
        Some(gpl) => gpl.nick.to_html(),
        None => Html::lit("Departing player").into(),
      },
      tself.describe(tgpc.face, &tvalue.html(show_tqty))?,
      &new_value.html(show_new_qty),
      currency,
    );

    let logents = vec![ LogEntry { html: logent } ];

  OpHookThunk::Reborrow(Box::new(move |igg: &mut InstanceGuard, (_player,)| {

    let (puo, uu_d) = igg.fastsplit_delete(tpiece, &logents)?;
    // commitment point
  Ok((move ||{
    let ig = &mut **igg;

    let () = (||{
      // None of these situations ought to happen, really, but the
      // callback structure means it isn't 100% possible to rule them out.
      let mgpc = ig.gs.pieces.get_mut(mpiece).ok_or("tpiece vanished")?;
      let mvalue = mgpc.xdata_mut_exp::<Value>().map_err(|_|"xdata vanished")?;
      mvalue.qty = mvalue.qty.checked_add(tqty).ok_or("overflow")?;
      if mvalue.qty != new_value.qty { throw!("modified value") }
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

  })()) // <- no ?
  }))}
}

const OCCULT_QTY: HtmlLit = Html::lit("?");

impl Value {
  fn html(&self, show: Option<ShowUnocculted>) -> Html {
    if show.is_some() {
      hformat!("{}", self.qty)
    } else {
      hformat!("{}", OCCULT_QTY)
    }
  }
}

impl Banknote {
  #[throws(IE)]
  fn describe(&self, face: FaceId, qty: &HtmlStr) -> Html {
    hformat!("{}, {}{}",
             self.image.describe_html(face)?,
             qty, &self.currency)
  }

  #[throws(IE)]
  fn render(&self, f: &mut Html, vpid: VisiblePieceId, face: FaceId,
            xdata_for_image_only: &PieceXDataState, qty: &HtmlStr) {
    self.image.svg(f, vpid, face, xdata_for_image_only)?;

    hwrite!(f,
            r##"<{}>{}<tspan font-size="{}">{}</tspan></text>"##,
            &self.label_options.start_element(), qty,
            &self.unit_size, &self.currency)?;
  }
}

#[typetag::serde(name="Currency")]
impl InertPieceTrait for Banknote {
  #[throws(IE)]
  fn svg(&self, f: &mut Html, id: VisiblePieceId, face: FaceId,
         xdata_for_image_only: &PieceXDataState /* use with care! */) {
    self.render(f, id, face, xdata_for_image_only, &OCCULT_QTY)?;
  }

  #[throws(IE)]
  fn describe_html(&self, face: FaceId) -> Html {
    self.describe(face, &OCCULT_QTY)?
  }
}
