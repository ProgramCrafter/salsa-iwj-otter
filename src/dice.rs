// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

//! Dice
//!
//! A "Die" piece
//!  - has image(s), which is another piece which it displays
//!  - can display a text string on top of that shape, per face
//!  - manages flipping
//!  - has a roll function to select a random face
//!  - displays and manages a countdown timer

use crate::prelude::*;

const MAX_COOLDOWN: Duration = Duration::from_secs(100);
fn default_cooldown() -> Duration { Duration::from_millis(4000) }
fn default_circle_scale() -> f64 { 1. }

// Copy of the value from die.svg's "timeblack"'s stroke-width
const COOLDOWN_STROKE_WIDTH: f64 = 1.512;

const COOLDOWN_EXTRA_RADIUS: f64 =
  0.5 * (SELECT_STROKE_WIDTH + COOLDOWN_STROKE_WIDTH) +
  DEFAULT_EDGE_WIDTH;

const DEFAULT_LABEL_FONT_SIZE: f64 = 8.;

#[derive(Debug,Serialize,Deserialize)]
pub struct Spec {
  // must be >1 faces on image, or >1 texts, and if both, same number
  image: Box<dyn PieceSpec>,
  #[serde(default)] labels: SpecLabels,
  occult: Option<OccultSpec>,
  // 1.0 means base outline circle size on most distant corner of bounding box
  // minimum is 0.5; maximum is 1.5
  #[serde(default="default_circle_scale")] circle_scale: f64,
  #[serde(default="default_cooldown")]
  #[serde(with="humantime_serde")] cooldown: Duration,
  itemname: Option<String>,
}

#[derive(Debug,Default,Clone,Serialize,Deserialize)]
pub struct OccultSpec {
  #[serde(default)] label: String,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
#[serde(untagged)]
pub enum SpecLabels {
  Texts(IndexVec<FaceId, String>),
  OneTo(u8),
  RangeInclusive([u8; 2]),
}
use SpecLabels as SL;
impl Default for SpecLabels {
  fn default() -> Self { Self::Texts(default()) }
}

#[derive(Debug,Serialize,Deserialize)]
struct Die {
  /// When occulted, the number of faces of the un-occulted version,
  ///
  /// Even though when occulted we only ever show one face, face 0.
  nfaces: RawFaceId,
  itemname: String,
  labels: IndexVec<FaceId, String>, // if .len()==1, always use [0]
  image: Arc<dyn InertPieceTrait>, // if image.nfaces()==1, always use face 0
  surround_outline: CircleShape,
  cooldown_radius: f64,
  cooldown_time: Duration,
}

#[derive(Debug,Serialize,Deserialize)]
struct State {
  cooldown_expires: Option<FutureInstant>,
}

#[typetag::serde(name="Die")]
impl PieceXData for State {
  fn dummy() -> Self { State { cooldown_expires: None } }
}

#[derive(Serialize, Debug)]
struct OverlayTemplateContext<'c> {
  label_text: &'c str,
  label_font_size: f64,
  label_y_adjust: f64,

  cooldown_active: bool,
  radius: f64,
  remprop: f64,
  path_d: &'c str,
  cd_elid: &'c str,
  total_ms: f64,
}

#[typetag::serde(name="Die")]
impl PieceSpec for Spec {
  #[throws(SpecError)]
  fn load(&self, _: usize, gpc: &mut GPiece, ig: &Instance, depth: SpecDepth)
          -> SpecLoaded {
    gpc.rotateable = false;

    let SpecLoadedInert { p: image, occultable: img_occultable } =
      self.image.load_inert(ig, depth)?;

    let mut nfaces: Option<(RawFaceId, &'static str)> = None;
    let mut set_nfaces = |n, why: &'static str| {
      if let Some((already, already_why)) = nfaces {
        if already != n {
          throw!(SpecError::WrongNumberOfFaces {
            got: n,
            got_why: why.into(),
            exp: already,
            exp_why: already_why.into(),
          })
        }
      }
      nfaces = Some((n, why));
      Ok::<_,SpecError>(())
    };

    match image.nfaces() {
      0 => throw!(SpecError::ZeroFaces),
      1 => { },
      n => set_nfaces(n, "image")?,
    }

    let range_labels = |a: RawFaceId, b: RawFaceId| {
      if a >= b { throw!(SpecError::InvalidRange(a.into(), b.into())) }
      let l = (a..=b).map(|i| i.to_string()).collect();
      Ok::<IndexVec<FaceId,String>,SpecError>(l)
    };

    let labels = match self.labels {
      SL::Texts(ref l) => l.clone(),
      SL::OneTo(n) => range_labels(1,n)?,
      SL::RangeInclusive([a,b]) => range_labels(a,b)?,
    };

    let labels = if labels.len() > 0 {
      let n = labels.len();
      let n = n.try_into().map_err(|_| SpecError::FarTooManyFaces(n))?;
      set_nfaces(n, "labels")?;
      labels.into()
    } else {
      index_vec!["".into()]
    };

    if_let!{ Some((nfaces,_)) = nfaces;
             else throw!(SpecError::MultipleFacesRequired) };

    let radius = if (0.5 .. 1.5).contains(&self.circle_scale) {
      image.bbox_approx()?.size()?.len()? * 0.5 * self.circle_scale
    } else {
      throw!(SpecError::InvalidSizeScale)
    };
    let surround_outline = CircleShape { diam: radius * 2. };
    let cooldown_radius = radius + COOLDOWN_EXTRA_RADIUS;

    let cooldown_time = {
      let t = self.cooldown;
      if t <= MAX_COOLDOWN { t }
      else { throw!(SpecError::TimeoutTooLarge { got: t, max: MAX_COOLDOWN }) }
    };
    let itemname = self.itemname.clone().unwrap_or_else(
      || format!("die.{}.{}", nfaces, image.itemname()));

    let initial_state = {
      State { cooldown_expires: cooldown_start_value(cooldown_time)? }
    };
    let _state: &mut State = gpc.xdata_mut(|| initial_state)?;

    let occ_label = |occ: &OccultSpec| -> String {
      if occ.label == "" && labels.iter().any(|l| l != "") {
        "?".into()
      } else {
        occ.label.clone()
      }
    };

    let occultable = match (img_occultable, &self.occult) {
      (None, None) => None,
      (None, Some(occ)) => {
        let SpecLoadedInert { p: image, occultable: image_occ_reload } =
          self.image.load_inert(ig, depth)?;

        if image_occ_reload.is_some() {
          throw!(internal_logic_error(
            format!("reloading image {:?} occ varies", &self)
          ));
        }
        if image.nfaces() != 1 {
          throw!(SpecError::UnoccultableButRichImageForOccultation)
        }
        let occ_label = occ_label(occ);
        Some((image.into(), occ_label))
      },
      (Some((_, image_occ_image)), occ) => {
        let default_occ = default();
        let occ = occ.as_ref().unwrap_or(&default_occ);
        let occ_label = occ_label(occ);
        Some((image_occ_image, occ_label))
      },
    }.map(|(occ_image, occ_label)| {
      let occ_ilk = LOI::Distinct;
      let our_occ_image = Arc::new(Die {
        nfaces, cooldown_time, cooldown_radius, surround_outline,
        itemname: itemname.clone(),
        image: occ_image,
        labels: index_vec![occ_label],
      }) as _;

      Ok::<_,SpecError>((occ_ilk, our_occ_image))
    }).transpose()?;

    let die = Die {
      nfaces, cooldown_time, cooldown_radius, surround_outline,
      itemname, labels,
      image: image.into()
    };

    SpecLoaded {
      p: Box::new(die) as _,
      occultable,
      special: default(),
    }
  }
}

#[throws(IE)]
pub fn cooldown_start_value(cooldown_time: Duration) -> Option<FutureInstant> {
  let t = cooldown_time.try_into().map_err(IE::from)?;
  Some(t)
}

impl Die {
  #[throws(IE)]
  pub fn cooldown_remaining(&self, state: &State) -> Duration {
    let expires = &state.cooldown_expires;
    if_let!{ Some(FutureInstant(then)) = *expires; else return Ok(default()) };
    let now = Instant::now();
    if now > then { return default() }
    let remaining = then - now;
    if remaining > self.cooldown_time {
      throw!(internal_logic_error(format!(
        "die {:?}: cooldown time remaining {:?} > total {:?}!, resetting",
        &self.itemname, 
        remaining, self.cooldown_time
      )))
    }
    remaining
  }

  #[throws(IE)]
  pub fn cooldown_remprop(&self, state: &State) -> f64 {
    self.cooldown_remaining(state)?.as_secs_f64()
      /
    self.cooldown_time             .as_secs_f64()
  }

  #[throws(IE)]
  pub fn cooldown_running(&self, state: &State) -> bool {
    self.cooldown_remaining(state)? != Duration::default()
  }

  #[throws(ApiPieceOpError)]
  pub fn check_permit_flip_roll(&self, state: &State) {
    if self.cooldown_running(state)? {
      throw!(Inapplicable::DieCooldown)
    } else {
      ()
    }        
  }

  /// Possible stores None, saving us calling Instant::now in the future
  #[throws(IE)]
  pub fn cooldown_cleanup(&self, state: &mut State) {
    if ! self.cooldown_running(state)? {
      state.cooldown_expires = None;
    }
  }

  #[throws(IE)]
  pub fn cooldown_cleanup_hook(&self, gpieces: &mut GPieces, piece: PieceId) {
    let state = gpieces
      .byid_mut(piece).context("load hook")?
      .xdata.get_mut_exp()?;
    self.cooldown_cleanup(state)?;
  }
}

#[dyn_upcast]
impl OutlineTrait for Die {
  // We have done our own adjustable radius adjustment, so we
  // apply that to surround_outline's outline_path, rather than its
  // surround_path.
  #[throws(IE)]
  fn surround_path(&self) -> Html { self.surround_outline.outline_path(1.0)? }

  delegate! {
    to self.image {
      // `outline_path` won't be called at all,
      // since we provide `surround_path`
      fn outline_path(&self, scale: f64) -> Result<Html, IE>;
      fn thresh_dragraise(&self) -> Result<Option<Coord>, IE>;
      fn bbox_approx(&self) -> Result<Rect, IE>;
    }
  }
}

#[dyn_upcast]
impl PieceBaseTrait for Die {
  fn nfaces(&self) -> RawFaceId { self.nfaces }

  fn itemname(&self) -> &str { &self.itemname }

  #[throws(IE)]
  fn special(&self) -> Option<SpecialClientRendering> {
    Some(SpecialClientRendering::Die{})
  }
}

#[typetag::serde(name="Die")]
impl PieceTrait for Die {
  #[throws(IE)]
  fn describe_html(&self, gpc: &GPiece, _: &GameOccults) -> Html {
    let nfaces = self.nfaces();
    let label = &self.labels[gpc.face];
    let idesc = || self.image.describe_html(gpc.face);
    let ldesc = || Html::from_txt(label);
    if label == "" {
      hformat!("a d{} (now showing {})", nfaces, idesc()?)
    } else if self.labels.iter().filter(|&l| l == label).count() == 1 {
      hformat!("a d{} (now showing {})", nfaces, ldesc())
    } else {
      hformat!("a d{} (now showing {}, {})", nfaces, idesc()?, ldesc())
    }
  }

  #[throws(ApiPieceOpError)]
  fn ui_permit_flip(&self, gpc: &GPiece) -> bool {
    let state: &State = gpc.xdata.get_exp()?;
    let () = self.check_permit_flip_roll(state)?;
    true
  }

  #[throws(InternalError)]
  fn add_ui_operations(&self, _: ShowUnocculted, upd: &mut Vec<UoDescription>,
                       _gs: &GameState, _gpc: &GPiece) {
    upd.push(UoDescription {
      kind: UoKind::Piece,
      def_key: 'r',
      opname: "roll".to_string(),
      desc: Html::lit("Roll").into(),
      wrc: WRC::UpdateSvg,
    });
  }

  #[throws(ApiPieceOpError)]
  fn ui_operation(&self, _: ShowUnocculted, args: ApiPieceOpArgs<'_>,
                  opname: &str, wrc: WhatResponseToClientOp)
                  -> UpdateFromOpComplex {
    let ApiPieceOpArgs { gs,piece,player,ioccults,ipc,.. } = args;
    let gpc = gs.pieces.byid_mut(piece)?;
    let gpl = gs.players.byid(player)?;
    let state = gpc.xdata.get_mut_exp()?;

    match (opname, wrc) {
      ("roll", wrc@ WRC::UpdateSvg) => {

        let () = self.check_permit_flip_roll(state)?;
        state.cooldown_expires = cooldown_start_value(self.cooldown_time)?;
        gpc.face = config().game_rng.range(0 .. self.nfaces).into();

        let logents = log_did_to_piece(
          ioccults,&gs.occults,gpl,gpc,ipc,
          "rolled"
        )?;
        ((
          wrc,
          PieceUpdateOp::Modify(()),
          logents,
        ).into(), None)

      },
      _ => throw!(Ia::BadUiOperation)
    }
  }

  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, gpc: &GPiece, _: &GameState,
               vpid: VisiblePieceId) {
    self.svg(f, vpid, gpc.face, &gpc.xdata)?
  }

  #[throws(IE)]
  fn held_change_hook(&self,
                      _: &InstanceRef,
                      gpieces: &mut GPieces,
                      piece: PieceId,
                      _was_held: Option<PlayerId>)
                      -> UnpreparedUpdates {
    self.cooldown_cleanup_hook(gpieces, piece)?;
    None
  }

  #[throws(IE)]
  fn loaded_hook(&self, piece: PieceId, gs: &mut GameState, _: &InstanceRef) {
    self.cooldown_cleanup_hook(&mut gs.pieces, piece)?;
  }
}

#[typetag::serde(name="Die")]
impl InertPieceTrait for Die {
  #[throws(IE)]
  fn svg(&self, f: &mut Html, vpid: VisiblePieceId, face: FaceId,
         xdata: &PieceXDataState /* use with care! */) {
    let state: &State = xdata.get_exp()?;

    // This is called by PieceTrait::svg_piece, so face may be non-0
    // despite the promise about face in InertPieceTrait.
    let iface = if self.image.nfaces() == 1 { default() } else { face };
    self.image.svg(f, vpid, iface, xdata)?;

    let label = self.labels.get(face).map(|s| &**s).unwrap_or_else(
        || self.labels.get(0).map(|s| &**s) .unwrap_or_default());

    let remprop = self.cooldown_remprop(state)?;

    let cooldown_active = remprop != 0.;

    let (path_d, cd_elid) = if cooldown_active {
      let mut path_d = String::new();
      die_cooldown_path(&mut path_d, self.cooldown_radius, remprop)?;
      (path_d, format!("def.{}.die.cd", vpid))
    } else {
      default()
    };

    let label_font_size = DEFAULT_LABEL_FONT_SIZE;
    

    let tc = OverlayTemplateContext {
      label_text: &label,
      label_font_size,
      label_y_adjust: label_font_size * SVG_FONT_Y_ADJUST_OF_FONT_SIZE,

      cooldown_active,
      radius: self.cooldown_radius,
      path_d: &path_d,
      cd_elid: &cd_elid,
      total_ms: self.cooldown_time.as_secs_f64() * 1000.,
      remprop,
    };

    write!(f.as_html_string_mut(), "{}",
           nwtemplates::render("die-overlay.tera", &tc)?)?;
  }

  // add throw operation
  #[throws(IE)]
  fn describe_html(&self, face: FaceId) -> Html {
    let label = &self.labels[face];
    let idesc = || self.image.describe_html(face);
    let ldesc = || Html::from_txt(label);
    if label == "" {
      hformat!("a d{} ({})", self.nfaces, idesc()?)
    } else {
      hformat!("a d{} ({}, {})", self.nfaces, idesc()?, ldesc())
    }
  }
}
