// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

pub const HELD_SURROUND_COLOUR: &str = "black";

const MONOSPACE: HtmlLit = Html::lit(
  r#"font-family="Latin Modern Mono, monospace" font-weight="700""#);

pub const DEFAULT_TABLE_SIZE: Pos = PosC::new( 300, 200 );
pub const DEFAULT_TABLE_COLOUR: &str = "green";

pub const SELECT_SCALE: f64 = 1.1;

// also in script.ts:redisplay_ancillaries ("halo")
//   nelem.setAttributeNS(null,'stroke-width','2px');
pub const SELECT_STROKE_WIDTH: f64 = 2.0;

pub const DEFAULT_EDGE_WIDTH: f64 = 0.2;
pub const INVISIBLE_EDGE_SENSITIVE: f64 = 2.;

pub const LABEL_FONT_SIZE: f64 = 4.0;

pub const HTML_TEXT_LABEL_ELEM_START: HtmlLit =
  Html::lit(r##"text pointer-events="none""##);

/// Fudge factor
///
/// When trying to centre text, we use text-align and/or text-anchor
/// to do the horizontal positioning, but vertical positioning is
/// troublesome.  We bodge it.  Multiple the font size (in pixels)
/// by this, and add it to the SVG y coordinate (ie, shufting the text
/// down).
pub const SVG_FONT_Y_ADJUST_OF_FONT_SIZE: f64 = 0.35;

pub fn default_edge_width() -> f64 { DEFAULT_EDGE_WIDTH }

pub fn monospace_font(size: u32) -> Html {
  hformat!(r##"{} font-size="{}""##, MONOSPACE, size)
}

#[derive(Clone,Copy,Debug,Eq,PartialEq,Serialize,Deserialize,EnumString)]
pub enum PresentationLayout {
  Portrait,
  Landscape,
}

type PL = PresentationLayout;

pub fn player_num_dasharray(player_num: NonZeroUsize) -> Html {
  let n: usize = player_num.into();
  let mut dasharray = String::with_capacity(n*3 + 4);
  for dash in iter::once("3").chain(
    iter::repeat("1").take(n-1))
  {
    write!(&mut dasharray, "{} 1 ", &dash).unwrap();
  }
  let spc = dasharray.pop();
  assert_eq!(spc,Some(' '));
  Html::from_html_string(dasharray)
}

pub fn player_dasharray(gplayers: &GPlayers, player: PlayerId) -> Html {
  let kd: slotmap::KeyData = player.into();
  let n: usize = kd.get_idx_version().0.try_into().unwrap();
  let n: NonZeroUsize = n.try_into()
    .unwrap_or_else(|_| gplayers.capacity().try_into().unwrap());
  player_num_dasharray(n)
}

pub fn occultation_notify_update_image(piece: PieceId)
                                       -> UnpreparedUpdates {
  Some(Box::new(
    move |updates: &mut PrepareUpdatesBuffer| {
      updates.piece_update_image(piece, &None)
        .unwrap_or_else(|e| error!("unable to send update! {:?}", e))
    }
  ))
}

impl PresentationLayout {
  pub fn template(self) -> &'static str {
    match self {
      PL::Portrait => "session",
      PL::Landscape => "landscape",
    }
  }
  pub fn abbreviate_timestamps(self) -> bool {
    match self {
      PL::Portrait => false,
      PL::Landscape => true,
    }
  }
}

impl Default for PresentationLayout {
  fn default() -> Self { PL::Portrait }
}

#[derive(Debug)]
pub struct AbbrevPresentationLayout(pub PresentationLayout);

#[derive(Error,Debug,Clone,Copy)]
#[error("Invalid presentation layout character")]
pub struct InvalidAbbrevPresentationLayout;

impl FromStr for AbbrevPresentationLayout {
  type Err = InvalidAbbrevPresentationLayout;
  #[throws(Self::Err)]
  fn from_str(s: &str) -> Self {
    AbbrevPresentationLayout(match s {
      "p" => PL::Portrait,
      "l" => PL::Landscape,
      _ => throw!(InvalidAbbrevPresentationLayout)
    })
  }
}

impl Display for AbbrevPresentationLayout {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    f.write_str(match self.0 {
      PL::Portrait => "p",
      PL::Landscape => "l",
    })?
  }
}
