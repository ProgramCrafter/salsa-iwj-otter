// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

pub const SVG_SCALE: f64 = 6.;

pub const HELD_SURROUND_COLOUR: &str = "black";

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
  Html(dasharray)
}

pub fn player_dasharray(gplayers: &GPlayers, player: PlayerId) -> Html {
  let kd: slotmap::KeyData = player.into();
  let n: usize = kd.get_idx_version().0.try_into().unwrap();
  let n: NonZeroUsize = n.try_into()
    .unwrap_or_else(|_| gplayers.capacity().try_into().unwrap());
  player_num_dasharray(n)
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

pub struct AbbrevPresentationLayout(pub PresentationLayout);

impl FromStr for AbbrevPresentationLayout {
  type Err = ();
  #[throws(Self::Err)]
  fn from_str(s: &str) -> Self {
    AbbrevPresentationLayout(match s {
      "p" => PL::Portrait,
      "l" => PL::Landscape,
      _ => throw!(())
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
