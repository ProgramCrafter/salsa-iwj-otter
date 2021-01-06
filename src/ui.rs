// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

#[derive(Clone,Copy,Debug,Eq,PartialEq,Serialize,Deserialize,EnumString)]
pub enum PresentationLayout {
  Portrait,
  Landscape,
}

type PL = PresentationLayout;

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
