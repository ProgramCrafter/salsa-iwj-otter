// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

type Fwd = BTreeMap<String, Box<dyn PieceSpec>>;

#[derive(Debug,Default,Serialize,Deserialize)]
#[serde(transparent)]
pub struct PieceAliasesSave(Fwd);

#[derive(Debug,Default,Serialize,Deserialize)]
pub struct PieceAliases {
  fwd: Fwd,
}

impl PieceAliases {
  pub fn remove(&mut self, alias: &str) {
    self.fwd.remove(alias);
  }

  pub fn insert(&mut self, alias: String, target: Box<dyn PieceSpec>) {
    self.fwd.insert(alias, target);
  }

  pub fn keys(&self) -> impl Iterator<Item=&String> {
    self.fwd.keys()
  }
}

#[derive(Debug,Clone,Serialize,Deserialize)]
struct Alias {
  target: String,
}

impl Alias {
  #[throws(SpecError)]
  fn resolve<'a>(&self, pcaliases: &'a PieceAliases) -> &'a dyn PieceSpec {
    Box::as_ref(
      pcaliases
        .fwd
        .get(&self.target)
        .ok_or(SpecError::AliasNotFound)?
    )
  }
}

#[typetag::serde]
impl PieceSpec for Alias {
  #[throws(SpecError)]
  fn count(&self, pcaliases: &PieceAliases) -> usize {
    // passing default() avoids resolving alias chains, so we don't
    // have to worry about cycles
    self.resolve(pcaliases)?.count(&default())?
  }
  #[throws(SpecError)]
  fn load(&self, i: usize, gpc: &mut GPiece,
          pcaliases: &PieceAliases, ir: &InstanceRef)
          -> PieceSpecLoaded {
    self.resolve(pcaliases)?.load(i, gpc, &default(), ir)?
  }
  #[throws(SpecError)]
  fn load_occult(&self, pcaliases: &PieceAliases)
                 -> Box<dyn OccultedPieceTrait> {
    self.resolve(pcaliases)?.load_occult(&default())?
  }
}

