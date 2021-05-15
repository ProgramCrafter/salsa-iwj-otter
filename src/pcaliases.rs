// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

type Map = BTreeMap<String, Box<dyn PieceSpec>>;

#[derive(Debug,Default,Serialize,Deserialize)]
#[serde(transparent)]
pub struct PieceAliases {
  map: Map,
}

impl PieceAliases {
  pub fn remove(&mut self, alias: &str) {
    self.map.remove(alias);
  }

  pub fn insert(&mut self, alias: String, target: Box<dyn PieceSpec>) {
    self.map.insert(alias, target);
  }

  pub fn keys(&self) -> impl Iterator<Item=&String> {
    self.map.keys()
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
        .map
        .get(&self.target)
        .ok_or(SpecError::AliasNotFound)?
    )
  }

  #[throws(SpecError)]
  fn new_depth(&self, depth: SpecDepth) -> SpecDepth {
    depth.increment().ok_or_else(||{
      SpE::AliasLoop(self.target.clone())
    })?
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
  fn load(&self, i: usize, gpc: &mut GPiece, ig: &Instance, depth: SpecDepth)
          -> PieceSpecLoaded {
    let mut r = self.resolve(&ig.pcaliases)?
      .load(i, gpc, ig, self.new_depth(depth)?)?;
    r.loaded_via_alias = Some(self.target.clone());
    r
  }
  #[throws(SpecError)]
  fn load_occult(&self, ig: &Instance, depth: SpecDepth)
                 -> Box<dyn OccultedPieceTrait> {
    self.resolve(&ig.pcaliases)?.load_occult(ig, self.new_depth(depth)?)?
  }
}

