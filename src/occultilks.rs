// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

slotmap::new_key_type!{ pub struct OccultIlkId; }

/// Does *not* `impl Drop`.  Don't just drop it.
#[derive(Debug,Serialize,Deserialize)]
#[serde(transparent)]
pub struct OccultIlkOwningId(Id);

pub type OccultIlkName = Arc<GoodItemName>;

#[derive(Debug,Serialize,Deserialize)]
pub struct OccultIlkData {
  pub p_occ: Arc<dyn InertPieceTrait>,
}

#[derive(Debug,Clone)]
pub enum LOccultIlk {
  /// Pieces does not participate in ilk-based mixing.
  ///
  /// Such a piece remains distinguishable from all other pieces, and
  /// trackable, by all players, even when occulted (unless made
  /// totally invisible, in which case it will still be trackable
  /// when it returns).
  Distinct,

  /// Ilk-based mixing
  ///
  /// Pieces with the same OccultIlkName will be mixed together when
  /// occulted, so that players who see the occulted view cannot track
  /// the individual piece identities.  This supports deck-shuffling
  /// for pickup decks (and for hands etc. hides what the player is
  /// doing with their own cards, from the other players).
  Mix(OccultIlkName),
}

serde_with_compat!{
  [ #[derive(Debug,Serialize,Deserialize)] ]
  [ pub ][ enum ] IOccultIlk="IOccultIlk" IOccultIlk_New "IOccultIlk_Compat" [
    {
      Distinct(OccultIlkData),
      Mix(OccultIlkOwningId),
    }
  ]
}

#[derive(Debug,Deserialize)]
#[serde(untagged)]
#[allow(non_camel_case_types)]
enum IOccultIlk_Compat {
  V1(OccultIlkOwningId), // Otter 1.0.0
  V2(#[serde(with="IOccultIlk_New")] IOccultIlk),
}
  
type Id = OccultIlkId;
type OId = OccultIlkOwningId;
type K = OccultIlkName;
type V = OccultIlkData;
type Refcount = u32;

#[derive(Debug,Serialize,Deserialize,Default)]
pub struct OccultIlks {
  lookup: HashMap<K, Id>,
  table: DenseSlotMap<Id, Data>,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct Data {
  k: K, // duplicated, ah well
  v: V,
  refcount: Refcount,
}

impl TryFrom<IOccultIlk_Compat> for IOccultIlk {
  type Error = Infallible;
  #[throws(Infallible)]
  fn try_from(compat: IOccultIlk_Compat) -> IOccultIlk { match compat {
    IOccultIlk_Compat::V1(oioi) => IOI::Mix(oioi),
    IOccultIlk_Compat::V2(ioi) => ioi,
  } }
}

impl OccultIlks {
  #[throws(as Option)]
  pub fn get<I: Borrow<Id>>(&self, id: &I) -> &V {
    &self.table.get(*id.borrow())?.v
  }

  /// Load from an LOccultIlk (as obtained from PieceTrait::load
  pub fn load_lilk(&mut self, lilk: LOccultIlk, data: OccultIlkData)
                   -> IOccultIlk {
    match lilk {
      LOI::Distinct => IOI::Distinct(data),
      LOI::Mix(ilkname) => IOI::Mix(self.create_coalesce(ilkname, data)),
    }
  }

  #[throws(as Option)]
  pub fn from_iilk<'r>(&'r self, iilk: &'r IOccultIlk) -> &'r V { match iilk {
    IOI::Distinct(data) => data,
    IOI::Mix(id) => self.get(id)?,
  } }
  pub fn dispose_iilk(&mut self, iilk: IOccultIlk) { match iilk {
    IOI::Distinct(_data) => { },
    IOI::Mix(id) => self.dispose(id),
  } }

  /// Ensure there's an entry for `K`, perhaps creating from `V`
  pub fn create_coalesce(&mut self, k: K, v: V) -> OId {
    let OccultIlks { lookup, table } = self;
    let id = *lookup
      .entry(k)
      .or_insert_with_key(|k|{
        let data = Data {
          v,
          k: k.clone(),
          refcount: 0,
        };
        table.insert(data)
      });
    table[id].refcount += 1;
    OccultIlkOwningId(id)
  }

  pub fn dispose(&mut self, id: OId) {
    let id: Id = id.0;
    let data = &mut self.table[id];
    data.refcount = data.refcount.checked_sub(1).unwrap();
    if data.refcount == 0 {
      let data = self.table.remove(id).unwrap();
      self.lookup.remove(&data.k);
    }
  }

  pub fn is_empty(&self) -> bool {
    let OccultIlks { lookup, table } = self;
    #[allow(unused_parens)]
    (
         lookup.is_empty()
      && table.is_empty()
    )
  }
}

impl Borrow<Id> for OId {
  fn borrow(&self) -> &Id { &self.0 }
}

impl IOccults {
  pub fn is_empty(&self) -> bool {
    let IOccults { ilks } = self;
    ilks.is_empty()
  }
}
