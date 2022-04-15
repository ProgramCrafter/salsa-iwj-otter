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

impl OccultIlks {
  #[throws(as Option)]
  pub fn get<I: Borrow<Id>>(&self, id: I) -> &V {
    &self.table.get(*id.borrow())?.v
  }

  pub fn create(&mut self, k: K, v: V) -> OId {
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
