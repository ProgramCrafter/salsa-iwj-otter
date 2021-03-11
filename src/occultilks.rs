// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

slotmap::new_key_type!{ pub struct OccultIlkId; }

/// Does *not* `impl Drop`.  Don't just drop it.
#[derive(Debug,Serialize,Deserialize)]
#[serde(transparent)]
pub struct OccultIlkOwningId(Id);

#[derive(Debug,Clone,Eq,PartialEq,Ord,PartialOrd,Hash)]
#[derive(Serialize,Deserialize)]
#[serde(transparent)]
pub struct OccultIlkName(pub Arc<String>);

#[derive(Debug,Serialize,Deserialize)]
pub struct OccultIlkData {
  pub p_occ: Box<dyn OccultedPieceTrait>,
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

  pub fn insert(&mut self, k: K, v: V) -> OId {
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
    let newrefcount = data.refcount.checked_sub(1).unwrap();
    if newrefcount == 0 {
      let data = self.table.remove(id).unwrap();
      self.lookup.remove(&data.k);
    }
  }
}

impl Borrow<Id> for OId {
  fn borrow(&self) -> &Id { &self.0 }
}
