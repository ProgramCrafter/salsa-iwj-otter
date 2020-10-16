// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

use parking_lot::{RwLock, const_rwlock};

#[derive(SerializeDisplay)]
#[derive(DeserializeFromStr)]
#[derive(Clone,Debug)]
pub struct Timezone (Arc<ChronoTz>);

#[derive(Clone,Debug,Default)]
struct ChronoTz {
  name: String,
  ctz: Option<chrono_tz::Tz>,
}
  
impl Timezone {
  pub fn name(&self) -> &str {
    &self.0.name
  }
  #[throws(fmt::Error)]
  pub fn format<W: fmt::Write>(&self, ts: Timestamp, w: &mut W) {
    write!(w, "TS{:?}(@{:?})", ts, &self)?
//        let ctz = chrono::offset::Utc;
  }

  pub fn default_todo() -> Self {
    Timezone::from_str("").unwrap()
  }
}

impl Display for Timezone {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    write!(f, "{}", &self.0.name)?;
  }
}

type MemoTable = Option<HashMap<String, Timezone>>;
static MEMO: RwLock<MemoTable> = const_rwlock(None);

impl FromStr for Timezone {
  type Err = !;
  #[throws(!)]
  fn from_str(name: &str) -> Self {
    if name.is_empty() { return default() }

    let get = |memor: &MemoTable| memor.as_ref()?.get(name).map(Clone::clone);
    if let Some(got) = get(&MEMO.read()) { return got }

    // slow path
    let mut memow = MEMO.write();
    if let Some(got) = get(&memow) { return got }

    // really slow path
    let out = {
      let name = name.to_string();
      match chrono_tz::Tz::from_str(&name) {
        Ok(ctz) => {
          Arc::new(ChronoTz { name, ctz: Some(ctz) })
        },
        Err(emsg) => {
          error!("Error loading timezone {:?}: {}, using UTC", name, emsg);
          Arc::new(ChronoTz { name, ctz: None })
        },
      }
    };
    let out = Timezone(out);
    memow.get_or_insert_with(default)
      .insert(name.to_string(), out.clone());
    out
  }
}

impl Default for Timezone {
  fn default() -> Self {
    Timezone(Arc::new(ChronoTz { ctz: None, name: default() }))
  }
}
