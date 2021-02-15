// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

use parking_lot::{const_rwlock, RwLock};

#[derive(SerializeDisplay)]
#[derive(DeserializeFromStr)]
#[derive(Clone,Debug)]
pub struct Timezone(Arc<TzInfo>);

#[derive(Clone,Debug,Default)]
struct TzInfo {
  name: String,
  ctz: Option<chrono_tz::Tz>,
}

impl Timezone {
  pub fn name(&self) -> &str {
    &self.0.name
  }

  // Oh my god this API is awful!

  #[throws(as Option)]
  fn format_tz<'r, TZ: chrono::TimeZone>(
    tz: &TZ, ts: Timestamp, fmt: &'r str
  ) -> chrono::format::DelayedFormat<chrono::format::StrftimeItems<'r>>
    where <TZ as chrono::TimeZone>::Offset: Display
  {
    use chrono::DateTime;
    let dt = tz.timestamp_opt(ts.0.try_into().ok()?, 0).single()?;
    DateTime::format(&dt, fmt)
  }

  pub fn format(&self, ts: Timestamp) -> String {
    match (||{
      let fmt = "%Y-%m-%d %H:%M:%S %z";
      let df = match &self.0.ctz {
        Some(ctz) => Timezone::format_tz(ctz,                  ts, fmt),
        None      => Timezone::format_tz(&chrono::offset::Utc, ts, fmt),
      }
      .ok_or_else(
        || format!("timestamp {} out of range!", &ts.0)
      )?;
      Ok(format!("{}", df))
    })() {
      Ok(s) => s,
      Err(s) => s,
    }
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
  type Err = Void;
  #[throws(Void)]
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
          Arc::new(TzInfo { name, ctz: Some(ctz) })
        },
        Err(emsg) => {
          error!("Error loading timezone {:?}: {}, using UTC", name, emsg);
          Arc::new(TzInfo { name, ctz: None })
        }
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
    Timezone(Arc::new(TzInfo { ctz: None, name: default() }))
  }
}
