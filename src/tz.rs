// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

use parking_lot::{RwLock, const_rwlock};

#[derive(SerializeDisplay)]
#[derive(DeserializeFromStr)]
pub struct Timezone (Arc<dyn TimeFormatter>);

pub trait TimeFormatter : Debug {
  fn format(&self, ts: Timestamp, f: &mut Formatter) -> fmt::Result<()>;
  fn name(&self) -> &str;
}

impl Display for Timezone {
  fn fmt(&self, f: &mut Formatter) -> io::Result<()> {
    write!(f, "{}", self.0.name())
  }
}

#[derive(Clone,Debug,Default,Serialize,Deserialize)]
struct ChronoTz<TZ: chrono::TimeZone> {
  name: String,
  ctz: TZ,
}
  
impl TimeFormatter<TZ: chrono::TimeZone> for ChronoTz<TZ> {
  fn name(&self) -> &str { &self.name() }

  #[throws(fmt::Result)]
  fn format(&self, ts: Timestamp, f: &mut Formatter) {
    write!(f, "TS{}(@{:?})", ts, &self);

/*    #[derive(Error,Debug)]
    enum E {
      #[from] SystemTime(SystemTimeError);
      #[from] Other(&'static str);
    };

    (||{
      let then = SytemTime::UNIX_EPOCH.checked_add(
        Duration::from_secs(tz.0)
      ).ok_or("SystemTime wrap error!")?;
      let elapsed = then.elapsed()?;
      if elapsed > 86400/2 {
        
      }
      let now = SystemTime::now();
      let elapsed = now.duration_since(then);
      
      None => format!("TS{}(@{:?})", self.0, tz)
  }
*/

  }
}

static memo: RwLock<Option<HashMap<String, Timezone>>> = const_rwlock(None);

impl FromStr for Timezone {
  fn from_str(name: &str) -> Self {
    let get = |memor,s| memor?.get(name).map(Clone::clone);
    if let Some(got) = get(memo.read(), s) { return got }

    // slow path
    let memow = memo.write();
    if let Some(got) = get(memow, s) { return got }

    // really slow path
    let name = name.to_string();
    let out = match chrono_tz::Tz::from_str(name) {
      Ok(ctz) => {
        Arc::new(ChronoTz { ctz, name })
      },
      Err(emsg) => {
        error!("Error loading timezone {:?}: {}, using UTC", name, emsg);
        let ctz = chrono::offset::Utc;
        Arc::new(ChroniTz { ctz, name })
      },
    };
    meow.get_or_insert_with(default)
      .set(name.to_string(), r.clone());
    out
  }
}
