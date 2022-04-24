// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

use parking_lot::Mutex;

type Millis = u32;
type Micros = u64;

#[derive(Deserialize,Debug,Clone,Default)]
#[serde(transparent)]
pub struct FakeTimeConfig(Option<FakeTimeSpec>);

#[derive(Deserialize,Serialize,Debug,Clone,Default)]
#[serde(transparent)]
pub struct FakeTimeSpec(Option<Millis>);

#[derive(Debug)]
pub struct GlobalClock {
  fakeable: Option<Mutex<Option<FakeClock>>>,
}

#[derive(Debug)]
struct FakeClock {
  start: Instant,
  current: Micros,
}

impl FakeClock {
  fn from_millis(ms: Millis) -> FakeClock { FakeClock {
    start: Instant::now(),
    current: (ms as Micros) * 1000,
  } }

  fn from_spec(FakeTimeSpec(fspec): FakeTimeSpec) -> Option<FakeClock> {
    fspec.map(FakeClock::from_millis)
  }
}

impl FakeTimeConfig {
  pub fn make_global_clock(self) -> GlobalClock {
    let fakeable = self.0.map(|fspec| FakeClock::from_spec(fspec).into());
    GlobalClock { fakeable }
  }
}

impl GlobalClock {
  pub fn now(&self) -> Instant {
    self.now_fake().unwrap_or_else(|| Instant::now())
  }

  #[throws(as Option)]
  fn now_fake(&self) -> Instant {
    let mut guard = self.fakeable.as_ref()?.lock();
    let fake = guard.as_mut()?;
    fake.current += 1;
    fake.start + Duration::from_micros(fake.current)
  }

  #[throws(MgmtError)]
  pub fn set_fake(&self, fspec: FakeTimeSpec, _: AuthorisationSuperuser) {
    let mut guard = self.fakeable.as_ref().ok_or(ME::TimeIsReal)?.lock();
    *guard = FakeClock::from_spec(fspec)
  }

  pub fn is_fake(&self) -> bool {
    self.fakeable.is_some()
  }
} 
