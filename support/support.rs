// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

//========== Timestamp ==========

#[derive(Copy,Clone,Debug,Serialize,Deserialize,Eq,Ord,PartialEq,PartialOrd)]
#[serde(transparent)]
pub struct Timestamp(pub u64); /* time_t */

impl Timestamp {
  /// Always >= previously
  pub fn now() -> Timestamp {
    use std::time::SystemTime;
    let now = SystemTime::now()
      .duration_since(SystemTime::UNIX_EPOCH)
      .unwrap()
      .as_secs();
    Timestamp(now)
  }

  pub fn render(&self, tz: &Timezone) -> String {
    tz.format(*self)
  }
}

//========== matches_doesnot ==========

#[macro_export] // <- otherwise bogus warning `unused_macros`
macro_rules! matches_doesnot_yn2bool {
  (=) => (true);
  (!) => (false);
}

#[macro_export]
macro_rules! matches_doesnot {
  ($v:expr,
   $(
     $yn:tt $p:pat
   ),* $(,)?
  ) => {
    match $v {
      $(
        $p => $crate::matches_doesnot_yn2bool!($yn),
      )*
    }
  }
}

#[test]
fn matches_doesnot_test() {
  assert!(
    matches_doesnot!(
      Some(42),
      = Some(_),
      ! None
    )
  );
  assert!(
    matches_doesnot!(
      Some(42),
      ! None,
      ! Some(3),
      = Some(_),
    )
  );
  assert!(
    matches_doesnot!(
      Some(1),
      = Some(1) | Some(2),
      ! Some(_) | None
    )
  );
  assert!(
    ! matches_doesnot!(
      Some(1),
      ! Some(1) | Some(2),
      = Some(_) | None
    )
  );
}

