// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

use parking_lot::Mutex;

#[derive(Deserialize,Debug,Clone,Default)]
#[serde(transparent)]
pub struct FakeRngSpec(Vec<String>);

impl FakeRngSpec {
  pub fn start(self) -> RngWrap { RngWrap(
    if self.0.is_empty() { None }
    else { Some(Arc::new(FakeRng {
      i: Mutex::new(0),
      ents: self.0,
    })) }
  )}
}

#[derive(Debug,Clone)]
pub struct RngWrap (
  Option<Arc<FakeRng>>
);

#[derive(Debug)]
struct FakeRng {
  i: Mutex<usize>,
  ents: Vec<String>,
}

impl RngWrap {
  pub fn is_fake(&self) -> bool { self.0.is_some() }

  #[throws(as Option)]
  fn next(&self) -> &str {
    let fake = self.0.as_ref()?;
    let mut i = fake.i.lock();
    let e = fake.ents[*i].as_str();
    *i += 1;
    *i %= fake.ents.len();
    e
  }

  pub fn shuffle<T:Copy>(&self, slice: &mut [T]) { match self.next() {
    None => {
      let mut rng = thread_rng();
      slice.shuffle(&mut rng);
    },
    Some(s) => {
      let l = slice.len();
      let n: usize = s.parse().unwrap_or(0);
      let front = slice[0..n].to_owned();
      slice.copy_within(n.., 0);
      slice[l-n..].copy_from_slice(&front);
    },
  } }
} 
