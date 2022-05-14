// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

use parking_lot::Mutex;
use rand::distributions::uniform::SampleUniform;

#[derive(Deserialize,Debug,Clone,Default)]
#[serde(transparent)]
pub struct FakeRngSpec(Option<Vec<String>>);

#[derive(Serialize,Deserialize,Error,Debug,Clone)]
#[error("RNG is real")]
pub struct RngIsReal;

impl FakeRngSpec {
  pub fn make_game_rng(self) -> RngWrap { RngWrap( match self.0 {
    None => None,
    Some(ents) => Some(Mutex::new(FakeRng {
      i: 0,
      ents,
    })) }
  )}
}

#[derive(Debug)]
pub struct RngWrap (
  Option<Mutex<FakeRng>>
);

#[derive(Debug)]
struct FakeRng {
  i: usize,
  ents: Vec<String>,
}

impl RngWrap {
  pub fn is_fake(&self) -> bool { self.0.is_some() }

  #[throws(RngIsReal)]
  pub fn set_fake(&self, v: Vec<String>, _: AuthorisationSuperuser) {
    let mut fake = self.0.as_ref().ok_or(RngIsReal)?.lock();
    fake.i = 0;
    fake.ents = v;
  }

  #[throws(as Option)]
  fn next_fake(&self) -> String {
    let mut fake = self.0.as_ref()?.lock();
    let e = fake.ents.get(fake.i)?.clone();
    fake.i += 1;
    fake.i %= fake.ents.len();
    e
  }

  pub fn shuffle<T:Copy>(&self, slice: &mut [T]) { match self.next_fake() {
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

  pub fn range<T>(&self, range: std::ops::Range<T>) -> T
  where T: SampleUniform + FromStr + Ord + Default
  {
    match self.next_fake() {
      None => {
        let mut rng = thread_rng();
        rng.gen_range(range)
      },
      Some(s) => (||{
        let n: T = s.parse().ok()?;
        range.contains(&n).then(|| n)
      })().unwrap_or_default(),
    }
  }
} 
