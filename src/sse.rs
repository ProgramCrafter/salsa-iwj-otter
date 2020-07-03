
use crate::imports::*;

#[derive(Copy,Clone,Debug,Eq,PartialEq,Ord,PartialOrd)]
#[derive(Serialize,Deserialize)]
#[serde(transparent)]
pub struct UpdateCounter (i64);

use vecdeque_stableix::StableIndexOffset;
use std::ops::Neg;

impl Neg for UpdateCounter {
  type Output = Self;
  fn neg(self) -> Self { UpdateCounter(-self.0) }
}

impl StableIndexOffset for UpdateCounter {
  fn try_increment(&mut self) -> Option<()> { self.0.try_increment() }
  fn try_decrement(&mut self) -> Option<()> { self.0.try_decrement() }
  fn index_input(&self, input: Self) -> Option<usize> {
    self.0.index_input(input.0)
  }
  fn index_output(&self, inner: usize) -> Option<Self> {
    self.0.index_output(inner).map(|v| UpdateCounter(v))
  }
  fn zero() -> Self { UpdateCounter(0) }
}
