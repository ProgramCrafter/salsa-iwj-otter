// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

use std::ops::{Add,Sub,Mul,Neg};

use num_traits::NumCast;

//---------- common types ----------

pub type Coord = i32;

#[derive(Clone,Copy,Debug,Serialize,Deserialize,Hash)]
#[derive(Eq,PartialEq,Ord,PartialOrd)]
#[serde(transparent)]
pub struct PosC<T>(pub [T; 2]);
pub type Pos = PosC<Coord>;

#[derive(Clone,Copy,Debug,Serialize,Deserialize,Hash)]
#[derive(Eq,PartialEq)]
#[serde(transparent)]
pub struct RectC<T>(pub [PosC<T>; 2]);
pub type Rect = RectC<Coord>;

// ---------- CheckedArith ----------

#[derive(Error,Clone,Copy,Debug,Serialize,Deserialize)]
#[error("error parsing Z coordinate")]
pub struct CoordinateOverflow;

pub trait CheckedArith: Copy + Clone + Debug + 'static {
  fn checked_add(self, rhs: Self) -> Result<Self, CoordinateOverflow>;
  fn checked_sub(self, rhs: Self) -> Result<Self, CoordinateOverflow>;
  fn checked_neg(self)            -> Result<Self, CoordinateOverflow>;
}
pub trait CheckedArithMul<RHS: Copy + Clone + Debug + 'static>:
                               Copy + Clone + Debug + 'static {
  fn checked_mul(self, rhs: RHS) -> Result<Self, CoordinateOverflow>;
}

macro_rules! checked_inherent { {$n:ident($($formal:tt)*) $($actual:tt)*} => {
  fn $n(self $($formal)*) -> Result<Self, CoordinateOverflow> {
    self.$n($($actual)*).ok_or(CoordinateOverflow)
  }
} }

impl CheckedArith for i32 {
  checked_inherent!{checked_add(, rhs: Self) rhs}
  checked_inherent!{checked_sub(, rhs: Self) rhs}
  checked_inherent!{checked_neg(           )    }
}
impl CheckedArithMul<i32> for i32 {
  checked_inherent!{checked_mul(, rhs: Self) rhs}
}
impl CheckedArithMul<f64> for i32 {
  fn checked_mul(self, rhs: f64) -> Result<Self, CoordinateOverflow> {
    let lhs: f64 = self.into();
    let out: f64 = lhs.checked_mul(rhs)?;
    let out: Self = NumCast::from(out).ok_or(CoordinateOverflow)?;
    Ok(out)
  }
}

macro_rules! checked_float { {$n:ident($($formal:tt)*) $($modify:tt)*} => {
  fn $n(self $($formal)*) -> Result<Self, CoordinateOverflow> {
    let out = self $($modify)*;
    if out.is_finite() { Ok(out) } else { Err(CoordinateOverflow) }
  }
} }

impl CheckedArith for f64 {
  checked_float!{checked_add(, rhs: Self)  + rhs }
  checked_float!{checked_sub(, rhs: Self)  - rhs }
  checked_float!{checked_neg()              .neg()}
}
impl CheckedArithMul<f64> for f64 {
  checked_float!{checked_mul(, rhs: Self)  * rhs }
}

//---------- Pos ----------

pub trait Mean { fn mean(&self, other: &Self) -> Self; }

impl Mean for i32 { fn mean(&self, other: &Self) -> Self {
  ((*self as i64 + *other as i64) / 2) as i32
} }

impl<T:CheckedArith> Add<PosC<T>> for PosC<T> {
  type Output = Result<Self, CoordinateOverflow>;
  #[throws(CoordinateOverflow)]
  fn add(self, rhs: PosC<T>) -> PosC<T> {
    PosC::try_from_iter_2(
      itertools::zip_eq(
        self.0.iter().cloned(),
        rhs .0.iter().cloned(),
      ).map(
        |(a,b)| a.checked_add(b)
      )
    )?
  }
}

impl<T:CheckedArith> Sub<PosC<T>> for PosC<T> {
  type Output = Result<Self, CoordinateOverflow>;
  #[throws(CoordinateOverflow)]
  fn sub(self, rhs: PosC<T>) -> PosC<T> {
    PosC::try_from_iter_2(
      itertools::zip_eq(
        self.0.iter().cloned(),
        rhs .0.iter().cloned(),
      ).map(|(a,b)| a.checked_sub(b))
    )?
  }
}

impl<S:Copy+Debug+Clone+'static,T:CheckedArithMul<S>> Mul<S> for PosC<T> {
  type Output = Result<Self, CoordinateOverflow>;
  #[throws(CoordinateOverflow)]
  fn mul(self, rhs: S) -> PosC<T> {
    PosC::try_from_iter_2(
      self.0.iter().cloned().map(
        |a| a.checked_mul(rhs)
      )
    )?
  }
}

impl<T:CheckedArith> Neg for PosC<T> {
  type Output = Result<Self, CoordinateOverflow>;
  #[throws(CoordinateOverflow)]
  fn neg(self) -> Self {
    PosC::try_from_iter_2(
      self.0.iter().cloned().map(|a| a.checked_neg())
    )?
  }
}

impl<T:Copy+Clone+Debug> PosC<T> {
  pub fn map<U:Copy+Clone+Debug, F: FnMut(T) -> U>(self, f: F) -> PosC<U> {
    PosC::from_iter(
      self.0.iter().cloned().map(f)
    ).unwrap()
  }
}

impl<T:Copy+Clone+Debug> PosC<T> {
  pub fn try_map<E:Debug, U:Copy+Clone+Debug, F: FnMut(T) -> Result<U,E>>
    (self, f: F) -> Result<PosC<U>,E>
  {
    PosC::try_from_iter_2(
      self.0.iter().cloned().map(f)
    )
  }
}

impl<T> Mean for PosC<T> where T: Mean + Debug {
  fn mean(&self, other: &Self) -> Self where T: Mean {
    PosC::try_from_iter_2(
      izip!(&self.0, &other.0)
        .map(|(a,b)| Ok::<_,Void>(a.mean(b)))
    ).unwrap_or_else(|v| match v { })
  }
}

impl PosC<Coord> {
  pub fn promote(&self) -> PosC<f64> { self.map(|v| v as f64) }
}

#[derive(Error,Debug,Copy,Clone,Serialize,Deserialize)]
pub struct PosCFromIteratorError;
display_as_debug!{PosCFromIteratorError}

impl<T> PosC<T> {
  #[throws(PosCFromIteratorError)]
  pub fn from_iter<I: Iterator<Item=T>>(i: I) -> Self { PosC(
    i
      .collect::<ArrayVec<_>>()
      .into_inner()
      .map_err(|_| PosCFromIteratorError)?
  )}
}

impl<T:Debug> PosC<T> {
  /// Panics if the iterator doesn't yield exactly 2 elements
  #[throws(E)]
  pub fn try_from_iter_2<
    E: Debug,
    I: Iterator<Item=Result<T,E>>
  >(i: I) -> Self { PosC(
    i
      .collect::<Result<ArrayVec<_>,E>>()?
      .into_inner().unwrap()
  )}
}

// ---------- Rect ----------

impl<T> RectC<T> {
  pub fn contains(&self, p: PosC<T>) -> bool where T: PartialOrd {
    (0..2).all(|i| {
      p.0[i] >= self.0[0].0[i] &&
      p.0[i] <= self.0[1].0[i]
    })
  }

  pub fn overlaps(&self, other: &RectC<T>) -> bool where T: PartialOrd {
    ! (0..2).any(|i| (
      other.0[1].0[i] < self .0[0].0[i] ||
      self .0[1].0[i] < other.0[0].0[i]
    ))
  }

  pub fn empty() -> Self where T: Copy + num_traits::Zero + num_traits::One {
    let zero = <T as num_traits::Zero>::zero();
    let one = <T as num_traits::One>::one();
    RectC([
      PosC([ one,  one  ]),
      PosC([ zero, zero ]),
    ])
  }
}

impl<T> RectC<T> where T: Mean + Debug {
  pub fn middle(&self) -> PosC<T> {
    Mean::mean(&self.0[0],
               &self.0[1])
  }
}

#[test]
fn empty_area() {
  let empty = Rect::empty();
  for x in -3..3 { for y in -3..3 {
    assert!(! empty.contains(PosC([x,y])));
  } }
}

// ---------- Region ----------

#[derive(Clone,Debug,Serialize,Deserialize)]
pub enum RegionC<T> {
  Rectangle(RectC<T>),
}
pub type Region = RegionC<Coord>;

impl<T> RegionC<T> {
  pub fn contains(&self, pos: PosC<T>) -> bool where T: PartialOrd {
    use RegionC::*;
    match &self {
      Rectangle(a) => a.contains(pos),
    }
  }

  pub fn overlaps(&self, other: &RegionC<T>) -> bool where T: PartialOrd {
    use RegionC::*;
    match (self, other) {
      (Rectangle(a), Rectangle(b)) => a.overlaps(b)
    }
  }

  pub fn empty() -> Self where T: Copy + num_traits::Zero + num_traits::One {
    RegionC::Rectangle(RectC::empty())
  }

}
