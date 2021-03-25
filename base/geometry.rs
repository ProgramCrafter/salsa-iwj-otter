// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

use std::ops::{Add,Sub,Mul,Neg};

use num_traits::NumCast;

//---------- common types ----------

pub type Coord = i32;

#[derive(Clone,Copy,Serialize,Deserialize,Hash)]
#[derive(Eq,PartialEq,Ord,PartialOrd)]
#[serde(transparent)]
pub struct PosC<T>{ pub coords: [T; 2] }
pub type Pos = PosC<Coord>;

#[derive(Clone,Copy,Serialize,Deserialize,Hash)]
#[derive(Eq,PartialEq,Ord,PartialOrd)]
#[serde(transparent)]
pub struct RectC<T>{ pub corners: [PosC<T>; 2] }
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

pub trait Mean { fn mean(&self, other: &Self) -> Self; }

impl Mean for i32 { fn mean(&self, other: &Self) -> Self {
  ((*self as i64 + *other as i64) / 2) as i32
} }

//---------- Pos ----------

impl PosC<Coord> {
  pub fn promote(&self) -> PosC<f64> { self.map(|v| v as f64) }
}

#[derive(Error,Debug,Copy,Clone,Serialize,Deserialize)]
pub struct PosCFromIteratorError;
display_as_debug!{PosCFromIteratorError}

impl<T> PosC<T> {
  pub const fn new(x: T, y: T) -> Self { PosC{ coords: [x,y] } }
  pub fn both(v: T) -> Self where T: Copy { PosC::new(v,v) }
  pub fn zero() -> Self where T: num_traits::Zero + Copy {
    PosC::both(<T as num_traits::Zero>::zero())
  }
}
impl<T> PosC<T> where T: Copy {
  pub fn x(self) -> T { self.coords[0] }
  pub fn y(self) -> T { self.coords[1] }
}

impl<T> PosC<T> {
  #[throws(PosCFromIteratorError)]
  pub fn from_iter<I: Iterator<Item=T>>(i: I) -> Self { PosC{ coords:
    i
      .collect::<ArrayVec<_>>()
      .into_inner()
      .map_err(|_| PosCFromIteratorError)?
  }}
}

impl<T> PosC<T> where T: Debug {
  pub fn from_iter_2<I: Iterator<Item=T>>(i: I) -> Self { PosC{ coords:
    i
      .collect::<ArrayVec<_>>()
      .into_inner()
      .unwrap()
  }}
}

impl<T> Debug for PosC<T> where T: Debug + Copy {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    write!(f, "[{:?},{:?}]", self.x(), self.y())?;
  }
}

impl<T:Debug> PosC<T> {
  /// Panics if the iterator doesn't yield exactly 2 elements
  #[throws(E)]
  pub fn try_from_iter_2<
    E: Debug,
    I: Iterator<Item=Result<T,E>>
  >(i: I) -> Self { PosC{ coords:
    i
      .collect::<Result<ArrayVec<_>,E>>()?
      .into_inner().unwrap()
  }}
}

impl<T:CheckedArith> Add<PosC<T>> for PosC<T> {
  type Output = Result<Self, CoordinateOverflow>;
  #[throws(CoordinateOverflow)]
  fn add(self, rhs: PosC<T>) -> PosC<T> {
    PosC::try_from_iter_2(
      itertools::zip_eq(
        self.coords.iter().cloned(),
        rhs .coords.iter().cloned(),
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
        self.coords.iter().cloned(),
        rhs .coords.iter().cloned(),
      ).map(|(a,b)| a.checked_sub(b))
    )?
  }
}

impl<S:Copy+Debug+Clone+'static,T:CheckedArithMul<S>> Mul<S> for PosC<T> {
  type Output = Result<Self, CoordinateOverflow>;
  #[throws(CoordinateOverflow)]
  fn mul(self, rhs: S) -> PosC<T> {
    PosC::try_from_iter_2(
      self.coords.iter().cloned().map(
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
      self.coords.iter().cloned().map(|a| a.checked_neg())
    )?
  }
}

impl<T:Copy+Clone+Debug> PosC<T> {
  pub fn map<U:Copy+Clone+Debug, F: FnMut(T) -> U>(self, f: F) -> PosC<U> {
    PosC::from_iter(
      self.coords.iter().cloned().map(f)
    ).unwrap()
  }
}

impl<T:Copy+Clone+Debug> PosC<T> {
  pub fn try_map<E:Debug, U:Copy+Clone+Debug, F: FnMut(T) -> Result<U,E>>
    (self, f: F) -> Result<PosC<U>,E>
  {
    PosC::try_from_iter_2(
      self.coords.iter().cloned().map(f)
    )
  }
}

impl<T> Mean for PosC<T> where T: Mean + Debug {
  fn mean(&self, other: &Self) -> Self where T: Mean {
    PosC::from_iter_2(
      izip!(&self.coords, &other.coords)
        .map(|(a,b)| a.mean(b))
    )
  }
}

// ---------- Rect ----------

impl<T> RectC<T> where T: Copy {
  pub fn tl(&self) -> PosC<T> { self.corners[0] }
  pub fn br(&self) -> PosC<T> { self.corners[1] }
}

impl<T> Debug for RectC<T> where T: Debug + Copy {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    write!(f, "Rect[{:?},{:?}]", self.tl(), self.br())?;
  }
}

impl<T> RectC<T> {
  pub fn contains(&self, p: PosC<T>) -> bool where T: PartialOrd + Copy {
    (0..2).all(|i| {
      p.coords[i] >= self.tl().coords[i] &&
      p.coords[i] <= self.br().coords[i]
    })
  }

  pub fn overlaps(&self, other: &RectC<T>) -> bool where T: PartialOrd + Copy {
    ! (0..2).any(|i| (
      other.br().coords[i] < self .tl().coords[i] ||
      self .br().coords[i] < other.tl().coords[i]
    ))
  }

  pub fn empty() -> Self where T: num_traits::Zero + num_traits::One + Copy {
    RectC{ corners: [
      PosC::both( <T as num_traits::One >::one()  ),
      PosC::both( <T as num_traits::Zero>::zero() ),
    ]}
  }
}

impl<T> RectC<T> where T: Mean + Debug + Copy {
  pub fn middle(&self) -> PosC<T> {
    Mean::mean(&self.tl(),
               &self.br())
  }
}

#[test]
fn empty_area() {
  let empty = Rect::empty();
  for x in -3..3 { for y in -3..3 {
    dbg!(empty,x,y);
    assert!(! empty.contains(PosC::new(x,y)));
  } }
}

// ---------- Region ----------

#[derive(Clone,Debug,Serialize,Deserialize)]
#[derive(Ord,PartialOrd,Eq,PartialEq)]
pub enum RegionC<T:Copy> {
  Rect(RectC<T>),
}
pub type Region = RegionC<Coord>;

impl<T:Copy> RegionC<T> {
  pub fn contains(&self, pos: PosC<T>) -> bool where T: PartialOrd {
    use RegionC::*;
    match &self {
      Rect(a) => a.contains(pos),
    }
  }

  pub fn overlaps(&self, other: &RegionC<T>) -> bool where T: PartialOrd {
    use RegionC::*;
    match (self, other) {
      (Rect(a), Rect(b)) => a.overlaps(b)
    }
  }

  pub fn empty() -> Self where T: Copy + num_traits::Zero + num_traits::One {
    RegionC::Rect(RectC::empty())
  }

}
