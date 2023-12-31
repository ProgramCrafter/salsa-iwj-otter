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

#[allow(clippy::only_used_in_recursion)] // FP nightly (1bfe40d11 2022-03-18
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
impl Mean for f64 { fn mean(&self, other: &Self) -> Self {
  self * 0.5 + other * 0.5
} }

//---------- Pos ----------

pub trait PosPromote {
  fn promote(&self) -> PosC<f64>;
}
impl<T> PosPromote for PosC<T> where T: Into<f64> + Copy + Debug {
  fn promote(&self) -> PosC<f64> { self.map(|v| v.into()) }
}

#[derive(Error,Debug,Copy,Clone,Serialize,Deserialize)]
pub struct PosCFromIteratorError;
display_as_debug!{PosCFromIteratorError}

#[macro_export]
macro_rules! pos_zip_try_map { {
  $( $input:expr ),* => $closure:expr
} => {
  PosC::try_from_iter_2(
    izip!($( $input .coords(), )*)
      .map($closure)
  )
} }
#[macro_export]
macro_rules! pos_zip_map { {
  $( $input:expr ),* => $closure:expr
} => {
  PosC::from_iter_2(
    izip!($( $input .coords(), )*)
      .map($closure)
  )
} }

impl<T> PosC<T> {
  pub const fn new(x: T, y: T) -> Self { PosC{ coords: [x,y] } }
  pub fn both(v: T) -> Self where T: Copy { PosC::new(v,v) }
  pub fn zero() -> Self where T: num_traits::Zero + Copy {
    PosC::both(<T as num_traits::Zero>::zero())
  }

  pub fn coords(self) -> impl ExactSizeIterator<Item=T> + FusedIterator {
    self.coords.into_iter()
  }

  #[throws(CoordinateOverflow)]
  pub fn len2(self) -> f64 where PosC<T>: PosPromote {
    self.promote().coords()
      .try_fold(0., |b, c| {
        let c2 = c.checked_mul(c)?;
        b.checked_add(c2)
      })?
  }

  #[throws(CoordinateOverflow)]
  pub fn len(self) -> f64 where PosC<T>: PosPromote {
    let d2 = self.len2()?;
    let d = d2.sqrt();
    if !d.is_finite() { throw!(CoordinateOverflow) }
    d
  }
}
impl<T> PosC<T> where T: Copy {
  pub fn x(self) -> T { self.coords[0] }
  pub fn y(self) -> T { self.coords[1] }
}

#[allow(clippy::should_implement_trait)] // this one is fallible, which is a bit odd
impl<T> PosC<T> {
  #[throws(PosCFromIteratorError)]
  pub fn from_iter<I: Iterator<Item=T>>(i: I) -> Self { PosC{ coords:
    i
      .collect::<ArrayVec<_,2>>()
      .into_inner()
      .map_err(|_| PosCFromIteratorError)?
  }}
}

impl<T> PosC<T> where T: Debug {
  pub fn from_iter_2<I: Iterator<Item=T>>(i: I) -> Self { PosC{ coords:
    i
      .collect::<ArrayVec<_,2>>()
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
      .collect::<Result<ArrayVec<_,2>,E>>()?
      .into_inner().unwrap()
  }}
}

impl<T:CheckedArith> Add<PosC<T>> for PosC<T> {
  type Output = Result<Self, CoordinateOverflow>;
  #[throws(CoordinateOverflow)]
  fn add(self, rhs: PosC<T>) -> PosC<T> {
    pos_zip_try_map!( self, rhs => |(a,b)| a.checked_add(b) )?
  }
}

impl<T:CheckedArith> Sub<PosC<T>> for PosC<T> {
  type Output = Result<Self, CoordinateOverflow>;
  #[throws(CoordinateOverflow)]
  fn sub(self, rhs: PosC<T>) -> PosC<T> {
    pos_zip_try_map!( self, rhs => |(a,b)| a.checked_sub(b) )?
  }
}

impl<S:Copy+Debug+Clone+'static,T:CheckedArithMul<S>> Mul<S> for PosC<T> {
  type Output = Result<Self, CoordinateOverflow>;
  #[throws(CoordinateOverflow)]
  fn mul(self, rhs: S) -> PosC<T> {
    pos_zip_try_map!( self => |a| a.checked_mul(rhs) )?
  }
}

impl<T:CheckedArith> Neg for PosC<T> {
  type Output = Result<Self, CoordinateOverflow>;
  #[throws(CoordinateOverflow)]
  fn neg(self) -> Self {
    pos_zip_try_map!( self => |a| a.checked_neg() )?
  }
}

impl<T:Copy+Clone+Debug> PosC<T> {
  pub fn map<U:Copy+Clone+Debug, F: FnMut(T) -> U>(self, f: F) -> PosC<U> {
    pos_zip_map!( self => f )
  }
}

impl<T:Copy+Clone+Debug> PosC<T> {
  pub fn try_map<E:Debug, U:Copy+Clone+Debug, F: FnMut(T) -> Result<U,E>>
    (self, f: F) -> Result<PosC<U>,E>
  {
    pos_zip_try_map!( self => f )
  }
}

impl<T> Mean for PosC<T> where T: Mean + Debug + Copy {
  fn mean(&self, other: &Self) -> Self where T: Mean {
    pos_zip_map!( self, other => |(a,b)| a.mean(&b) )
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

impl<T> RectC<T> where T: CheckedArith + Debug + Copy {
  #[throws(CoordinateOverflow)]
  pub fn size(&self) -> PosC<T> {
    (self.br() - self.tl())?
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
