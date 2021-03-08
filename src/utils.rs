// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;
use crate::prelude::*;

use std::ops::Neg;

#[macro_export]
macro_rules! ensure_eq {
  ($v1:expr, $v2:expr) => {
    ({
      let v1 = &$v1;
      let v2 = &$v2;
      if v1 != v2 {
        Err(anyhow!("ensure_eq failed: {} != {}: {:?} != {:?}",
                    stringify!($v1), stringify!($v2),
                    v1, v2))
      } else {
        Ok(())
      }
    }?)
  }
}

#[ext(pub, name=OrdExt)]
impl<T: Ord + Sized + Clone> T {
  fn update_max(&mut self, new: &Self) {
    if *new > *self { *self = new.clone() }
  }
}

#[ext(pub, name=SplitAtDelim)]
impl str {
  fn split_at_delim(&self, delim: char) -> (&Self, &Self) {
    match self.find(delim) {
      Some(index) => self.split_at(index),
      None => (self, ""),
    }
  }
}

#[derive(Copy,Clone,Debug,From,Into)]
#[derive(Hash,Eq,PartialEq)]
pub struct OldNew<T>([T; 2]);

#[derive(Copy,Clone,Debug)]
#[derive(Hash,Eq,PartialEq)]
pub enum OldNewIndex { Old, New }

impl<T> OldNew<T> {
  pub fn old(&self) -> &T { &self.0[0] }
  pub fn new(&self) -> &T { &self.0[1] }

  pub fn map<U, F: FnMut(&T) -> U>(&self, f: F) -> OldNew<U> {
    OldNew(
      self.iter().map(f)
        .collect::<ArrayVec<[U; 2]>>()
        .into_inner()
        .unwrap_or_else(|_|panic!())
    )
  }

  pub fn as_refs(&self) -> OldNew<&T> {
    OldNew(
      self.iter()
        .collect::<ArrayVec<[&T; 2]>>()
        .into_inner()
        .unwrap_or_else(|_|panic!())
    )
  }

  pub fn iter(&self) -> impl Iterator<Item=&T> {
    self.0.iter()
  }

  pub fn into_iter(self) -> impl Iterator<Item=T> {
    std::array::IntoIter::new(self.0)
  }
}

impl<T> Index<OldNewIndex> for OldNew<T> {
  type Output = T;
  fn index(&self, i: OldNewIndex) -> &T { &self.0[i as usize] }
}

/*
put trait OptionExt {
  type Output;
  fn get_or_try_insert_with<
      E: Error,
      F: FnOnce() -> Result<Output,E>,
    >(&mut self, f: F) -> Result<&mut Output, E>;
}

impl<T> OptionExt for Option<T> {
  type Output = T;
  fn get_or_try_insert_with<E,F>
    (&mut self, f: F) -> Result<&mut Output, E>
    where E: Error, F: FnOnce() -> Result<Output,E>,
  {
    if self.is_none() {
      *self = Some(f()?);
    }
    Ok(self.as_mut().unwrap())
  }
}
*/

// https://github.com/rust-lang/rust/issues/32255 :-(

pub trait LocalFileExt {
  fn close(self) -> Result<(), io::Error>;
}

impl LocalFileExt for fs::File {
  #[throws(io::Error)]
  fn close(self) {
    let r = unsafe {
      let fd = self.into_raw_fd();
      libc::close(fd)
    };
    if r == 0 {
      ()
    } else if r == -1 {
      throw!(io::Error::last_os_error())
    } else {
      panic!("close(2) returned {}", r)
    }
  }
}


// todo #[derive(Clone)]
pub struct Thunk<U: Sync, F: Sync + FnOnce() -> U> (
  lazy_init::LazyTransform<F, U>
);

impl<T: Sync, F: Sync + FnOnce() -> T> Debug for Thunk<T, F> where T: Debug{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self.0.get() {
      Some(inner) => write!(f, "Thunk({:?})", inner),
      None        => write!(f, "Thunk(...)"),
    }
  }
}

impl<T: Sync, F: Sync + FnOnce() -> T> Thunk<T, F> {
  pub fn new(f: F) -> Self {
    Thunk(
      lazy_init::LazyTransform::new(f)
    )
  }
  pub fn force_eval(thunk: &Self) {
    thunk.0.get_or_create(|f| f());
  }
  pub fn into_inner(thunk: Self) -> T {
    Thunk::force_eval(&thunk);
    thunk.0.into_inner().unwrap_or_else(|_|panic!())
  }
}

impl<T: Sync, F: Sync + FnOnce() -> T> Deref for Thunk<T, F> {
  type Target = T;
  fn deref(&self) -> &T {
    Thunk::force_eval(self);
    self.0.get().unwrap()
  }
}

impl<Y: Sync, E: Sync, F: Sync + FnOnce() -> Result<Y,E>>
  From<Thunk<Result<Y,E>, F>> for Result<Y,E>
{
  fn from(thunk: Thunk<Result<Y,E>, F>) -> Result<Y,E> {
    Thunk::into_inner(thunk)
  }
}

// todo: DerefMut

#[derive(Error,Clone,Copy,Debug,Eq,PartialEq,Serialize,Deserialize)]
#[error("error parsing Z coordinate")]
pub struct FooParseError;

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
    let out: Self = num::NumCast::from(out).ok_or(CoordinateOverflow)?;
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

pub fn toml_merge<'u,
                  S: 'u + AsRef<str>,
                  KV: IntoIterator<Item=(&'u S, &'u toml::Value)>
                  >(
  table: &mut toml::value::Table,
  updates: KV,
) {
  use toml::value::{Table, Value};
  type TME<'e> = toml::map::Entry<'e>;

  let mut kv = updates.into_iter().map(|(k, v)| (k.as_ref(), v));
  inner(table, &mut kv);

  fn inner<'u>(
    table: &mut Table,
    updates: &'u mut dyn Iterator<Item=(&'u str, &'u Value)>
  ) {
    for (k, v) in updates {
      let e = table.entry(k);
      match e {
        TME::Vacant(ve) => {
          ve.insert(v.clone());
        }
        TME::Occupied(mut oe) => match (oe.get_mut(), v) {
          (Value::Table(old), Value::Table(new)) => {
            toml_merge(old, new);
          }
          (Value::Array(old), Value::Array(new)) => {
            old.extend(new.iter().cloned());
          }
          (old, new) => {
            *old = new.clone();
          }
        }
      }
    }
  }
}

#[macro_export]
macro_rules! deref_to_field {
  {$({ $($gen:tt)* })? $outer:ty, $inner:ty, $($field:tt)*} => {
    impl $(< $($gen)* >)? Deref for $outer {
      type Target = $inner;
      fn deref(&self) -> &$inner { &self.$($field)* }
    }
  }
}
#[macro_export]
macro_rules! deref_to_field_mut {
  {$({ $($gen:tt)* })? $outer:ty, $inner:ty, $($field:tt)*} => {
    deref_to_field!{ $({ $($gen)* })? $outer, $inner, $($field)*}
    impl $(< $($gen)* >)? DerefMut for $outer {
      fn deref_mut(&mut self) -> &mut $inner { &mut self.$($field)* }
    }
  }
}

#[derive(Debug)]
pub enum Loop<E> {
  Continue,
  Break,
  Error(E),
}
impl<E> From<E> for Loop<E> {
  fn from(e: E) -> Loop<E> { Loop::Error(e) }
}
impl Loop<Infallible> {
  pub fn ok<T>(t: T) -> Result<T,Loop<Infallible>> { Ok(t) }
}

pub trait IteratorExt<U,E,F>: Iterator
  where F: FnMut(Self::Item) -> Result<U,Loop<E>>,
{
  type Return: Iterator<Item=U>;
  fn map_loop(self, f: F) -> Self::Return where E: EmptyType;

  type TryReturn: Iterator<Item=Result<U,E>>;
  fn try_map_loop(self, f: F) -> Self::TryReturn;
}

pub trait EmptyType { fn diverge<T>(self) -> T; }

impl EmptyType for Infallible {
  fn diverge<T>(self) -> T { match self { } }
}

impl<T,U,E,F> IteratorExt<U,E,F> for T where
  T: Iterator,
  F: FnMut(Self::Item) -> Result<U,Loop<E>>,
{
  type Return = impl Iterator<Item=U>;
  fn map_loop(self, f: F) -> Self::Return where E: EmptyType {
    self
      .map(f)
      .filter(|i| !matches!(i, Err(Loop::Continue)))
      .take_while(|i| !matches!(i, Err(Loop::Break)))
      .map(|i| i.ok().unwrap())
  }

  type TryReturn = impl Iterator<Item=Result<U,E>>;
  fn try_map_loop(self, f: F) -> Self::TryReturn {
    self
      .map(f)
      .filter(|i| matches!(i, Err(Loop::Continue)))
      .take_while(|i| matches!(i, Err(Loop::Break)))
      .map(|i| match i {
        Ok(y) => Ok(y),
        Err(Loop::Error(e)) => Err(e),
        _ => panic!(),
      })
  }
}

#[macro_export]
macro_rules! matches_doesnot {
  ($v:expr, = $y:pat, ! $n:pat $(,)?) =>
  { match $v { $y => true, $n => false } }
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
}

#[macro_export]
macro_rules! dbgc {
    // NOTE: We cannot use `concat!` to make a static string as a format argument
    // of `eprintln!` because `file!` could contain a `{` or
    // `$val` expression could be a block (`{ .. }`), in which case the `eprintln!`
    // will be malformed.
    () => {
        std::eprintln!("[{}:{}]", std::file!(), std::line!());
    };
    ($val:expr $(,)?) => {
        // Use of `match` here is intentional because it affects the lifetimes
        // of temporaries - https://stackoverflow.com/a/48732525/1063961
        match $val {
            tmp => {
                std::eprintln!("[{}:{}] {} = {:?}",
                    std::file!(), std::line!(), std::stringify!($val), &tmp);
                tmp
            }
        }
    };
    ($($val:expr),+ $(,)?) => {
        ($($crate::dbgc!($val)),+,)
    };
}
