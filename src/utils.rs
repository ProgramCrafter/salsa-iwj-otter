// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use std::fmt::{self, Debug};
use std::fs;
use std::io;
use std::ops::{Deref, Index};
use std::os::unix::io::IntoRawFd;

use arrayvec::ArrayVec;
use derive_more::*;
use fehler::{throw, throws};
use libc;

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

pub trait OrdExt: Ord + Sized + Clone {
  fn update_max(&mut self, new: &Self) {
    if *new > *self { *self = new.clone() }
  }
}
impl<T> OrdExt for T where T: Ord + Sized + Clone { }

pub trait SplitAtDelim<Delim> {
  fn split_at_delim(&self, delim: Delim) -> (&Self, &Self);
}

impl SplitAtDelim<char> for str {
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
  pub fn new(&self) -> &T { &self.0[0] }

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
