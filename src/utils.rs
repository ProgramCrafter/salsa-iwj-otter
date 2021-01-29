// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use std::fmt::{self, Debug};
use std::fs;
use std::io;
use std::ops::Deref;
use std::os::unix::io::IntoRawFd;

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
