// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

#[derive(Copy,Clone,Debug)]
pub struct Global;

#[derive(Debug,Copy,Clone)]
pub struct Unauthorised<T,A> (T, PhantomData<A>);
impl<T,A> Unauthorised<T,A> {
  pub fn of(t: T) -> Self { Unauthorised(t, PhantomData) }
  pub fn by(self, _auth: Authorisation<A>) -> T { self.0 }
  pub fn by_ref(&self,     _auth: Authorisation<A>) -> &    T { &    self.0 }
  pub fn by_mut(&mut self, _auth: Authorisation<A>) -> &mut T { &mut self.0 }
}
impl<T,A> From<T> for Unauthorised<T,A> {
  fn from(t: T) -> Self { Self::of(t) }
}

#[derive(Error,Debug)]
#[error("internal AuthorisationError {0}")]
pub struct AuthorisationError(pub String);

#[derive(Debug)]
pub struct Authorisation<A> (PhantomData<*const A>);
impl<A> Clone for Authorisation<A> { fn clone(&self) -> Self { *self } }
impl<A> Copy for Authorisation<A> { }

pub type AuthorisationSuperuser = Authorisation<Global>;

impl<T> Authorisation<T> {
  pub const fn authorised(_v: &T) -> Authorisation<T> {
    Authorisation(PhantomData)
  }
  pub fn map<U>(self, _f: fn(&T) -> &U) -> Authorisation<U> {
    self.therefore_ok()
  }
  pub fn therefore_ok<U>(self) -> Authorisation<U> {
    Authorisation(PhantomData)
  }
  pub const fn authorise_any() -> Authorisation<T> {
    Authorisation(PhantomData)
  }
}

impl<T:Serialize> From<Authorisation<Global>> for Authorisation<T> {
  // ^ we need a bound not met by Global or we conflict with From<T> for T
  fn from(global: Authorisation<Global>) -> Self {
    global.therefore_ok()
  }
}

impl From<anyhow::Error> for AuthorisationError {
  fn from(a: anyhow::Error) -> AuthorisationError {
    AuthorisationError(format!("{}", a))
  }
}

pub trait AuthorisationCombine: Sized {
  type Output;
  fn combine(self) -> Authorisation<Self::Output> {
    Authorisation(PhantomData)
  }
}
impl<A,B> AuthorisationCombine
  for (Authorisation<A>, Authorisation<B>) {
  type Output = (A, B);
}
impl<A,B,C> AuthorisationCombine
  for (Authorisation<A>, Authorisation<B>, Authorisation<C>) {
  type Output = (A, B, C);
}
