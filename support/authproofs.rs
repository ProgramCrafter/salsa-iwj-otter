// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

#[derive(Copy,Clone,Debug)]
pub struct Global;

#[derive(Debug,Copy,Clone)]
pub struct Unauthorised<T,A> (T, PhantomData<A>);
impl<T,A> Unauthorised<T,A> {
  #[inline] pub fn of(t: T) -> Self { Unauthorised(t, PhantomData) }
  #[inline] pub fn by(self, _auth: Authorisation<A>) -> T { self.0 }
  #[inline]
  pub fn by_ref(&self,     _auth: Authorisation<A>) -> &    T { &    self.0 }
  #[inline]
  pub fn by_mut(&mut self, _auth: Authorisation<A>) -> &mut T { &mut self.0 }
}
impl<T,A> From<T> for Unauthorised<T,A> {
  #[inline] fn from(t: T) -> Self { Self::of(t) }
}

#[derive(Error,Debug)]
#[error("internal AuthorisationError {0}")]
pub struct AuthorisationError(pub String);

#[derive(Debug)]
pub struct Authorisation<A> (PhantomData<*const A>);
impl<A> Clone for Authorisation<A> { #[inline] fn clone(&self)->Self{ *self }}
impl<A> Copy for Authorisation<A> { }

pub type AuthorisationSuperuser = Authorisation<Global>;

impl<T> Authorisation<T> {
  /// Proof obligation: access to this `T` has been authorised.
  #[inline]
  pub const fn promise_for(_v: &T) -> Authorisation<T> {
    Authorisation(PhantomData)
  }
  #[inline]
  pub fn map<U,F>(self, _f: F) -> Authorisation<U> where F: Fn(&T) -> &U {
    self.so_promise()
  }
  /// Minor proof obligation: in this case, authorised access to `T`
  /// implies authorised access to `U`.
  #[inline]
  pub fn so_promise<U>(self) -> Authorisation<U> {
    Authorisation(PhantomData)
  }
  /// Proof obligation: access to `T` has been authorised.
  #[inline]
  pub const fn promise_any() -> Authorisation<T> {
    Authorisation(PhantomData)
  }
}

impl<T:Serialize> From<Authorisation<Global>> for Authorisation<T> {
  // ^ we need a bound not met by Global or we conflict with From<T> for T
  #[inline]
  fn from(global: Authorisation<Global>) -> Self {
    global.so_promise()
  }
}

impl From<anyhow::Error> for AuthorisationError {
  fn from(a: anyhow::Error) -> AuthorisationError {
    AuthorisationError(format!("{}", a))
  }
}

pub trait AuthorisationCombine: Sized {
  type Output;
  #[inline]
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
