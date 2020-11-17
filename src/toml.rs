// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use std::fmt::{self, Display};
use std::iter::Peekable;

use fehler::throws;
use serde::de::{Deserializer, DeserializeSeed, MapAccess, SeqAccess, Visitor};
use thiserror::Error;

#[derive(Error,Debug)]
enum Error {
  Custom(Box<dyn Display>),
}

impl Display for Error {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    type E = Error;
    match self {
      E::Custom(x) => write!(f, "toml::TomlDe::Error::Custom:{}", x)?,
    }
  }
}

impl serde::de::Error for Error {
  fn custom<X: Display>(x: X) -> Self { Error::Custom(Box::new(x)) }
}

pub struct TomlDe<'de>(pub &'de toml::Value);

struct SA<'de> (&'de [toml::Value]);

impl<'de> SeqAccess<'de> for SA<'de> {
  type Error = Error;
  #[throws(Error)]
  fn next_element_seed<T: DeserializeSeed<'de>>
    (&mut self, seed: T) -> Option<T::Value>
  {
    let elem = (self.0).next()?;
    Some(seed.deserialize(elem)?)
  }
  fn size_hint(&self) -> Option<usize> {
    Some(self.0.len())
  }
}

struct MA<'de> (Peekable<toml::map::Iter<'de>>);

impl<'de> MapAccess<'de> for MA<'de> {
  type Error = Error;
  #[throws(Error)]
  fn next_key_seed<K: DeserializeSeed<'de>>
    (&mut self, seed: K) -> Option<K::Value>
  {
    let (k, _v) = self.0.peek()?;
    Some(seed.deserialize(k)?)
  }
  #[throws(Error)]
  fn next_value_seed<V: DeserializeSeed<'de>>
    (&mut self, seed: V) -> V::Value
  {
    let (_k, v) = self.0.next().unwrap();
    seed.deserialize(v)?
  }
}

#[throws(Error)]
fn visit<'de, V: Visitor<'de>>(v: &V, tv: &toml::Value) -> V::Value {
  type TV = toml::Value;
  match tv {
    TV::String(s) => v.visit_borrowed_str(s)?,
    TV::Array(a) => v.visit_seq(SA(a.as_slice()))?,
    TV::Table(t) => v.visit_table(MA(t.iter().peekable()))?,
  }
}

impl<'de> Deserializer<'de> for TomlDe<'de> {
  type Error = Error;
  #[throws(Error)]
  fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> V::Value {
    visit(visitor, &self.0);
  }
}
