// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use std::fmt::{self, Debug, Display};
use std::iter::Peekable;
use std::slice;

use fehler::throws;
use serde::forward_to_deserialize_any;
use serde::de::{Deserializer, DeserializeSeed, IntoDeserializer};
use serde::de::{MapAccess, SeqAccess, Visitor};
use thiserror::Error;

#[derive(Error,Debug)]
pub enum Error {
  Custom(Box<str>),
}

impl Display for Error {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    type E = Error;
    match self {
      E::Custom(x) => write!(f, "toml::TomlDe::Error::Custom:{}", &x)?,
    }
  }
}

impl serde::de::Error for Error {
  fn custom<X: Display>(x: X) -> Self {
    Error::Custom(x.to_string().into_boxed_str())
  }
}

pub struct TomlDe<'de>(pub &'de toml::Value);

struct SA<'de> (slice::Iter<'de, toml::Value>);

impl<'de> SeqAccess<'de> for SA<'de> {
  type Error = Error;
  #[throws(Error)]
  fn next_element_seed<T: DeserializeSeed<'de>>
    (&mut self, seed: T) -> Option<T::Value>
  {
    if let Some(elem) = (self.0).next() {
      Some(seed.deserialize(TomlDe(elem))?)
    } else {
      None
    }
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
    if let Some((k, _v)) = self.0.peek() {
      Some(seed.deserialize(k.as_str().into_deserializer())?)
    } else {
      None
    }
  }
  #[throws(Error)]
  fn next_value_seed<V: DeserializeSeed<'de>>
    (&mut self, seed: V) -> V::Value
  {
    let (_k, v) = self.0.next().unwrap();
    seed.deserialize(TomlDe(v))?
  }
}

#[throws(Error)]
fn visit<'de, V: Visitor<'de>>(v: V, tv: &'de toml::Value) -> V::Value {
  type TV = toml::Value;
  match tv {
    TV::String(s) => v.visit_borrowed_str(s)?,
    TV::Array(a) => v.visit_seq(SA(a.as_slice().iter()))?,
    TV::Table(t) => v.visit_map(MA(t.iter().peekable()))?,
  }
}

impl<'de> Deserializer<'de> for TomlDe<'de> {
  type Error = Error;
  #[throws(Error)]
  fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> V::Value {
    visit(visitor, &self.0)?
  }
  forward_to_deserialize_any! {
    bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
    bytes byte_buf option unit unit_struct newtype_struct seq tuple
    tuple_struct map struct enum identifier ignored_any
  }
}
