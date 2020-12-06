// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use std::fmt::{Debug, Display};
use std::iter::Peekable;
use std::slice;

use fehler::throws;
use if_chain::if_chain;
use thiserror::Error;

use serde::forward_to_deserialize_any;
use serde::de::{
  Deserialize, DeserializeOwned, Deserializer,
  DeserializeSeed, EnumAccess,
  IntoDeserializer, MapAccess,
  SeqAccess, VariantAccess, Visitor
};

#[derive(Error,Debug)]
pub enum Error {
  #[error("deserialize failed (improper TOML structure?): {0}")]
  Custom(Box<str>),
  #[error("config file has invalid TOML syntax: {0}")]
  TomlSyntax(toml::de::Error),
}

impl serde::de::Error for Error {
  fn custom<X: Display>(x: X) -> Self {
    Error::Custom(x.to_string().into_boxed_str())
  }
}

fn str_deserialize<'de, S: DeserializeSeed<'de>>
  (seed: S, k: &'de str) -> Result<S::Value, Error>
{
  seed.deserialize(
    k.into_deserializer()
  )
}

pub struct TomlDe<'de>(pub &'de toml::Value);

struct SA<'de>(slice::Iter<'de, toml::Value>);

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

struct MA<'de>(Peekable<toml::map::Iter<'de>>);

impl<'de> MapAccess<'de> for MA<'de> {
  type Error = Error;
  fn next_key_seed<K: DeserializeSeed<'de>>
    (&mut self, seed: K) -> Result<Option<K::Value>, Error>
  {
    Ok(if let Some((k, _v)) = self.0.peek() {
      Some(str_deserialize(seed, k)?)
    } else {
      None
    })
  }
  #[throws(Error)]
  fn next_value_seed<V: DeserializeSeed<'de>>
    (&mut self, seed: V) -> V::Value
  {
    let (_k, v) = self.0.next().unwrap();
    seed.deserialize(TomlDe(v))?
  }
}

struct EA<'de> { k: &'de str, v: &'de toml::Value }

impl<'de> EnumAccess<'de> for EA<'de> {
  type Error = Error;
  type Variant = TomlDe<'de>;
  #[throws(Error)]
  fn variant_seed<V: DeserializeSeed<'de>>
    (self, seed: V) -> (V::Value, TomlDe<'de>)
  {
    (str_deserialize(seed, self.k)?,
     TomlDe(self.v))
  }
}

impl<'de> VariantAccess<'de> for TomlDe<'de> {
  type Error = Error;
  #[throws(Error)]
  fn unit_variant(self) { }

  #[throws(Error)]
  fn newtype_variant_seed<S: DeserializeSeed<'de>>
    (self, seed: S) -> S::Value
  {
    seed.deserialize(self)?
  }

  #[throws(Error)]
  fn tuple_variant<V: Visitor<'de>>(self, _: usize, v: V) -> V::Value {
    visit(v, &self.0)?
  }

  #[throws(Error)]
  fn struct_variant<V: Visitor<'de>>(self, _:&[&str], v: V) -> V::Value {
    visit(v, &self.0)?
  }
}

#[throws(Error)]
fn visit<'de, V: Visitor<'de>>(v: V, tv: &'de toml::Value) -> V::Value {
  type TV = toml::Value;
  match tv {
    TV::String(s) => v.visit_borrowed_str::<Error>(s)?,
    &TV::Integer(i) => v.visit_i64::<Error>(i)?,
    &TV::Float(f) => v.visit_f64::<Error>(f)?,
    &TV::Boolean(b) => v.visit_bool::<Error>(b)?,
    TV::Datetime(dt) => v.visit_str::<Error>(&dt.to_string())?,
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
  #[throws(Error)]
  fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> V::Value {
    // Option only works in structs, where it's represented by an
    // absence of the key.  When we are called, we are either in a
    // non-working context, or a context where we've already had
    // the relevant struct key.
    visitor.visit_some(self)?
  }
  #[throws(Error)]
  fn deserialize_enum<V: Visitor<'de>>
    (self, _:&str, _:&[&str], vi: V) -> V::Value
  {
    type TV = toml::Value;
    match &self.0 {
      TV::String(s) => return vi.visit_enum(s.as_str().into_deserializer())?,
      TV::Table(s) => if_chain! {
        let mut s = s.iter();
        if let Some((k, v)) = s.next();
        if let None = s.next();
        then { return vi.visit_enum(EA { k, v }) }
      },
      _ => {}
    }
    // hopefully the format will figure it out, or produce an error
    visit(vi, &self.0)?
  }
  forward_to_deserialize_any! {
    bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
    bytes byte_buf unit unit_struct newtype_struct seq tuple
    tuple_struct map struct identifier ignored_any
  }
}

#[throws(Error)]
pub fn from_value<'de, T: Deserialize<'de>>(tv: &'de toml::Value) -> T {
  Deserialize::deserialize(TomlDe(tv))?
}

#[throws(Error)]
pub fn from_str<T: DeserializeOwned>(s: &str) -> T {
  let tv: toml::Value = s.parse().map_err(Error::TomlSyntax)?;
//  dbg!(&tv);
  from_value(&tv)?
}
