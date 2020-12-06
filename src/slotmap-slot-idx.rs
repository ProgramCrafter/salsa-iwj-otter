// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: MIT-0 OR AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(clippy::or_fun_call)]

//! Provides a [`get_idx_key`](trait.KeyDataExt.html#tymethod.get_idx_version) method on
//! `slotmap::KeyData`.  See [KeyDataExt::get_idx_version].

/// Extension trait for `slotmap::KeyData`, providing `get_idx_version`.
///
/// No-one is expected to implement this trait for anything else.
pub trait KeyDataExt {
  /// Returns the slot index and version.  This is useful only in
  /// unusual situations.
  ///
  /// At any one time, a slotmap has at most one entry with each
  /// index.  The combination of index and version are unique across
  /// time.  Indices are generally not significantly bigger than thw
  /// maximum ever occupancy.  No other guarantees are made.
  ///
  /// For serialisation, use `serde` or `as_ffi`.
  ///
  /// ### panics: ###
  ///
  /// This function panics if the `slotmap::KeyData` `serde::ser::Serialize`
  /// representation has changed too much.  This ought to be caught
  /// by the tests, and would probably be a breaking change in the
  /// underlying `slotmap` crate in any case.
  ///
  /// If you prefer to receive an error rather than panicing,
  /// see [keydata_extract].
  fn get_idx_version(self) -> (u32, u32);
}

impl KeyDataExt for slotmap::KeyData {
  fn get_idx_version(self) -> (u32, u32) {
    keydata_extract(self).expect(
      "slotmap KeyData Serialize representation changed!"
    )
  }
}

/// Underlying extraction function.  Fails rather than panicing.
///
/// Fails if the `slotmap::KeyData` `serde::ser::Serialize`
/// representation has changed too much.  Should not be able to fail
/// otherwise.
pub fn keydata_extract(key: slotmap::KeyData) -> Result<(u32, u32), Error> {
  let mut m: MainExtractor = std::default::Default::default();
  key.serialize(&mut m)?;
  Ok(( m.idx    .ok_or(error(line!()))?,
       m.version.ok_or(error(line!()))? ))
}

#[derive(Debug)]
/// Problem with the `slotmap::KeyData` `Serialize` implementation.
///
/// Not really helpful.  `Unexpected` gives the source line number
/// in `slotmap-slot-idx.rs`.  `WasCustomSerialize` threw the
/// actual error away.
pub enum Error {
  WasCustomSerialize,
  Unexpected(std::num::NonZeroU32),
}
impl std::error::Error for Error { }

//---------- implementation.  avert your eyes ----------

use std::convert::TryFrom;
use std::fmt;
use std::line;
use serde::ser::{self, *};

#[derive(Default)]
struct MainExtractor {
  idx: Option<u32>,
  version: Option<u32>,
}

struct ValueExtractor;

type R<Return> = Result<Return, Error>;
type ROk = R<()>;
use self::Error::*;

fn error(line: u32) -> Error { Unexpected(TryFrom::try_from(line).unwrap()) }
fn u<T>(line: u32) -> R<T> { Err(error(line)) }

type Imp = Impossible<(), Error>;
type RI = R<Imp>;

impl Serializer for &mut MainExtractor {
  type Ok = ();
  type Error = Error;

  type SerializeStruct = Self;

  type SerializeMap           = Imp;
  type SerializeSeq           = Imp;
  type SerializeTuple         = Imp;
  type SerializeTupleStruct   = Imp;
  type SerializeTupleVariant  = Imp;
  type SerializeStructVariant = Imp;

  fn serialize_struct(self, _:&str, _: usize) -> R<Self> { Ok(self) }

  fn serialize_bool (self, _: bool )  -> ROk { u(line!()) }
  fn serialize_i8   (self, _: i8   )  -> ROk { u(line!()) }
  fn serialize_i16  (self, _: i16  )  -> ROk { u(line!()) }
  fn serialize_i32  (self, _: i32  )  -> ROk { u(line!()) }
  fn serialize_i64  (self, _: i64  )  -> ROk { u(line!()) }
  fn serialize_u8   (self, _: u8   )  -> ROk { u(line!()) }
  fn serialize_u16  (self, _: u16  )  -> ROk { u(line!()) }
  fn serialize_u32  (self, _: u32  )  -> ROk { u(line!()) }
  fn serialize_u64  (self, _: u64  )  -> ROk { u(line!()) }
  fn serialize_f32  (self, _: f32  )  -> ROk { u(line!()) }
  fn serialize_f64  (self, _: f64  )  -> ROk { u(line!()) }
  fn serialize_char (self, _: char )  -> ROk { u(line!()) }
  fn serialize_str  (self, _: &str )  -> ROk { u(line!()) }
  fn serialize_bytes(self, _: &[u8 ]) -> ROk { u(line!()) }
  fn serialize_none (self)            -> ROk { u(line!()) }
  fn serialize_unit (self)            -> ROk { u(line!()) }

  fn serialize_some<T>(self,                                   _: &T) -> ROk
  where T : Serialize + ?Sized  { u(line!()) }
  fn serialize_newtype_struct <T>(self, _:&str,                _: &T) -> ROk
  where T : Serialize + ?Sized  { u(line!()) }
  fn serialize_newtype_variant<T>(self, _:&str, _:u32, _:&str, _: &T) -> ROk
  where T : Serialize + ?Sized  { u(line!()) }

  fn serialize_unit_struct (self,_:&str             ) -> ROk { u(line!()) }
  fn serialize_unit_variant(self,_:&str,_:u32,_:&str) -> ROk { u(line!()) }
  fn serialize_seq         (self,_:Option<usize>    ) -> RI  { u(line!()) }
  fn serialize_tuple       (self,_:usize            ) -> RI  { u(line!()) }
  fn serialize_tuple_struct(self,_:&str, _:usize    ) -> RI  { u(line!()) }
  fn serialize_map         (self,_:Option<usize>    ) -> RI  { u(line!()) }
 fn serialize_tuple_variant (self,_:&str,_:u32,_:&str,_:usize)->RI{u(line!())}
 fn serialize_struct_variant(self,_:&str,_:u32,_:&str,_:usize)->RI{u(line!())}
}

impl SerializeStruct for &mut MainExtractor {
  type Ok = ();
  type Error = Error;

  fn serialize_field<T: Serialize + ?Sized> (
    &mut self, key: &str, value: &T
  ) -> R<()> {
    if key == "idx"     { self.idx     = Some(value.serialize(ValueExtractor)?); }
    if key == "version" { self.version = Some(value.serialize(ValueExtractor)?); }
    Ok(())
  }

  fn end(self) -> ROk { Ok(()) }
}

type V = u32;
type ImpV = Impossible<V,Error>;
type RIV = R<ImpV>;

impl Serializer for ValueExtractor {
  type Ok = V;
  type Error = Error;
  
  fn serialize_u32(self, value: u32) -> R<V> { Ok(value) }

  type SerializeStruct        = ImpV;
  type SerializeMap           = ImpV;
  type SerializeSeq           = ImpV;
  type SerializeTuple         = ImpV;
  type SerializeTupleStruct   = ImpV;
  type SerializeTupleVariant  = ImpV;
  type SerializeStructVariant = ImpV;

  fn serialize_bool (self, _: bool )  -> R<V> { u(line!()) }
  fn serialize_i8   (self, _: i8   )  -> R<V> { u(line!()) }
  fn serialize_i16  (self, _: i16  )  -> R<V> { u(line!()) }
  fn serialize_i32  (self, _: i32  )  -> R<V> { u(line!()) }
  fn serialize_i64  (self, _: i64  )  -> R<V> { u(line!()) }
  fn serialize_u8   (self, _: u8   )  -> R<V> { u(line!()) }
  fn serialize_u16  (self, _: u16  )  -> R<V> { u(line!()) }
  fn serialize_u64  (self, _: u64  )  -> R<V> { u(line!()) }
  fn serialize_f32  (self, _: f32  )  -> R<V> { u(line!()) }
  fn serialize_f64  (self, _: f64  )  -> R<V> { u(line!()) }
  fn serialize_char (self, _: char )  -> R<V> { u(line!()) }
  fn serialize_str  (self, _: &str )  -> R<V> { u(line!()) }
  fn serialize_bytes(self, _: &[u8 ]) -> R<V> { u(line!()) }
  fn serialize_none (self)            -> R<V> { u(line!()) }
  fn serialize_unit (self)            -> R<V> { u(line!()) }

  fn serialize_some           <T>(self,                        _: &T) -> R<V>
  where T : Serialize + ?Sized  { u(line!()) }
  fn serialize_newtype_struct <T>(self, _:&str,                _: &T) -> R<V>
  where T : Serialize + ?Sized  { u(line!()) }
  fn serialize_newtype_variant<T>(self, _:&str, _:u32, _:&str, _: &T) -> R<V>
  where T : Serialize + ?Sized  { u(line!()) }

  fn serialize_unit_struct   (self,_:&str             ) -> R<V>    { u(line!()) }
  fn serialize_unit_variant  (self,_:&str,_:u32,_:&str) -> R<V>    { u(line!()) }
  fn serialize_seq           (self,_:Option<usize>    ) -> RIV { u(line!()) }
  fn serialize_tuple         (self,_:usize            ) -> RIV { u(line!()) }
  fn serialize_tuple_struct  (self,_:&str, _: usize   ) -> RIV { u(line!()) }
  fn serialize_struct        (self,_:&str, _: usize   ) -> RIV { u(line!()) }
  fn serialize_map           (self,_:Option<usize>    ) -> RIV { u(line!()) }
 fn serialize_tuple_variant (self,_:&str,_:u32,_:&str,_:usize)->RIV{u(line!())}
 fn serialize_struct_variant(self,_:&str,_:u32,_:&str,_:usize)->RIV{u(line!())}
}

impl ser::Error for Error {
  fn custom<T>(_msg: T) -> Self { Error::WasCustomSerialize }
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    fmt::Debug::fmt(self,f)
  }
}

#[test]
fn check(){
  fn t(v: u64) {
    let kd = slotmap::KeyData::from_ffi(v);
    let v = kd.as_ffi(); // KeyData is not transparent for all u64
    let (idx,vsn) = self::KeyDataExt::get_idx_version(kd);
    eprintln!("KeyData={:?} v=0x{:x?} idx={}=0x{:x} vsn={}=0x{:x}",
              &kd, &v, idx,idx, vsn,vsn);
    assert_eq!(v, ((vsn as u64) << 32) | (idx as u64));
  }
  t(0x0123456789abcdef);
  t(0xfedcba9876543210);
  t(0);
  t(0xffffffff12345678);
  t(0x12345678ffffffff);
  t(0xffffffff00000000);
  t(0x00000000ffffffff);
  t(0x0000000012345678);
  t(0x1234567800000000);
}
