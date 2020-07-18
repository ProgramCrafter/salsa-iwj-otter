
use serde::ser::{self,*};
use thiserror::Error;
use std::line;

pub trait KeyDataExt {
  fn get_idx_version(self) -> (u32, u32);
}

impl KeyDataExt for slotmap::KeyData {
  fn get_idx_version(self) -> (u32, u32) {
    keydata_extract(self).unwrap()
  }
}

pub fn keydata_extract(key : slotmap::KeyData) -> Result<(u32, u32), Error> {
  let mut m : MainExtractor = std::default::Default::default();
  key.serialize(&mut m)?;
  Ok(( m.idx    .ok_or(error(line!()))?,
       m.version.ok_or(error(line!()))? ))
}

#[derive(Error,Debug)]
pub enum Error {
  WasCustomSerialize,
  Unexpected(std::num::NonZeroU32),
}

#[derive(Default)]
struct MainExtractor {
  idx: Option<u32>,
  version: Option<u32>,
}

struct ValueExtractor;

type R<Return> = Result<Return,Error>;
type ROk = R<()>;


fn error(line: u32) -> Error { Error::Unexpected(std::convert::TryFrom::try_from(line).unwrap()) }
fn unexpected<T>(line: u32) -> R<T> { Err(error(line)) }

type Imp = Impossible<(),Error>;

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

  fn serialize_bool (self, _: bool )  -> ROk { unexpected(line!()) }
  fn serialize_i8   (self, _: i8   )  -> ROk { unexpected(line!()) }
  fn serialize_i16  (self, _: i16  )  -> ROk { unexpected(line!()) }
  fn serialize_i32  (self, _: i32  )  -> ROk { unexpected(line!()) }
  fn serialize_i64  (self, _: i64  )  -> ROk { unexpected(line!()) }
  fn serialize_u8   (self, _: u8   )  -> ROk { unexpected(line!()) }
  fn serialize_u16  (self, _: u16  )  -> ROk { unexpected(line!()) }
  fn serialize_u32  (self, _: u32  )  -> ROk { unexpected(line!()) }
  fn serialize_u64  (self, _: u64  )  -> ROk { unexpected(line!()) }
  fn serialize_f32  (self, _: f32  )  -> ROk { unexpected(line!()) }
  fn serialize_f64  (self, _: f64  )  -> ROk { unexpected(line!()) }
  fn serialize_char (self, _: char )  -> ROk { unexpected(line!()) }
  fn serialize_str  (self, _: &str )  -> ROk { unexpected(line!()) }
  fn serialize_bytes(self, _: &[u8 ]) -> ROk { unexpected(line!()) }
  fn serialize_none (self)            -> ROk { unexpected(line!()) }
  fn serialize_unit (self)            -> ROk { unexpected(line!()) }

  fn serialize_some           <T:Serialize+?Sized>(self,                        _: &T) -> ROk { unexpected(line!()) }
  fn serialize_newtype_struct <T:Serialize+?Sized>(self, _:&str,                _: &T) -> ROk { unexpected(line!()) }
  fn serialize_newtype_variant<T:Serialize+?Sized>(self, _:&str, _:u32, _:&str, _: &T) -> ROk { unexpected(line!()) }

  fn serialize_unit_struct                 (self, _: &str                           ) -> ROk    { unexpected(line!()) }
  fn serialize_unit_variant                (self, _: &str, _:u32, _:&str            ) -> ROk    { unexpected(line!()) }
  fn serialize_seq                         (self, _: Option<usize>                  ) -> R<Imp> { unexpected(line!()) }
  fn serialize_tuple                       (self, _: usize                          ) -> R<Imp> { unexpected(line!()) }
  fn serialize_tuple_struct                (self, _: &str, _: usize                 ) -> R<Imp> { unexpected(line!()) }
  fn serialize_tuple_variant               (self, _: &str, _: u32, _: &str, _: usize) -> R<Imp> { unexpected(line!()) }
  fn serialize_map                         (self, _: std::option::Option<usize>     ) -> R<Imp> { unexpected(line!()) }
  fn serialize_struct_variant              (self, _: &str, _: u32, _: &str, _: usize) -> R<Imp> { unexpected(line!()) }
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

  fn serialize_bool (self, _: bool )  -> R<V> { unexpected(line!()) }
  fn serialize_i8   (self, _: i8   )  -> R<V> { unexpected(line!()) }
  fn serialize_i16  (self, _: i16  )  -> R<V> { unexpected(line!()) }
  fn serialize_i32  (self, _: i32  )  -> R<V> { unexpected(line!()) }
  fn serialize_i64  (self, _: i64  )  -> R<V> { unexpected(line!()) }
  fn serialize_u8   (self, _: u8   )  -> R<V> { unexpected(line!()) }
  fn serialize_u16  (self, _: u16  )  -> R<V> { unexpected(line!()) }
  fn serialize_u64  (self, _: u64  )  -> R<V> { unexpected(line!()) }
  fn serialize_f32  (self, _: f32  )  -> R<V> { unexpected(line!()) }
  fn serialize_f64  (self, _: f64  )  -> R<V> { unexpected(line!()) }
  fn serialize_char (self, _: char )  -> R<V> { unexpected(line!()) }
  fn serialize_str  (self, _: &str )  -> R<V> { unexpected(line!()) }
  fn serialize_bytes(self, _: &[u8 ]) -> R<V> { unexpected(line!()) }
  fn serialize_none (self)            -> R<V> { unexpected(line!()) }
  fn serialize_unit (self)            -> R<V> { unexpected(line!()) }

  fn serialize_some           <T:Serialize+?Sized>(self,                        _: &T) -> R<V> { unexpected(line!()) }
  fn serialize_newtype_struct <T:Serialize+?Sized>(self, _:&str,                _: &T) -> R<V> { unexpected(line!()) }
  fn serialize_newtype_variant<T:Serialize+?Sized>(self, _:&str, _:u32, _:&str, _: &T) -> R<V> { unexpected(line!()) }

  fn serialize_unit_struct                 (self, _: &str                           ) -> R<V>    { unexpected(line!()) }
  fn serialize_unit_variant                (self, _: &str, _:u32, _:&str            ) -> R<V>    { unexpected(line!()) }
  fn serialize_seq                         (self, _: Option<usize>                  ) -> R<ImpV> { unexpected(line!()) }
  fn serialize_tuple                       (self, _: usize                          ) -> R<ImpV> { unexpected(line!()) }
  fn serialize_tuple_struct                (self, _: &str, _: usize                 ) -> R<ImpV> { unexpected(line!()) }
  fn serialize_tuple_variant               (self, _: &str, _: u32, _: &str, _: usize) -> R<ImpV> { unexpected(line!()) }
  fn serialize_struct                      (self, _: &str, _: usize                 ) -> R<ImpV> { unexpected(line!()) }
  fn serialize_map                         (self, _: std::option::Option<usize>     ) -> R<ImpV> { unexpected(line!()) }
  fn serialize_struct_variant              (self, _: &str, _: u32, _: &str, _: usize) -> R<ImpV> { unexpected(line!()) }
}

impl ser::Error for Error {
  fn custom<T>(_msg: T) -> Self { return Error::WasCustomSerialize; }
}

impl std::fmt::Display for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
    std::fmt::Debug::fmt(self,f)
  }
}
