// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use otter_base::imports::*;

use std::fmt::Display;
use std::collections::hash_map::HashMap;

use fehler::throws;
use js_sys::JsString;
use thiserror::Error;
use wasm_bindgen::prelude::*;

use otter_base::imports::serde_json;

use otter_base::geometry::{PosC,RegionC};
use otter_base::zcoord;
use otter_base::misc as base_misc;
use zcoord::{Mutable,ZCoord};
use base_misc::default;

// ---------- general, errors, etc. ----------

#[derive(Error,Clone,Copy,Debug,Eq,PartialEq)]
#[error("packed Z coordinate wrong JS type (not a string)")]
pub struct JsZCoordTypeError;

trait WasmError {
  fn e(self) -> JsValue;
}

impl<E> WasmError for E where E: Display+Copy {
  fn e(self) -> JsValue {
    let s = format!("{}", self);
    JsValue::from_str(&s)
  }
}

trait WasmResult<V> {
  fn e(self) -> Result<V,JsValue>;
}

impl<E:Display+Copy, V> WasmResult<V> for Result<V, E> {
  fn e(self) -> Result<V, JsValue> { self.map_err(WasmError::e) }
}

#[throws(JsValue)]
fn get_packed_str(js: &JsValue) -> String {
  js.as_string().ok_or(JsZCoordTypeError).e()?
}

// ---------- zcoord ----------

#[throws(JsValue)]
#[wasm_bindgen]
pub fn check(packed: &JsValue) {
  ZCoord::check_str(&get_packed_str(packed)?).e()?;
}

#[throws(JsValue)]
#[wasm_bindgen]
pub fn increment(packed: &JsValue) -> JsValue {
  let mut m = Mutable::from_str(&get_packed_str(packed)?).e()?;
  m.increment().e()?.to_string().into()
}

#[wasm_bindgen]
pub fn def_zcoord() -> JsValue {
  let z: ZCoord = default();
  z.to_string().into()
}

#[wasm_bindgen]
pub struct ZCoordIterator(zcoord::BoxedIterator);

#[throws(JsValue)]
#[wasm_bindgen]
pub fn range(a: &JsValue, b: &JsValue, count: zcoord::RangeCount)
             -> ZCoordIterator {
  #[throws(JsValue)]
  fn get1(js: &JsValue) -> Option<Mutable> {
    if js.is_null() { return None }
    let s = get_packed_str(js)?;
    let m = Mutable::from_str(&s).e()?;
    Some(m)
  }

  let a = get1(a)?;
  let b = get1(b)?;
  let inner = Mutable::some_range(a.as_ref(), b.as_ref(), count).e()?;
  ZCoordIterator(inner)
}

#[wasm_bindgen]
impl ZCoordIterator {
  pub fn next(&mut self) -> JsValue {
    let packed = match self.0.next() {
      None => return JsValue::NULL,
      Some(p) => p,
    };
    packed.to_string().into()
  }
  pub fn debug(&self) -> JsValue {
    format!("ZCoordIterator({:?})", &self.0).into()
  }
}

// ---------- timestamps ----------

#[wasm_bindgen]
pub struct TimestampAbbreviator {
  last: String,
}

#[wasm_bindgen]
pub fn timestamp_abbreviator(j: JsString) -> TimestampAbbreviator {
  TimestampAbbreviator { last: j.into() }
}

#[wasm_bindgen]
impl TimestampAbbreviator {
  pub fn update(&mut self, now: &JsString) -> JsString {
    let now: String = now.into();
    let (abbrev, _) = base_misc::timestring_abbreviate(&self.last, &now);
    let abbrev = abbrev.into();
    self.last = now;
    abbrev
  }
}

// ---------- angle ----------

#[wasm_bindgen]
pub fn angle_transform(angle: u8) -> JsString {
  base_misc::raw_angle_transform(angle).into()
}

// ---------- region ----------

#[derive(Error,Clone,Copy,Debug,Eq,PartialEq)]
#[error("region or piece argument not suitable")]
pub struct BadRegionArguments;

type Number = f64;

#[wasm_bindgen]
pub struct RegionList(HashMap<String, RegionC<Number>>);

#[wasm_bindgen]
pub fn empty_region_list() -> RegionList { RegionList(default()) }

#[wasm_bindgen]
impl RegionList {
  pub fn insert(&mut self, piece: JsValue, region: JsValue)
                -> Result<(), JsValue>
  {
    let piece  = piece .as_string().ok_or(BadRegionArguments).e()?;
    let region = region.as_string().ok_or(BadRegionArguments).e()?;
    let region: RegionC<Number> = serde_json::from_str(&region)
      .map_err(|_| BadRegionArguments).e()?;
    self.0.insert(piece, region);
    Ok(())
  }

  pub fn remove(&mut self, piece: JsValue)
                -> Result<(), JsValue>
  {
    let piece  = piece .as_string().ok_or(BadRegionArguments).e()?;
    self.0.remove(&piece);
    Ok(())
  }

  pub fn contains_pos(&mut self, x: Number, y: Number) -> bool {
    self.0
      .values()
      .any(
        |r| r.contains(PosC::new(x,y))
      )
  }
}
 

// ---------- setup ----------

#[wasm_bindgen]
pub fn setup(s: &str) -> JsString {
  // returning String produces a wasm-opt error, as here
  //  https://github.com/WebAssembly/binaryen/issues/3006
  console_error_panic_hook::set_once();
  format!("WASM {}", s).into()
}

//#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
