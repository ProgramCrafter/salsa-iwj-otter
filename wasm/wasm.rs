// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use std::fmt::Display;

use fehler::throws;
use js_sys::JsString;
use thiserror::Error;
use wasm_bindgen::prelude::*;

use zcoord::{Mutable,ZCoord};

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
  let z: ZCoord = Default::default();
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
    let (abbrev, _) = zcoord::misc::timestring_abbreviate(&self.last, &now);
    let abbrev = abbrev.into();
    self.last = now;
    abbrev
  }
}

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
