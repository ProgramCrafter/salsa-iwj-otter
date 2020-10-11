
use wasm_bindgen::prelude::*;
use fehler::throws;
use std::fmt::Display;
use js_sys::JsString;

use zcoord::ZCoord;

#[wasm_bindgen]
pub struct ZCoordIterator (zcoord::Mutable);

trait WasmError {
  fn e(self) -> JsValue;
}

impl<E> WasmError for E where E: Display {
  fn e(self) -> JsValue {
    let s = format!("{}", self);
    JsValue::from_str(&s)
  }
}

trait WasmResult<V> {
  fn e(self) -> Result<V,JsValue>;
}

impl<E:Display, V> WasmResult<V> for Result<V, E> {
  fn e(self) -> Result<V, JsValue> { self.map_err(WasmError::e) }
}

#[throws(JsValue)]
#[wasm_bindgen]
pub fn check(packed: &JsValue) {
  let s = packed.as_string().ok_or(
    "packed Z coordinate wrong JS type (not a string)",
  ).e()?;
  ZCoord::check_str(&s).e()?;
}

//const X : &'static str = "invalid value passed to wasm";
/*
#[throws(JsValue)]
#[wasm_bindgen]
pub fn mutable(s: String) -> ZCoordIterator {
  ZCoordIterator(ZCoord::from_str(&s).ok_or(X)?.clone_mut())
}*/

#[wasm_bindgen]
impl ZCoordIterator {
  pub fn next(&mut self) -> u32 { 42 }
}

#[wasm_bindgen]
pub fn canary(s: &str) -> JsString {
  // returning String produces a wasm-opt error, as here
  //  https://github.com/WebAssembly/binaryen/issues/3006
  format!("WASM {}", s).into()
}

//#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
