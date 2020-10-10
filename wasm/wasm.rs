
use wasm_bindgen::prelude::*;
use fehler::throws;

use zcoord::ZCoord;

#[wasm_bindgen]
pub struct ZCoordIterator (zcoord::Mutable);

const X : &'static str = "invalid value passed to wasm";

#[throws(JsValue)]
#[wasm_bindgen]
pub fn mutable(s: String) -> ZCoordIterator {
  ZCoordIterator(ZCoord::from_str(&s).ok_or(X)?.clone_mut())
}
