// See bigfloat.ts

#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_mut)]

enum Sign { Pos, Neg, }
use Sign::*;

struct Bigfloat {
  sign: Sign,
  exp: u16,
  limbs: [[u16;3]],
}

impl Bigfloat {
  fn from_str(s: &str) -> Option<Box<Self>> {
    let mut s = s.as_bytes();
    let mut s = s.iter();
    let sign = match *s.next()? {
      b'+' => Pos,
      b'!' => Neg,
      _ => None?,
    };
    panic!();
  }
}
