// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// This is a multiprecision sort-of-bigfloat with only a handful of
// operations available!
//
// Representation, and model, ought to have these properties
//     CBOR is binary and compact
//  *  JSON is not lossy
//  *  JSON is human-comprehensible
//  *  JavaScript can compare efficiently
//     JavaScript can do arithmetic efficiently
//  *  Limb size is 48 for fast JS arithmetic
//     Limb size is small for not being too full of padding
//  *  Limb size is big so step algorithm rarely encounters limb boundaries
//
// Many of these are not compatible (in theory extending
// the serde data model might help a bit, but not completely).
// We choose those properties marked with "*".
//
// Main representation is a string:
//   VVVVVVVVVV_VVVVVVVVVV ...]
// where
//   VVVVVVVVVV = 50 bits in lowercase base32hex ("0-9a-..."), unsigned
// Value is a whole number of 50-bit groups ("limbs"), at least one sucb.
//
// The abstract value is completed by an infinite number of zero
// limbs to the right.  Trailing zero limbs are forbidden in the
// representation.
//
// Supported operations are:
//
//    Total ordering
//       Values are compared lexically.
//
//    Select initial value:
//       g000000000
//
//    Pick a value (or "n" values) strictly in between any two
//    existing values.
//
//       Find first limb that they differ.  Divide intervening values
//       for tht limb evenly (ie divide the range by n+1).  If this
//       leaves less than an increment of 1 per output value, do
//       differently: choose a value halfway (rounding down) for the
//       first differeing limb, and add a new limb, whose whole range
//       0000000000..vvvvvvvvvv is divided evenly into n+1 (thus
//       guaranteeing that the added limb is nonzero).
//
//    Pick a value later than a specified value.
//
//       Try to add delta to rightmost nonzero limb, with carry.  If
//       this would overflow top limb, start again: add two limbs
//       0000000000 and then redo (this guarantees that one of the
//       added limbs iis nonzero).  Delta is 0001000000.
//
//    Pick a value earlier than a specified value.
//
//       Try to subtract delta from rightmost nonzero limb, with
//       borrow.  If this would underflow, or would change leftmost
//       limb to 0000000000, start again: decrement rightmost nonzero
//       limb by 1, with borrow, then add two limbs vvvvvvvvvv, and
//       redo.

use std::cmp::{Ordering, max};
use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Debug, Display, Formatter};
use std::num::{TryFromIntError, Wrapping};
use std::str;
use fehler::{throw, throws};
use serde::{Serialize, Serializer, Deserialize};
use thiserror::Error;

//---------- core definitions ----------

const BITS_PER_DIGIT : usize = 5;
const DIGITS_PER_LIMB : usize = 10;

type RawLimbVal = u64;
type LimbVal = Wrapping<RawLimbVal>;

const DELTA : LimbVal = Wrapping(0x4000_0000);
const ZERO : LimbVal = Wrapping(0);
const ONE : LimbVal = Wrapping(1);

const RAW_LIMB_MODULUS : RawLimbVal = 1u64 << BITS_PER_LIMB;

const BITS_PER_LIMB : usize = BITS_PER_DIGIT * DIGITS_PER_LIMB;
const DIGIT_MASK : LimbVal = Wrapping((1u64 << BITS_PER_DIGIT) - 1);
const TEXT_PER_LIMB : usize = DIGITS_PER_LIMB + 1;
const LIMB_MODULUS : LimbVal = Wrapping(RAW_LIMB_MODULUS);
const LIMB_MASK    : LimbVal = Wrapping(RAW_LIMB_MODULUS-1);

#[derive(Deserialize)]
#[serde(try_from="&str")]
pub struct ZCoord(innards::Innards);

#[derive(Error,Clone,Copy,Debug)]
#[error("error parsing zcoord (z value)")]
pub struct ParseError;

//---------- Mutabel ----------

#[derive(Clone,Debug)]
pub struct Mutable {
  limbs: Vec<LimbVal>,
}

impl ZCoord {
  pub fn clone_mut(&self) -> Mutable {
    let tail = self.tail();
    let nlimbs = (tail.len() + 1) / TEXT_PER_LIMB;
    let mut limbs = Vec::with_capacity(nlimbs+2);
    for lt in tail.chunks(TEXT_PER_LIMB) {
      let s = str::from_utf8(&lt[0..DIGITS_PER_LIMB]).unwrap();
      let v = RawLimbVal::from_str_radix(s, 1 << BITS_PER_DIGIT).unwrap();
      limbs.push(Wrapping(v));
    }
    Mutable { limbs }
  }
}

#[derive(Error,Debug,Copy,Clone,Serialize,Deserialize)]
#[error("Z coordinate overflow")]
pub struct Overflow;

impl From<TryFromIntError> for Overflow {
  fn from(_: TryFromIntError) -> Overflow { Overflow }
}

trait IncDecOffset {
  const INIT_DELTA  : LimbVal;
  const CARRY_DELTA : LimbVal;
  const NEW_LIMBS   : LimbVal;
  fn check_underflow(m: &Mutable, i: usize, nv: LimbVal) -> Option<()>;
}

struct IncDecInc;
impl IncDecOffset for IncDecInc {
  const INIT_DELTA  : LimbVal = DELTA;
  const CARRY_DELTA : LimbVal = ONE;
  const NEW_LIMBS   : LimbVal = ZERO;
  #[throws(as Option)]
  fn check_underflow(_: &Mutable, _: usize, _: LimbVal) { }
}

struct IncDecDec;
impl IncDecOffset for IncDecDec {
  const INIT_DELTA  : LimbVal = Wrapping(DELTA.0.wrapping_neg());
  const CARRY_DELTA : LimbVal = Wrapping(ONE  .0.wrapping_neg());
  const NEW_LIMBS   : LimbVal = LIMB_MASK;
  #[throws(as Option)]
  fn check_underflow(_: &Mutable, i: usize, nv: LimbVal) {
    if i == 0 && nv == ZERO { throw!() }
  }
}

impl Mutable {
  #[throws(Overflow)]
  fn incdec<ID:IncDecOffset>(&mut self, _: ID) -> ZCoord {
    'attempt: loop {
      let mut i = self.limbs.len() - 1;
      let mut delta = ID::INIT_DELTA;

      if (||{
        loop {
          let nv = self.limbs[i] + delta;
          self.limbs[i] = nv & LIMB_MASK;
          ID::check_underflow(self, i, nv)?;
          if nv < LIMB_MODULUS { return Some(()) }
          if i == 0 { return None }
          i -= 1;
          delta = ID::CARRY_DELTA;
        }
      })() == Some(()) { break 'attempt }

      // undo
      loop {
        if i >= self.limbs.len() { break }
        else if i == self.limbs.len()-1 { delta = ID::INIT_DELTA; }
        let nv = self.limbs[i] - delta;
        self.limbs[i] = nv & LIMB_MASK;
        i += 1;
      }
      self.limbs.push(ID::NEW_LIMBS);
      self.limbs.push(ID::NEW_LIMBS);
    }
    self.repack()?
  }

  #[throws(Overflow)]
  pub fn increment(&mut self) -> ZCoord { self.incdec(IncDecInc)? }
  #[throws(Overflow)]
  pub fn decrement(&mut self) -> ZCoord { self.incdec(IncDecDec)? }

  #[throws(Overflow)]
  pub fn repack(&self) -> ZCoord { self.try_into()? }
}

//---------- main features of a Zcoord ----------

impl ZCoord {
  pub fn as_str(&self) -> &str {
    let tail = self.tail();
    str::from_utf8(tail).unwrap()
  }

  pub fn to_string(&self) -> String {
    self.as_str().to_string()
  }
}

impl Display for ZCoord {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    write!(f, "{}", self.as_str())?
  }
}

impl Debug for ZCoord {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    write!(f, r#"Bf""#)?;
    <ZCoord as Display>::fmt(self, f)?;
    write!(f, r#"""#)?;
  }    
}

impl Ord for ZCoord {
  fn cmp(&self, other: &ZCoord) -> Ordering {
    let at = self.tail();
    let bt = other.tail();
    at.cmp(bt)
  }
}
impl PartialOrd for ZCoord {
  fn partial_cmp(&self, other: &ZCoord) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}
impl Eq for ZCoord {
}
impl PartialEq for ZCoord {
  fn eq(&self, other: &ZCoord) -> bool {
    self.cmp(other) == Ordering::Equal
  }
}

impl TryFrom<&str> for ZCoord {
  type Error = ParseError;
  #[throws(ParseError)]
  fn try_from(s: &str) -> ZCoord {
    ZCoord::from_str(s).ok_or(ParseError)?
  }
}

impl Serialize for ZCoord {
  fn serialize<S:Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
    s.serialize_str(self.as_str())
  }
}

//---------- construction of ZCoord contents ---------
//
// We can panic if this code is buggy, but not compromise safety.

const DEFAULT_TEXT  : &[u8] = b"gggggggggg";

impl Default for ZCoord {
  fn default() -> ZCoord {
    ZCoord::alloc_copy(DEFAULT_TEXT).unwrap()
  }
}

impl ZCoord {
  #[throws(as Option)]
  pub fn from_str(s: &str) -> Self {
    let s = s.as_bytes();
    let nomlen = s.len() + 1;
    if nomlen % TEXT_PER_LIMB !=0 { None? }
    for lt in s.chunks(TEXT_PER_LIMB) {
      if !lt[0..DIGITS_PER_LIMB].iter().all(
        |c: &u8| {
          (b'0'..=b'9').contains(&c) ||
          (b'a'..=b'v').contains(&c)
        }) { None? }
      match lt[DIGITS_PER_LIMB..] { [] | [b'_'] => (), _ => None? };
    }
    if &s[s.len() - DIGITS_PER_LIMB.. ] == b"0000000000" { None? }
    ZCoord::alloc_copy(s).ok()?
  }
}

impl TryFrom<&Mutable> for ZCoord {
  type Error = Overflow;
  #[throws(Overflow)]
  fn try_from(m: &Mutable) -> ZCoord {
    let taillen = (m.limbs.len() * TEXT_PER_LIMB - 1).try_into()?;
    let mut bf = ZCoord::alloc(taillen);
    let mut w = bf.tail_mut();
    for mut l in m.limbs.iter().cloned() {
      if l >= LIMB_MODULUS { throw!(Overflow) };
      for p in w[0..DIGITS_PER_LIMB].rchunks_exact_mut(1) {
        let v = (l & DIGIT_MASK).0 as u8;
        p[0] = if v < 10 { b'0' + v } else { (b'a' - 10) + v };
        l >>= BITS_PER_DIGIT;
      }
      if let Some(p) = w.get_mut(DIGITS_PER_LIMB) {
        *p = b'_';
      } else {
        break;
      }
      w = &mut w[TEXT_PER_LIMB..];
    }
    bf
  }
}

//---------- innards, unsafe ----------

mod innards {
  use super::*;
  use std::mem::{self, align_of, size_of};
  use std::ptr::{self, NonNull};
  use std::alloc::{self, Layout};
  use std::slice;

  unsafe impl Send for ZCoord { }
  unsafe impl Sync for ZCoord { }

  pub(in super) type Innards = NonNull<u8>;
  type Taillen = u16;
  type Tail1 = u8;

  pub(in super)
  struct Header {
    pub taillen: u16,
  }

  #[repr(C)]
  #[allow(dead_code)] // this is for documentation purposes
  struct Repr {
    h: Header,
    d: [Tail1],
  }

  const OFFSET : usize = {
    let h_size = size_of::<Header>();
    let l_align = align_of::<Tail1>();
    l_align * ((h_size + l_align - 1) / l_align)
  };

  fn layout(len: Taillen) -> (usize, Layout) {
    let tail_nbytes : usize = size_of::<Tail1>() * (len as usize);
    let all_nbytes = OFFSET + tail_nbytes;
    let align = max(align_of::<Header>(), align_of::<Tail1>());
    (all_nbytes, Layout::from_size_align(all_nbytes, align).unwrap())
  }

  fn ptrs(p: *mut u8) -> (*mut Header, *mut Tail1) { unsafe {
    let p_header : *mut Header = mem::transmute(p);
    let p_tail   : *mut Tail1  = mem::transmute(p.add(OFFSET));
    (p_header, p_tail)
  } }

  impl ZCoord {
    unsafe fn alloc_unsafe<F>(taillen: Taillen, f:F) -> ZCoord
    where F: FnOnce(*mut Tail1)
    {
      #[allow(unused_unsafe)] // unsafe block in unsafe fn
      unsafe {
        let p = alloc::alloc(layout(taillen).1);
        let (p_header, p_tail) = ptrs(p);
        ptr::write(p_header, Header { taillen });
        f(p_tail);
        ZCoord(NonNull::new(p).unwrap())
      }
    }
  
    pub(in super)
    fn alloc(taillen: Taillen) -> ZCoord {
      unsafe {
        ZCoord::alloc_unsafe(taillen, |nt : *mut Tail1| {
          ptr::write_bytes(nt, 0, taillen as usize);
        })
      }
    }

    #[throws(Overflow)]
    pub(in super)
    fn alloc_copy(tail: &[Tail1]) -> ZCoord {
      let taillen = tail.len().try_into()?;
      unsafe {
        ZCoord::alloc_unsafe(taillen, |nt : *mut Tail1| {
          ptr::copy_nonoverlapping(tail.as_ptr(), nt, taillen as usize);
        })
      }
    }

    pub(in super)
    fn tail(&self) -> &[Tail1] {
      unsafe {
        let (h, t) = ptrs(self.0.as_ptr());
        let h = h.as_ref().unwrap();
        slice::from_raw_parts(t, h.taillen as usize)
      }
    }

    pub(in super)
    fn tail_mut(&mut self) -> &mut [Tail1] {
      unsafe {
        let (h, t) = ptrs(self.0.as_ptr());
        let h = h.as_ref().unwrap();
        slice::from_raw_parts_mut(t, h.taillen as usize)
      }
    }

    fn layout(&self) -> (usize, Layout) {
      unsafe {
        let (h, _) = ptrs(self.0.as_ptr());
        let h = h.as_ref().unwrap();
        let taillen = h.taillen;
        layout(taillen)
      }
    }
  }

  impl Drop for ZCoord {
    fn drop(&mut self) {
      let layout = self.layout().1;
      unsafe {
        alloc::dealloc(self.0.as_mut(), layout);
      }
    }
  }

  impl Clone for ZCoord {
    fn clone(&self) -> ZCoord {
      let (all_bytes, layout) = self.layout();
      unsafe {
        let p = alloc::alloc(layout);
        ptr::copy_nonoverlapping(self.0.as_ptr(), p, all_bytes);
        ZCoord(NonNull::new(p).unwrap())
      }
    }
  }

}

//---------- tests ----------

#[cfg(test)]
mod test {
  // everything from here on is seded by the js test extractor!
  use super::*;

  fn bf(s: &str) -> ZCoord {
    ZCoord::from_str(s).unwrap()
  }

  #[test]
  fn bfparse() {
    let s = "gg0123abcd_0123456789";
    let b = ZCoord::from_str(s).unwrap();
    let b2 = b.clone();
    assert_eq!(format!("{}", &b), s);
    assert_eq!(format!("{}", &b.clone_mut().repack().unwrap()), s);
    mem::drop(b);
    assert_eq!(format!("{}", &b2), s);
    assert_eq!(format!("{:?}", &b2),
               format!(r#"Bf"{}""#, &b2));
    fn bad(s: &str) { assert_eq!(None, ZCoord::from_str(s)); }
    bad("");
    bad("0");
    bad("0000000000");
    bad("0000000000_0000000000");
    bad("aaaaaaaa0_aaaaaaaa00");
    bad("aaaaaaaa0_aaaaaaaa00");
    bad("aaaaaaaa00_aaaaaaaa0");
    bad("#aaaaaaaa0_aaaaaaaa00");
    bad("aaaaaaaa0#_aaaaaaaa00");
    bad("aaaaaaaa00#aaaaaaaa00");
    bad("aaaaaaaa00_aaaaaaaa0#");
    bad("Zaaaaaaaa0_#aaaaaaaa0");
    bad("Aaaaaaaaa0_#aaaaaaaa0");
    bad("waaaaaaaa0_#aaaaaaaa0");
    bad("/aaaaaaaa0_#aaaaaaaa0");
    bad(":aaaaaaaa0_#aaaaaaaa0");
    bad("`aaaaaaaa0_#aaaaaaaa0");
  }

  #[test]
  fn equality() {
    assert!( bf("gg0123abcd_0123456789") <
             bf("gg0123abcd_012345678a") );
    
    assert!( bf("gg0123abcd") <
             bf("gg0123abcd_012345678a") );
  }

  #[test]
  fn addition() {
    fn mk(s: &str) -> super::Mutable { bf(s).clone_mut() }
    impl Mutable {
      fn tinc(mut self, exp: &str) -> Self {
        let got = self.increment().unwrap();
        assert_eq!(got.to_string(), exp);
        self
      }
    }
    mk("000000000a")
      .tinc("000100000a")
      .tinc("000200000a")
      ;
    mk("vvvvvvvvvv")
      .tinc("vvvvvvvvvv_0000000000_0001000000")
      ;
    mk("vvvvvvvvvv_vvvvvvvvvv_vvvvv01234")
      .tinc("vvvvvvvvvv_vvvvvvvvvv_vvvvv01234_0000000000_0001000000")
      ;
  }
}
