// Copyright 2020-2021 Ian Jackson and contributors to Otter
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
// The value "0000000000" is forbidden.
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
//       Find first limb that they differ.    If this
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
//
//    Given a base value, and an u32 "offset multiplier", produce a value
//
//       The resulting values are all greater than the base, and
//       in the same order as the provided offset multipliers.
//       The function is deterministic

use crate::prelude::*;

//---------- core definitions ----------

pub type RangeCount = u32;

type Tail1 = u8;

const BITS_PER_DIGIT: usize = 5;
const DIGITS_PER_LIMB: usize = 10;

type RawLimbVal = u64;
#[derive(Copy,Clone,Eq,PartialEq,Ord,PartialOrd)]
#[derive(Neg,Add,BitAnd,Sub,Shr,ShrAssign)]
pub struct LimbVal(Wrapping<RawLimbVal>);

const DELTA: LimbVal = lv(0x4000_0000);
const ZERO: LimbVal = lv(0);
const ONE: LimbVal = lv(1);
const MINUS_ONE: LimbVal = lv(-1i64 as u64);

const RAW_LIMB_MODULUS: RawLimbVal = 1u64 << BITS_PER_LIMB;

const BITS_PER_LIMB: usize = BITS_PER_DIGIT * DIGITS_PER_LIMB;
const DIGIT_MASK: LimbVal = lv((1u64 << BITS_PER_DIGIT) - 1);
const TEXT_PER_LIMB: usize = DIGITS_PER_LIMB + 1;
const LIMB_MODULUS: LimbVal = lv(RAW_LIMB_MODULUS);
const LIMB_MASK: LimbVal = lv(RAW_LIMB_MODULUS - 1);

#[derive(DeserializeFromStr,SerializeDisplay)]
pub struct ZCoord(innards::Innards);

#[derive(Error,Clone,Copy,Debug,Eq,PartialEq,Serialize,Deserialize)]
#[error("error parsing Z coordinate")]
pub struct ParseError;

#[derive(Error,Clone,Copy,Debug,Eq,PartialEq,Serialize,Deserialize)]
pub enum RangeImpossible {
  #[error("Z coordinate range has end before start, cannot iterate")]
  Backwards,
  #[error("Z coordinate range has end equal to start, cannot iterate")]
  Empty,
}

#[derive(Error,Clone,Copy,Debug,Eq,PartialEq,Serialize,Deserialize)]
#[error("Z coordinate range has neither end, cannot iterate")]
pub struct TotallyUnboundedRange;

#[derive(Error,Debug,Copy,Clone,Eq,PartialEq,Serialize,Deserialize)]
#[error("Z coordinate overflow")]
pub struct Overflow;

#[derive(Error,Clone,Copy,Debug,Eq,PartialEq,Serialize,Deserialize)]
pub enum LogicError {
  #[error("{0}")] RangeTotallyUnbounded(#[from] TotallyUnboundedRange),
  #[error("{0}")] RangeImpossible      (#[from] RangeImpossible      ),
}

//---------- LimbVal ----------

impl From<RawLimbVal> for LimbVal {
  fn from(raw: RawLimbVal) -> LimbVal { lv(raw) }
}

const fn lv(raw: RawLimbVal) -> LimbVal { LimbVal(Wrapping(raw)) }

impl LimbVal {
  pub fn primitive(self) -> RawLimbVal { self.0.0 }
  /// return value is the top bits, shifted
  pub fn to_str_buf(self, out: &mut [Tail1; DIGITS_PER_LIMB]) -> LimbVal {
    let mut l = self;
    for p in out.into_iter().rev() {
      let v = (l & DIGIT_MASK).primitive() as u8;
      *p = if v < 10 { b'0' + v } else { (b'a' - 10) + v };
      l >>= BITS_PER_DIGIT;
    }
    l
  }
}

impl Display for LimbVal {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    let mut buf = [0u8; DIGITS_PER_LIMB];
    let lhs: RawLimbVal = self.to_str_buf(&mut buf).primitive();
    if lhs != 0 {
      write!(f, "{:#x?}_!_", lhs)?;
    }
    write!(f, "{}", str::from_utf8(&buf).unwrap())?;
  }
}

impl Debug for LimbVal {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    write!(f, "lv(")?;
    Display::fmt(self, f)?;
    write!(f, ")")?;
  }
}

//---------- Mutabel ----------

#[derive(Clone,Debug)]
pub struct Mutable {
  limbs: Vec<LimbVal>,
}

impl ZCoord {
  pub fn clone_mut(&self) -> Mutable {
    Mutable::from_u8_unchecked(self.tail())
  }
}

impl Mutable {
  fn from_u8_unchecked(tail: &[u8]) -> Mutable {
    let nlimbs = (tail.len() + 1) / TEXT_PER_LIMB;
    let mut limbs = Vec::with_capacity(nlimbs + 2);
    for lt in tail.chunks(TEXT_PER_LIMB) {
      let s = str::from_utf8(&lt[0..DIGITS_PER_LIMB]).unwrap();
      let v = RawLimbVal::from_str_radix(s, 1 << BITS_PER_DIGIT).unwrap();
      limbs.push(v.into());
    }
    Mutable { limbs }
  }
}

impl From<TryFromIntError> for Overflow {
  fn from(_: TryFromIntError) -> Overflow { Overflow }
}

pub trait AddSubOffset {
  fn init_delta(&self) -> LimbVal;
  const CARRY_DELTA      : LimbVal;
  const NEW_LIMBS        : LimbVal;
  fn check_underflow(m: &Mutable, i: usize, nv: LimbVal) -> Option<()>;
  #[throws(as Option)]
  fn check_nospace(i: usize) { if i == 0 { throw!() } }
  fn start_limb(&self, m: &Mutable) -> usize { m.limbs.len() - 1 }
  fn final_undo_delta() -> LimbVal;
  const SEALED_TRAIT: Sealed;
}

pub struct Sealed(());

#[derive(Debug)]
pub struct Increment;
impl AddSubOffset for Increment {
  fn init_delta(&self) -> LimbVal { DELTA }
  const CARRY_DELTA      : LimbVal = ONE;
  const NEW_LIMBS        : LimbVal = ZERO;
  #[throws(as Option)]
  fn check_underflow(_: &Mutable, _: usize, _: LimbVal) { }
  fn final_undo_delta() -> LimbVal { DELTA }
  const SEALED_TRAIT: Sealed = Sealed(());
}

#[derive(Debug)]
pub struct Decrement;
impl AddSubOffset for Decrement {
  fn init_delta(&self) -> LimbVal { -DELTA }
  const CARRY_DELTA : LimbVal = MINUS_ONE;
  const NEW_LIMBS   : LimbVal = LIMB_MASK;
  #[throws(as Option)]
  fn check_underflow(_: &Mutable, i: usize, nv: LimbVal) {
    if i == 0 && nv == ZERO { throw!() }
  }
  fn final_undo_delta() -> LimbVal { -DELTA + ONE }
  const SEALED_TRAIT: Sealed = Sealed(());
}

impl Mutable {
  #[throws(Overflow)]
  fn addsub<ASO:AddSubOffset>(&mut self, aso: &ASO) -> ZCoord {
    'attempt: loop {
      let mut i = aso.start_limb(self);
      let mut delta = aso.init_delta();

      if (||{
        loop {
          let nv = self.limbs[i] + delta;
          self.limbs[i] = nv & LIMB_MASK;
          ASO::check_underflow(self, i, nv)?;
          if nv < LIMB_MODULUS { return Some(()) }
          ASO::check_nospace(i)?;
          i -= 1;
          delta = ASO::CARRY_DELTA;
        }
      })() == Some(()) { break 'attempt }

      // undo
      loop {
        if i >= self.limbs.len() { break }
        else if i == aso.start_limb(self) { delta = ASO::final_undo_delta(); }
        let nv = self.limbs[i] - delta;
        self.limbs[i] = nv & LIMB_MASK;
        i += 1;
      }
      self.limbs.push(ASO::NEW_LIMBS);
      self.limbs.push(ASO::NEW_LIMBS);
    }
    self.repack()?
  }

  #[throws(Overflow)]
  pub fn increment(&mut self) -> ZCoord { self.addsub(&Increment)? }
  #[throws(Overflow)]
  pub fn decrement(&mut self) -> ZCoord { self.addsub(&Decrement)? }

  #[throws(Overflow)]
  pub fn repack(&self) -> ZCoord {
    let mut limbs = self.limbs.as_slice();
    while let Some(l) = limbs.strip_suffix(&[ZERO]) { limbs = l }

    let taillen = (limbs.len() * TEXT_PER_LIMB - 1).try_into()?;
    let mut bf = ZCoord::alloc(taillen);
    let mut w = bf.tail_mut();

    for l in limbs.iter().cloned() {
      if l >= LIMB_MODULUS { throw!(Overflow) };
      l.to_str_buf((&mut w[0..DIGITS_PER_LIMB]).try_into().unwrap());
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

pub type RangeIterator = std::iter::Take<
    IteratorCore<AddSubRangeDelta, MutateFirst>
    >;

pub trait MutateReturn {
  fn op<T, U,
        M: FnOnce(&mut T),
        O: FnOnce(&T) -> U>
    (x: &mut T,
     m: M,
     o: O) -> U;
}

#[derive(Debug)]
pub struct MutateFirst;
impl MutateReturn for MutateFirst {
  fn op<T, U,
        M: FnOnce(&mut T),
        O: FnOnce(&T) -> U>
    (x: &mut T, m: M, o: O) -> U
  {
    m(x);
    o(x)
  }
}

#[derive(Debug)]
pub struct MutateLast;
impl MutateReturn for MutateLast {
  fn op<T, U,
        M: FnOnce(&mut T),
        O: FnOnce(&T) -> U>
    (x: &mut T, m: M, o: O) -> U
  {
    let u = o(x);
    m(x);
    u
  }
}

#[derive(Debug)]
pub struct IteratorCore<ASO, MR> {
  current: Mutable,
  aso: ASO,
  mr: MR,
}

#[derive(Debug)]
pub struct AddSubRangeDelta {
  i: usize,
  step: LimbVal,
}
impl AddSubOffset for AddSubRangeDelta {
  fn init_delta(&self) -> LimbVal { self.step }
  const CARRY_DELTA : LimbVal = ONE;
  const NEW_LIMBS   : LimbVal = ZERO;
  #[throws(as Option)]
  fn check_underflow(_: &Mutable, _: usize, _: LimbVal) { }
  #[throws(as Option)]
  fn check_nospace(i: usize) { assert_ne!(i, 0) }
  fn start_limb(&self, _: &Mutable) -> usize { self.i }
  fn final_undo_delta() -> LimbVal { panic!() }
  const SEALED_TRAIT: Sealed = Sealed(());
}

impl Mutable {
  fn limb_val_lookup(&self, i: usize) -> LimbVal {
    *self.limbs.get(i).unwrap_or(&ZERO)
  }
  fn extend_to_limb(&mut self, i: usize) {
    if self.limbs.len() < i {
      self.limbs.resize(i+1, ZERO);
    }
  }

  #[throws(RangeImpossible)]
  fn range_core(a: &Mutable, b: &Mutable, count: RangeCount)
                -> (Mutable, AddSubRangeDelta) {
    type ASRD = AddSubRangeDelta;
    let count = count as RawLimbVal;
    let mut current = a.clone();
    let mut borrowing = false;
    let aso = 'ok: loop { for i in 0.. {
      if i >= a.limbs.len() && i >= b.limbs.len() {
	// Oh actually these numbers are equal!
	throw!(RangeImpossible::Empty);
      }
      current.extend_to_limb(i);

      let la = a.limb_val_lookup(i);
      let lb = b.limb_val_lookup(i);
      if la == lb { continue }

      let wantgaps = count+1;
      let avail = (lb.primitive() as i64) - (la.primitive() as i64)
        + if borrowing { RAW_LIMB_MODULUS as i64 } else { 0};
      if avail < 0 { throw!(RangeImpossible::Backwards) }
      let avail = avail as u64;

      if avail < 2 {
        // Only 1 difference in this limb and the next.  We are
        // goint to have to borrow, and, later, add with carry.
        borrowing = true;
        continue;
      }
      // avail might be up to 2x limb range

      let mut i = i;
      let (step, init);
      if avail >= wantgaps {
        // Evenly divide intervening values for this, the first
        // differeing limb
	step = avail / wantgaps;
        // ^ if count==0, wantgaps==1 and step is perhaps still too large,
        //   but in that case our next() will never be called, so fine
        init = la;
      } else {
        // Not enough space here, but we can pick a unique value for
        // this limb, and divide the next limb evenly
        current.limbs[i] = la + (avail/2).into();
	i += 1;
	step = (RAW_LIMB_MODULUS-1) / wantgaps;
        init = ZERO;
      }
      current.extend_to_limb(i);
      current.limbs[i] = init;
      break 'ok ASRD { i, step: step.into() };
    } };
    (current, aso)
  }

  #[throws(RangeImpossible)]
  /// Iterator producing a half-open range `[self, other>`
  ///
  /// Produces precisely `count` items.
  pub fn range_upto(&self, other: &Mutable, count: RangeCount)
                    -> RangeIterator {
    let (current, aso) = Mutable::range_core(self, other, count)?;
    IteratorCore { current, aso, mr: MutateFirst }.take(count as usize)
  }
}

impl<ASO:AddSubOffset, MR:MutateReturn> Iterator for IteratorCore<ASO, MR> {
  type Item = ZCoord;
  fn next(&mut self) -> Option<ZCoord> {
    let aso = &self.aso;
    Some(MR::op(
      &mut self.current,
      |current| { current.addsub(aso).unwrap(); },
      |current| { current.repack().unwrap() },
    ))
  }
}

pub trait BoxedIteratorTrait: Iterator<Item = ZCoord> + Debug { }
pub type BoxedIterator = Box<dyn BoxedIteratorTrait>;
impl<T> BoxedIteratorTrait for T where T: Iterator<Item = ZCoord> + Debug {}

impl Mutable {
  /// Iterator producing `<self, ..>`
  pub fn iter<ASO:AddSubOffset>(self, aso: ASO)
                                -> IteratorCore<ASO, impl MutateReturn + Debug>
  {
    IteratorCore { current: self, aso, mr: MutateFirst }
  }
  #[throws(LogicError)]
  /// Iterator producing a half-open range, `[a, b)`
  pub fn some_range(a: Option<&Mutable>, b: Option<&Mutable>,
                    count: RangeCount) -> BoxedIterator {
    fn mk<T:'static + Debug + Iterator<Item=ZCoord>>(x: T) -> BoxedIterator
    { Box::new(x) }
    let c = count as usize;
    if c == 0 { return mk( iter::empty() ) }
    match (a, b) {
      (None,    None   ) => throw!(TotallyUnboundedRange),
      (Some(a), None   ) => mk( a.clone().iter(Increment).take(c) ),
      (Some(a), Some(b)) => mk( Mutable::range_upto(&a,&b,count)? ),
      (None,    Some(b)) => mk({
        let mut first = b.clone();
        first.addsub(&Decrement).unwrap();
        let (current, aso) = Mutable::range_core(&first, &b, count-1)?;
        IteratorCore { current, aso, mr: MutateLast }.take(c)
      }),
    }
  }

  #[throws(ParseError)]
  pub fn from_str(s: &str) -> Mutable {
    let tail = ZCoord::checked(s)?;
    Mutable::from_u8_unchecked(tail)
  }
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
    write!(f, r#"Zc""#)?;
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
impl Eq for ZCoord { }
impl PartialEq for ZCoord {
  fn eq(&self, other: &ZCoord) -> bool {
    self.cmp(other) == Ordering::Equal
  }
}

impl TryFrom<&str> for ZCoord {
  type Error = ParseError;
  #[throws(ParseError)]
  fn try_from(s: &str) -> ZCoord { ZCoord::from_str(s)? }
}

impl FromStr for ZCoord {
  type Err = ParseError;
  #[throws(ParseError)]
  fn from_str(s: &str) -> ZCoord { ZCoord::from_str(s)? }
}

//---------- construction of ZCoord contents ---------
//
// We can panic if this code is buggy, but not compromise safety.

const DEFAULT_TEXT: &[u8] = b"g000000000";

impl Default for ZCoord {
  fn default() -> ZCoord {
    ZCoord::alloc_copy(DEFAULT_TEXT).unwrap()
  }
}

impl ZCoord {
  #[throws(ParseError)]
  fn checked(s: &str) -> &[u8] {
    let s = s.as_bytes();
    let nomlen = s.len() + 1;
    if nomlen % TEXT_PER_LIMB !=0 { throw!(ParseError) }
    let _: innards::Taillen = (nomlen / TEXT_PER_LIMB).try_into()
      .map_err(|_:TryFromIntError| ParseError)?;
    for lt in s.chunks(TEXT_PER_LIMB) {
      if !lt[0..DIGITS_PER_LIMB].iter().all(
        |c: &u8| {
          (b'0'..=b'9').contains(&c) ||
          (b'a'..=b'v').contains(&c)
        }) { throw!(ParseError) }
      match lt[DIGITS_PER_LIMB..] {
        [] | [b'_'] => (),
        _ => throw!(ParseError)
      };
    }
    if &s[s.len() - DIGITS_PER_LIMB..] == b"0000000000" {
      throw!(ParseError)
    }
    s
  }

  #[throws(ParseError)]
  pub fn check_str(s: &str) {
    Self::checked(s)?;
  }

  #[throws(ParseError)]
  pub fn from_str(s: &str) -> Self {
    let tail = ZCoord::checked(s)?;
    ZCoord::alloc_copy(tail).unwrap()
  }
}

impl TryFrom<&Mutable> for ZCoord {
  type Error = Overflow;
  #[throws(Overflow)]
  fn try_from(m: &Mutable) -> ZCoord { m.repack()? }
}

//---------- innards, unsafe ----------

mod innards {
  use std::alloc::{self, Layout};
  use std::mem::{self, align_of, size_of};
  use std::ptr::{self, NonNull};
  use std::slice;
  use super::*;

  unsafe impl Send for ZCoord { }
  unsafe impl Sync for ZCoord { }

  pub(super)
  type Innards = NonNull<u8>;
  pub type Taillen = u16;

  pub(super)
  struct Header {
    pub taillen: u16, // in characters
  }

  #[repr(C)]
  #[allow(dead_code)] // this is for documentation purposes
  struct Repr {
    h: Header,
    d: [Tail1],
  }

  const OFFSET: usize = {
    let h_size = size_of::<Header>();
    let l_align = align_of::<Tail1>();
    l_align * ((h_size + l_align - 1) / l_align)
  };

  fn layout(len: Taillen) -> (usize, Layout) {
    let tail_nbytes: usize = size_of::<Tail1>() * (len as usize);
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
  
    pub(super)
    fn alloc(taillen: Taillen) -> ZCoord {
      unsafe {
        ZCoord::alloc_unsafe(taillen, |nt: *mut Tail1| {
          ptr::write_bytes(nt, 0, taillen as usize);
        })
      }
    }

    #[throws(Overflow)]
    pub(super)
    fn alloc_copy(tail: &[Tail1]) -> ZCoord {
      let taillen = tail.len().try_into()?;
      unsafe {
        ZCoord::alloc_unsafe(taillen, |nt: *mut Tail1| {
          ptr::copy_nonoverlapping(tail.as_ptr(), nt, taillen as usize);
        })
      }
    }

    pub(super) fn tail(&self) -> &[Tail1] {
      unsafe {
        let (h, t) = ptrs(self.0.as_ptr());
        let h = h.as_ref().unwrap();
        slice::from_raw_parts(t, h.taillen as usize)
      }
    }

    pub(super)
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

    #[throws(Overflow)]
    pub fn plus_offset(&self, offset: u32) -> Self { unsafe {
      let (old_header, old_tail) = ptrs(self.0.as_ptr());
      let old_taillen = old_header.as_ref().unwrap().taillen;
      let new_taillen = old_taillen
        .checked_add(TEXT_PER_LIMB as Taillen).ok_or(Overflow)?;
      let old_taillen: usize = old_taillen.into();
      let new_limb = lv(
        (offset as RawLimbVal) << (BITS_PER_LIMB - 32) |
         1                     << (BITS_PER_LIMB - 33)
      );
      let mut buf: [u8; TEXT_PER_LIMB] = default();
      buf[0] = b'_';
      new_limb.to_str_buf((&mut buf[1..TEXT_PER_LIMB]).try_into().unwrap());
      ZCoord::alloc_unsafe(new_taillen, |new_tail| {
        ptr::copy_nonoverlapping(old_tail,
                                 new_tail,
                                 old_taillen);

        ptr::copy_nonoverlapping(buf.as_ptr(),
                                 new_tail.add(old_taillen),
                                 TEXT_PER_LIMB);
      })
    } }
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

  impl Hash for ZCoord {
    fn hash<H:Hasher>(&self, state: &mut H) {
      self.tail().hash(state)
    }
  }

}

//---------- tests ----------

#[cfg(test)]
mod test {
  use crate::misc::default;
  use super::*;
  use std::collections::hash_map::DefaultHasher;
  use std::mem;

  fn bf(s: &str) -> ZCoord { ZCoord::from_str(s).unwrap() }
  fn mk(s: &str) -> super::Mutable { bf(s).clone_mut() }

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
               format!(r#"Zc"{}""#, &b2));
    fn bad(s: &str) { assert_eq!(Err(ParseError), ZCoord::from_str(s)); }
    bad("");
    bad("0");
    bad("0000000000");
    bad("1111111111_0000000000");
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
  fn limb_debug() {
    fn chk(raw: RawLimbVal, disp: &str) {
      let l: LimbVal = raw.into();
      let dbg = format!("lv({})", &disp);
      assert_eq!( &format!("{}",   &l), disp );
      assert_eq!( &format!("{:?}", &l), &dbg );
    }
    chk(0x42, "0000000022");
    chk(0x42 + RAW_LIMB_MODULUS *   0x33,   "0x33_!_0000000022");
    chk(0x42 + RAW_LIMB_MODULUS * 0x3fae, "0x3fae_!_0000000022");
  }

  #[test]
  fn inequality() {
    assert!( bf("gg0123abcd_0123456789") <
             bf("gg0123abcd_012345678a") );
    
    assert!( bf("gg0123abcd") <
             bf("gg0123abcd_012345678a") );
  }

  #[test]
  fn incdec() {
    use core::cmp::Ordering::{Greater,Less};
    impl Mutable {
      fn tincdec<ASO:AddSubOffset>(mut self, exp: &str, aso: ASO,
                                   exp_ord: Ordering) -> Self {
        let before = self.repack().unwrap();
        let got = self.addsub(&aso).unwrap();
        assert_eq!(got.to_string(), exp);
        assert_eq!(got.cmp(&before), exp_ord);

        fn h(z: &ZCoord) -> u64 {
          let mut h = DefaultHasher::new();
          z.hash(&mut h);
          h.finish()
        }
        assert_ne!(h(&got), h(&before));
        self
      }
      fn tinc(self, e: &str) -> Self { self.tincdec(e, Increment, Greater) }
      fn tdec(self, e: &str) -> Self { self.tincdec(e, Decrement, Less)    }
    }
    let start: ZCoord = default();
    assert_eq!(format!("{}", &start), "g000000000");
    start.clone_mut()
      .tinc("g001000000");
    start.clone_mut()
      .tdec("fvvv000000");

    mk("000000000a")
      .tinc("000100000a")
      .tinc("000200000a")
      .tdec("000100000a")
      .tdec("000000000a")
      .tdec("0000000009_vvvvvvvvvv_vvvuvvvvvv")
      .tdec("0000000009_vvvvvvvvvv_vvvtvvvvvv")
      ;
    mk("vvvvvvvvvv")
      .tinc("vvvvvvvvvv_0000000000_0001000000")
      .tdec("vvvvvvvvvv")
      ;
    mk("vvvvvvvvvv_vvvvvvvvvv_vvvvv01234")
      .tinc("vvvvvvvvvv_vvvvvvvvvv_vvvvv01234_0000000000_0001000000")
      ;
    mk("0000000000_0000000000_0001012340")
      .tdec("0000000000_0000000000_0000012340")
      .tdec("0000000000_0000000000_000001233v_vvvvvvvvvv_vvvuvvvvvv")
      ;

    mk("vvvvvvvvvv")
      .tinc("vvvvvvvvvv_0000000000_0001000000")
      ;
    mk("vvvvvvvvvv_vvvvvvvvvv_vvvvv01234")
      .tinc("vvvvvvvvvv_vvvvvvvvvv_vvvvv01234_0000000000_0001000000")
      ;

    assert_eq!( Mutable::some_range(Some(&mk("0200000000")),
                                    Some(&mk("0100000000")),
                                    1).unwrap_err(),
                LogicError::from(RangeImpossible::Backwards) );

    assert_eq!( Mutable::some_range(Some(&mk("0200000000")),
                                    Some(&mk("0200000000")),
                                    1).unwrap_err(),
                LogicError::from(RangeImpossible::Empty) );
  }

  #[test]
  fn iter() {
    let mut m = mk("000000000a").iter(Increment);
    assert_eq!( m.next(), Some(bf("000100000a")) );
    assert_eq!( m.next(), Some(bf("000200000a")) );

    let mut m = mk("000000000a").iter(Decrement);
    assert_eq!( m.next(), Some(bf("0000000009_vvvvvvvvvv_vvvuvvvvvv")) );
    assert_eq!( m.next(), Some(bf("0000000009_vvvvvvvvvv_vvvtvvvvvv")) );
  }

  #[test]
  fn range() {
    struct It {
      i: RangeIterator,
      last: ZCoord,
    }
    impl It {
      fn nxt(&mut self, exp: &str) {
        let got = self.i.next().unwrap();
        assert_eq!(got.to_string(), exp);
        assert_eq!(got, bf(exp));
        assert!(got > self.last);
        self.last = got.clone();
      }
    }
    let x = bf("3333333333_vvvvvvvvv0").clone_mut();
    let y = bf("3333333334_0000000040").clone_mut();
    let i = x.range_upto(&y, 4).unwrap();
    let mut it = It { i, last: x.repack().unwrap() };
    it.nxt("3333333334");
    it.nxt("3333333334_0000000010");
    it.nxt("3333333334_0000000020");
    it.nxt("3333333334_0000000030");
    assert_eq!(it.i.next(), None);

    let x = bf("1000000000").clone_mut();
    let y = bf("2000000000").clone_mut();
    let i = x.range_upto(&y, 3).unwrap();
    let mut it = It { i, last: x.repack().unwrap() };
    it.nxt("1800000000");
    it.nxt("1g00000000");
    it.nxt("1o00000000");
    assert_eq!(it.i.next(), None);
  }

  #[test]
  fn some_range() {
    struct It {
      i: BoxedIterator,
      last: Option<ZCoord>,
    }
    #[throws(LogicError)]
    fn mkr(a: Option<&str>, b: Option<&str>, count: RangeCount) -> It {
      let a = a.map(|s: &str| s.parse::<ZCoord>().unwrap().clone_mut());
      let b = b.map(|s: &str| s.parse::<ZCoord>().unwrap().clone_mut());
      let last = a.as_ref().map(|m| m.repack().unwrap());
      let i = Mutable::some_range(a.as_ref(), b.as_ref(), count)?;
      It { i, last }
    }
    impl It {
      fn nxt(&mut self, exp: Option<&str>) {
        let got = self.i.next();
        let got_s = got.as_ref().map(|s| s.to_string());
        let got_s = got_s.as_ref().map(|s| s.as_str());
        assert_eq!(got_s, exp);
        if let (Some(got), Some(exp)) = (&got, &exp) {
          assert_eq!(got, &bf(exp));
          if let Some(last) = &self.last { assert!(got > last); }
        }
        self.last = got.clone();
      }
    }
    let mut it = mkr(Some("3000000000"),Some("4000000000"),3).unwrap();
    it.nxt(Some("3800000000"));
    it.nxt(Some("3g00000000"));
    it.nxt(Some("3o00000000"));
    it.nxt(None);
    let mut it = mkr(Some("3000000000"),Some("4000000000"),1).unwrap();
    it.nxt(Some("3g00000000"));
    it.nxt(None);
    let mut it = mkr(None, Some("4000000000"),2).unwrap();
    it.nxt(Some("3vvv000000"));
    it.nxt(Some("3vvvg00000"));
    it.nxt(None);
    let mut it = mkr(Some("4000000000"),None,2).unwrap();
    it.nxt(Some("4001000000"));
    it.nxt(Some("4002000000"));
    it.nxt(None);
    let x = mkr(None,None,2);
    assert_eq!( x.err(), Some(
      LogicError::RangeTotallyUnbounded(TotallyUnboundedRange)
    ));
    let mut it = mkr(Some("fvvq000000"),Some("g026000000"),1).unwrap();
    it.nxt(Some("g010000000"));
    it.nxt(None);
    let mut it = mkr(None,Some("fvvq000000"),0).unwrap();
    it.nxt(None);
  }

  #[test]
  fn plus_offset() {
    fn chk(off: u32, s: &str) {
      let z: ZCoord = "3o00000000".parse().unwrap();
      let p = z.plus_offset(off).unwrap();
      assert_eq!(s, format!("{}", &p));
      assert_eq!(p, s.parse().unwrap());
    }

    chk(         0, "3o00000000_0000004000");
    chk(         1, "3o00000000_000000c000");
    chk(0xffffffff, "3o00000000_vvvvvvs000");
  }
}
