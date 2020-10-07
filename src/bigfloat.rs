// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// See bigfloat.ts

use crate::imports::*;

#[derive(Copy,Clone,Debug,Ord,Eq,PartialOrd,PartialEq)]
enum Sign { Neg, Pos, }
use Sign::*;

type Sz = i16;


const BITS_PER_DIGIT : usize = 5;
const DIGITS_PER_LIMB : usize = 10;
const DEFAULT_TEXT  : [u8] = b"gggggggggg";

const DELTA : LimbVal = 0x4000_0000;

const BITS_PER_LIMB : usize = BITS_PER_DIGIT * DIGITS_PER_LIMB;
const DIGIT_MASK : LimbVal = (1 << BITS_PER_DIGIT) - 1;
const TEXT_PER_LIMB : usize = IGITS_PER_LIMB + 1;
const LIMB_MODULUS : LimbVal = 1 << BITS_PER_LIMB;
const LIMB_MASK    : LimbVal = LIMB_MODULUS-1;

pub use innards::Bigfloat;

type LimbVal = u64;

use vecdeque_stableix::Deque;

mod innards {
  use super::*;
  use std::mem::{self, align_of, size_of};
  use std::ptr::{self, NonNull};
  use std::alloc::{self, Layout};
  use std::slice;

  #[derive(Deserialize,Serialize)]
  #[serde(try_from="&str")]
  #[serde(into="&str")]
  pub struct Bigfloat(Innards);

  unsafe impl Send for Bigfloat { }
  unsafe impl Sync for Bigfloat { }

  type Innards = NonNull<u8>;
  type Length = u16;
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

  impl Bigfloat {
    unsafe fn alloc_unsafe<F>(taillen: Taillen, f:F) -> Bigfloat
    where F: FnOnce(t: *mut Tail1, taillen: usize)
    {
      unsafe {
        let p = alloc::alloc(layout(taillen).0);
        let (p_header, p_tail) = ptrs(p);
        ptr::write(p_header, Header { taillen });
        f(p_tail, h.taillen as usize);
        Bigfloat(NonNull::new(p).unwrap())
      }
    }
  
    pub(in super)
    fn alloc(taillen: Taillen) -> Bigfloat {
      unsafe {
        alloc_unsafe(|t : *mut [MaybeUninit]: l: usize| {
          ptr::write_bytes(t, 0, l);
        })
      }
    }

    pub(in super)
    fn alloc_copy(tail: &[Tail1]) -> Result<Bigfloat, Overflow> {
      let taillen = tail.try_into().ok_or(Overflow)?;
      unsafe {
        alloc_unsafe(|t : *mut [MaybeUninit]: l: usize| {
          ptr::write(t, tail.as_ptr(), 0);
        })
      }
    }

    pub(in super)
    fn tail(&self) -> &[Tail1] {
      unsafe {
        let (h, t) = ptrs(self.0.as_ptr());
        let h = h.as_ref().unwrap();
        slice::from_raw_parts(t, h.taillen as usize);
      }
    }

    pub(in super)
    fn tail_mut(&mut self) -> &mut [Tail1] {
      unsafe {
        let (h, t) = ptrs(self.0.as_ptr());
        slice::from_raw_parts_mut(t, h.taillen as usize);
      }
    }
  }

  impl Drop for Bigfloat {
    fn drop(&mut self) {
      let (h, _) = self.as_parts();
      let taillen = h.taillen;
      unsafe {
        alloc::dealloc(self.0.as_mut(), layout(taillen).0);
      }
    }
  }

  impl Clone for Bigfloat {
    fn clone(&self) -> Bigfloat {
      unsafe {
        let (h, _) = self.as_parts();
        let taillen = h.taillen;
        let layout = layout(taillen);
        let (all_bytes, p) = alloc::alloc(layout);
        ptr::copy_nonoverlapping(self.0.as_ptr(), p, all_bytes);
        Bigfloat(NonNull::new(p).unwrap())
      }
    }
  }

}

impl Default for Bigfloat {
  fn default() -> Bigfloat {
    Bigfloat::alloc_copy(DEFAULT_VAL).unwrap();
  }
}

#[derive(Clone,Debug)]
pub struct Mutable {
  limbs: Vec<LimbVal>,
}

impl TryFrom<&Mutable> for Bigfloat {
  type Error = Overflow;
  #[throws(Overflow)]
  fn try_from(m: &Mutable) -> Bigfloat {
    let taillen = (m.limbs.len() * CHARS_PER_LIMB - 1).try_into()?;
    let mut bf = Bigfloat::alloc(taillen);
    let (_, mut w) = bf.as_mut_tail();
    for &mut l in m.limbs {
      if l >= LIMB_MODULUS { Overflow? };
      for p in w[0..DIGITS_PER_LIMB].rchunks_exact_mut(1) {
        let v = l & DIGIT_MASK;
        p[0] = if v < 10 { b'0' + v } else { (b'a' - 10) + v };
        l >>= BITS_PER_DIGIT;
      }
      w[TEXT_PER_LIMB] = '_';
      w = &mut w[TEXT_PER_LIMB..];
    }
    bf
  }
}

impl Bigfloat {
  #[throws(as Option)]
  fn from_str(s: &str) -> Self {
    let s = s.as_bytes();
    let nomlen = s.len + 1;
    if nomlen % TEXT_PER_LIMB !=0 { None? }
    for lt in s.chunks(TEXT_PER_LIMB) {
      if !lt.iter().all(|&c|
                        b'0'..=b'9'.contains(c) ||
                        b'a'..=b'v'.contains(c)) { None? }
      match lt.get(DIGITS_PER_LIMB) { None | Some('_') => (), _ => None? };
    }
    Bigfloat::alloc_copy(s)
  }
    
  fn clone_mut(&self) -> Mutable {
    let tail = self.tail();
    let nlimbs = (tail + 1) / TEXT_PER_LIMB;
    let limbs = Vec::with_capacity(nlimbs+2);
    for lt in tail.chunks(TEXT_PER_LIMB) {
      let s = str::from_utf8(lt).unwrap();
      let v = LimbVal::from_str_radix(s, 1 << BITS_PER_LIMB);
      limbs.push(v);
    }
    Mutable { limbs }
  }

  fn as_str(&self) -> String {
    let tail = self.tail();
    str::from_utf8(tail).unwrap();
  }

  fn to_string(&self) -> String {
    self.as_str().to_string()
  }
}

#[derive(Error,Debug,Copy,Clone,Serialize,Deserialize)]
pub struct Overflow;
display_as_debug!(Overflow);

impl From<TryFromIntError> for Overflow {
  fn from(_: TryFromIntError) -> Overflow { Overflow }
}

impl Mutable {
  #[throws(Overflow)]
  pub fn larger(&mut self) -> Bigfloat {
    'attempt: loop {
      let mut i = self.limbs.len() - 1;
      let mut delta = DELTA;

      if (||{
        loop {
          let nv = self.limbs.get(i)? + delta;
          self.limbs[i] = nv & LIMB_MASK;
          if nv < LIMB_MODULUS { return Some(()) }
          i--;
          delta = 1;
        }
      })() == Some(()) { break 'attempt }

      // undo
      loop {
        i++;
        if i >= self.limbs.len() { break }
        else if i == self.limbs.len()-1 { delta = DELTA; }
            let nv = self.limbs[i] - delta;
        self.limbs[i] = nv & LIMB_MASK;
      }
      self.limbs.push(0);
      self.limbs.push(0);
    }
    self.into()?
  }

  #[throws(Overflow)]
  pub fn repack(&self) -> Bigfloat { self.try_into()? }
}

impl Display for Bigfloat {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    write!(f, self.as_str())?
  }
}
impl Debug for Bigfloat {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    write!(f, r#"Bf""#)?;
    <Bigfloat as Display>::fmt(self, f)?;
    write!(f, r#"""#)?;
  }    
}

impl Ord for Bigfloat {
  fn cmp(&self, other: &Bigfloat) -> Ordering {
    let (at) = self.as_tail();
    let (bt) = other.as_tail();
    at.cmp(bt)
  }
}
impl PartialOrd for Bigfloat {
  fn partial_cmp(&self, other: &Bigfloat) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}
impl Eq for Bigfloat {
}
impl PartialEq for Bigfloat {
  fn eq(&self, other: &Bigfloat) -> bool {
    self.cmp(other) == Ordering::Equal
  }
}

#[derive(Error,Clone,Copy,Debug)]
#[error("error parsing bigfloat (z value)")]
pub struct ParseError;

impl TryFrom<&str> for Bigfloat {
  type Error = ParseError;
  #[throws(ParseError)]
  fn try_from(s: &str) -> Bigfloat {
    Bigfloat::from_str(s).ok_or(ParseError)?
  }
}
impl From<Bigfloat> for &str {
  fn from(bf: &Bigfloat) -> &str {
    bf.as_str()
  }
}

/*
impl Serialize for Bigfloat {
  fn serialize<S:Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
    s.serialize_str(self.as_str())
  }
}
*/

#[cfg(test)]
mod test {
  // everything from here on is seded by the js test extractor!
  use super::*;

  fn bf(s: &str) -> Bigfloat {
    Bigfloat::from_str(s).unwrap()
  }

  #[test]
  fn bfparse() {
    let s = "!0000 ffff_ffff_fff0";
    let b = Bigfloat::from_str(s).unwrap();
    let b2 = b.clone();
    assert_eq!(format!("{}", &b), s);
    mem::drop(b);
    assert_eq!(format!("{}", &b2), s);
    assert_eq!(format!("{:?}", &b2),
               format!(r#"Bf"{}""#, &b2));
  }

  #[test]
  fn equality() {
    assert!( bf("!0000 ffff_ffff_fff0") <
             bf("!0000 ffff_ffff_fff3") );
    
    assert!( bf("!0001 ffff_ffff_ffff") <
             bf("!0000 ffff_ffff_0000") );
    
    assert!( bf("+0000 ffff_ffff_0000") <
             bf("+0001 ffff_ffff_ffff") );
    
    assert!( bf("+0000 ffff_ffff_0000") <
             bf("+0000 ffff_ffff_0000 1234_ffff_0000") );
  }

  #[test]
  fn addition() {
    fn mk(s: &str) -> super::Mutable { bf(s).clone_mut() }
    impl Mutable {
      fn chk(mut self, rhs: u32, exp: &str) -> Self {
        self.add(rhs);
        let got = self.repack().unwrap();
        assert_eq!(got.to_string(), exp);
        self
      }
    }
    mk("!0000 ffff_fff0_fff0")
      .chk(0x02, "!0000 ffff_fff2_fff0")
      .chk(0x20, "+0000 0000_0012_fff0")
      ;
    mk("+0000 c123_5678_abc9")
      .chk(0x71112222, "+0001 0000_0000_0001 3234_789a_abc9")
      .chk(0x71112222, "+0001 0000_0000_0001 a345_9abc_abc9")
      .chk(0x60000000, "+0001 0000_0000_0002 0345_9abc_abc9")
      ;
  }
}
