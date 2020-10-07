// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// See bigfloat.ts

use crate::imports::*;

const BITS_PER_DIGIT : usize = 5;
const DIGITS_PER_LIMB : usize = 10;
const DEFAULT_TEXT  : &[u8] = b"gggggggggg";

const DELTA : LimbVal = 0x4000_0000;

const BITS_PER_LIMB : usize = BITS_PER_DIGIT * DIGITS_PER_LIMB;
const DIGIT_MASK : LimbVal = (1 << BITS_PER_DIGIT) - 1;
const TEXT_PER_LIMB : usize = DIGITS_PER_LIMB + 1;
const LIMB_MODULUS : LimbVal = 1 << BITS_PER_LIMB;
const LIMB_MASK    : LimbVal = LIMB_MODULUS-1;

#[derive(Deserialize)]
#[serde(try_from="&str")]
pub struct Bigfloat(innards::Innards);

type LimbVal = u64;

mod innards {
  use super::*;
  use std::mem::{self, align_of, size_of};
  use std::ptr::{self, NonNull};
  use std::alloc::{self, Layout};
  use std::slice;

  unsafe impl Send for Bigfloat { }
  unsafe impl Sync for Bigfloat { }

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

  impl Bigfloat {
    unsafe fn alloc_unsafe<F>(taillen: Taillen, f:F) -> Bigfloat
    where F: FnOnce(*mut Tail1)
    {
      #[allow(unused_unsafe)] // unsafe block in unsafe fn
      unsafe {
        let p = alloc::alloc(layout(taillen).1);
        let (p_header, p_tail) = ptrs(p);
        ptr::write(p_header, Header { taillen });
        f(p_tail);
        Bigfloat(NonNull::new(p).unwrap())
      }
    }
  
    pub(in super)
    fn alloc(taillen: Taillen) -> Bigfloat {
      unsafe {
        Bigfloat::alloc_unsafe(taillen, |nt : *mut Tail1| {
          ptr::write_bytes(nt, 0, taillen as usize);
        })
      }
    }

    #[throws(Overflow)]
    pub(in super)
    fn alloc_copy(tail: &[Tail1]) -> Bigfloat {
      let taillen = tail.len().try_into()?;
      unsafe {
        Bigfloat::alloc_unsafe(taillen, |nt : *mut Tail1| {
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

  impl Drop for Bigfloat {
    fn drop(&mut self) {
      let layout = self.layout().1;
      unsafe {
        alloc::dealloc(self.0.as_mut(), layout);
      }
    }
  }

  impl Clone for Bigfloat {
    fn clone(&self) -> Bigfloat {
      let (all_bytes, layout) = self.layout();
      unsafe {
        let p = alloc::alloc(layout);
        ptr::copy_nonoverlapping(self.0.as_ptr(), p, all_bytes);
        Bigfloat(NonNull::new(p).unwrap())
      }
    }
  }

}

impl Default for Bigfloat {
  fn default() -> Bigfloat {
    Bigfloat::alloc_copy(DEFAULT_TEXT).unwrap()
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
    let taillen = (m.limbs.len() * TEXT_PER_LIMB - 1).try_into()?;
    let mut bf = Bigfloat::alloc(taillen);
    let mut w = bf.tail_mut();
    for mut l in m.limbs.iter().cloned() {
      if l >= LIMB_MODULUS { throw!(Overflow) };
      for p in w[0..DIGITS_PER_LIMB].rchunks_exact_mut(1) {
        let v = (l & DIGIT_MASK) as u8;
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

impl Bigfloat {
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
    Bigfloat::alloc_copy(s).ok()?
  }

  pub fn clone_mut(&self) -> Mutable {
    let tail = self.tail();
    let nlimbs = (tail.len() + 1) / TEXT_PER_LIMB;
    let mut limbs = Vec::with_capacity(nlimbs+2);
    for lt in tail.chunks(TEXT_PER_LIMB) {
      let s = str::from_utf8(lt).unwrap();
      let v = LimbVal::from_str_radix(s, 1 << BITS_PER_DIGIT).unwrap();
      limbs.push(v);
    }
    Mutable { limbs }
  }

  pub fn as_str(&self) -> &str {
    let tail = self.tail();
    str::from_utf8(tail).unwrap()
  }

  pub fn to_string(&self) -> String {
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
  pub fn increment(&mut self) -> Bigfloat {
    'attempt: loop {
      let mut i = self.limbs.len() - 1;
      let mut delta = DELTA;

      if (||{
        loop {
          let nv = self.limbs[i] + delta;
          self.limbs[i] = nv & LIMB_MASK;
          if nv < LIMB_MODULUS { return Some(()) }
          if i == 0 { return None }
          i -= 1;
          delta = 1;
        }
      })() == Some(()) { break 'attempt }

      // undo
      loop {
        if i >= self.limbs.len() { break }
        else if i == self.limbs.len()-1 { delta = DELTA; }
            let nv = self.limbs[i] - delta;
        self.limbs[i] = nv & LIMB_MASK;
        i += 1;
      }
      self.limbs.push(0);
      self.limbs.push(0);
    }
    self.repack()?
  }

  #[throws(Overflow)]
  pub fn repack(&self) -> Bigfloat { self.try_into()? }
}

impl Display for Bigfloat {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    write!(f, "{}", self.as_str())?
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
    let at = self.tail();
    let bt = other.tail();
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

impl Serialize for Bigfloat {
  fn serialize<S:Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
    s.serialize_str(self.as_str())
  }
}

#[cfg(test)]
mod test {
  // everything from here on is seded by the js test extractor!
  use super::*;

  fn bf(s: &str) -> Bigfloat {
    Bigfloat::from_str(s).unwrap()
  }

  #[test]
  fn bfparse() {
    let s = "gg0123abcd_0123456789";
    let b = Bigfloat::from_str(s).unwrap();
    let b2 = b.clone();
    assert_eq!(format!("{}", &b), s);
    mem::drop(b);
    assert_eq!(format!("{}", &b2), s);
    assert_eq!(format!("{:?}", &b2),
               format!(r#"Bf"{}""#, &b2));
    fn bad(s: &str) { assert_eq!(None, Bigfloat::from_str(s)); }
    bad("");
    bad("0");
    bad("000000000_0000000000");
    bad("0000000000_000000000");
    bad("#000000000_0000000000");
    bad("000000000#_0000000000");
    bad("0000000000#0000000000");
    bad("0000000000_000000000#");
    bad("Z000000000_#000000000");
    bad("A000000000_#000000000");
    bad("w000000000_#000000000");
    bad("/000000000_#000000000");
    bad(":000000000_#000000000");
    bad("`000000000_#000000000");
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
    }/*
    mk("000000000a")
      .tinc("000100000a")
      .tinc("000200000a")
      ;*/
    mk("vvvvvvvvvv")
      .tinc("vvvvvvvvvv_0000000000_0001000000")
      ;
    mk("vvvvvvvvvv_vvvvvvvvvv_vvvvv01234")
      .tinc("vvvvvvvvvv_vvvvvvvvvv_vvvvv01234_0000000000_0001000000")
      ;
  }
}
