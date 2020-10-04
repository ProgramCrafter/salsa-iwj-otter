// See bigfloat.ts

#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_mut)]
#![allow(unused_imports)]

use crate::imports::*;

enum Sign { Pos, Neg, }
use Sign::*;

type Sz = u16;
type Limb = [u16;3];

pub struct Bigfloat(innards::Innards);

mod innards {
  use super::*;
  use std::mem::{self, align_of, size_of};
  use std::ptr::{self, NonNull};
  use std::alloc::{self, Layout};
  use std::slice;

  pub type Innards = NonNull<u8>;

  struct Header {
    sign: Sign,
    exp: Sz,
    nlimbs: Sz,
  }

  #[repr(C)]
  struct Repr {
    h: Header,
    limbs: [Limb],
  }

  const OFFSET : usize = {
    let h_size = size_of::<Header>();
    let l_align = align_of::<Limb>();
    l_align * ((h_size + l_align - 1) / l_align)
  };

  fn layout(nlimbs: Sz) -> Layout {
    let limbs_nbytes : usize = size_of::<Limb>() * (nlimbs as usize);
    let all_nbytes = OFFSET + limbs_nbytes;
    let align = max(align_of::<Header>(), align_of::<Limb>());
    Layout::from_size_align(all_nbytes, align).unwrap()
  }

  fn ptrs(p: *mut u8) -> (*mut Header, *mut Limb) { unsafe {
    let p_header : *mut Header = mem::transmute(p);
    let p_limbs  : *mut Limb   = mem::transmute(p.add(OFFSET));
    (p_header, p_limbs)
  } }

  impl Bigfloat {
    pub(in super)
    fn from_parts(sign: Sign, exp: Sz, limbs: &[Limb]) -> Bigfloat {
      let nlimbs : Sz = limbs.len().try_into().expect("limb count overflow");
      unsafe {
        let p = alloc::alloc(layout(nlimbs));
        let (p_header, p_limbs) = ptrs(p);
        ptr::write(p_header, Header { sign, exp, nlimbs });
        ptr::copy_nonoverlapping(limbs.as_ptr(), p_limbs, nlimbs.into());
        Bigfloat(NonNull::new(p).unwrap())
      }
    }

    fn as_parts(&self) -> (&Header, &[Limb]) {
      unsafe {
        let (h, l) = ptrs(self.0.as_ptr());
        let h = h.as_ref().unwrap();
        let limbs = slice::from_raw_parts(l, h.nlimbs.into());
        (h, limbs)
      }
    }
    fn as_mut_limbs(&self) -> (&Header, &mut [Limb]) {
      unsafe {
        let (h, l) = ptrs(self.0.as_ptr());
        let h = h.as_mut().unwrap();
        let limbs = slice::from_raw_parts_mut(l, h.nlimbs.into());
        (h, limbs)
      }
    }
  }

  impl Drop for Bigfloat {
    fn drop(&mut self) {
      let (h, _) = self.as_parts();
      let nlimbs = h.nlimbs;
      unsafe {
        alloc::dealloc(self.0.as_mut(), layout(nlimbs));
      }
    }
  }

  impl Clone for Bigfloat {
    fn clone(&self) -> Bigfloat {
      unsafe {
        let (h, _) = self.as_parts();
        let nlimbs = h.nlimbs;
        let layout = layout(nlimbs);
        let p = alloc::alloc(layout);
        ptr::copy_nonoverlapping(self.0.as_ptr(), p, layout.size());
        Bigfloat(NonNull::new(p).unwrap())
      }
    }
  }

}
/*/
impl Bigfloat {
  fn from_parts(sign: Sign, exp: Sz, limbs: &[Limb]) {
    Bigfloat(Innards::from_parts(sign, exp, limbs))
*/

impl Bigfloat {
  #[throws(as Option)]
  fn from_str(s: &str) -> Self {
    struct P<'s> (&'s str);
    impl P<'_> {
      #[throws(as Option)]
      fn hex16(&mut self) -> u16 {
        const L : usize = 4;
        if self.0.len() < L { None? }
        let (l, r) = self.0.split_at(L);
        self.0 = r;
        Sz::from_str_radix(l, 16).ok()?
      }

      #[throws(as Option)]
      fn next(&mut self) -> char {
        let r = self.0.chars().next()?;
        self.0 = &self.0[1..];
        r
      }

      #[throws(as Option)]
      fn hex16_(&mut self) -> u16 {
        let r = self.hex16()?;
        if !matches!(self.next(), Some('_')) { None? }
        r
      }
    }

    let mut p = P(s);
    let sign = match p.next()? {
      '+' => Pos,
      '!' => Neg,
      _ => None?,
    };
    let exp = p.hex16();

    let mut limbs = Vec::with_capacity(p.0.len() / 15);
    loop {
      match p.next() {
        None => break,
        Some(' ') => (),
        _ => None?,
      };
      limbs.push([
        p.hex16_()?,
        p.hex16_()?,
        p.hex16()?,
      ]);
    }
    Bigfloat::from_parts(sign, 0, &limbs)
  }
}

#[cfg(test)]
mod test {
  #[test]
  fn bfparse() {
  }
}
