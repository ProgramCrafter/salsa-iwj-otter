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
  use std::alloc::{alloc, Layout};

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

  impl Bigfloat {
    pub(in super) fn from_parts(sign: Sign, exp: Sz, limbs: &[Limb]) -> Bigfloat {
      let nlimbs_u = limbs.len();
      let nlimbs_sz : Sz = nlimbs_u.try_into().expect("limb count overflow");
      let limbs_nbytes = nlimbs_u * size_of::<Limb>();
      let all_nbytes = OFFSET + limbs_nbytes;
      let align = max(align_of::<Header>(), align_of::<Limb>());
      let layout = Layout::from_size_align(all_nbytes, align).unwrap();
      unsafe {
        let p = alloc(layout);
        let p_header : *mut Header = mem::transmute(p);
        let p_limbs  : *mut Limb   = mem::transmute(p.add(OFFSET));
        ptr::write(p_header, Header { sign, exp, nlimbs: nlimbs_sz });
        ptr::copy_nonoverlapping(limbs.as_ptr(), p_limbs, nlimbs_u);
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
    let mut s = s.as_bytes();
    let mut s = s.iter();
    let sign = match *s.next()? {
      b'+' => Pos,
      b'!' => Neg,
      _ => None?,
    };
    let limbs = vec![];
    Bigfloat::from_parts(sign, 0, &limbs)
  }
}
