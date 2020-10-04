// See bigfloat.ts

use crate::imports::*;

#[derive(Copy,Clone,Debug,Ord,Eq,PartialOrd,PartialEq)]
enum Sign { Neg, Pos, }
use Sign::*;

type Sz = u16;

const CHARS_HEADER : usize = 5;
const CHARS_PER_LIMB : usize = 15;

pub use innards::Bigfloat;

#[repr(transparent)]
#[derive(Copy,Clone,Debug,Ord,PartialOrd,Eq,PartialEq)]
struct Limb (pub [u16;3]);

mod innards {
  use super::*;
  use std::mem::{self, align_of, size_of};
  use std::ptr::{self, NonNull};
  use std::alloc::{self, Layout};
  use std::slice;

  #[derive(Deserialize)]//,Serialize
  #[serde(try_from="&str")]
  pub struct Bigfloat(Innards);

  type Innards = NonNull<u8>;

  pub(in super)
  struct Header {
    pub sign: Sign,
    pub exp: Sz,
    pub nlimbs: Sz,
  }

  #[repr(C)]
  #[allow(dead_code)] // this is for documentation purposes
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

    pub(in super)
    fn as_parts(&self) -> (&Header, &[Limb]) {
      unsafe {
        let (h, l) = ptrs(self.0.as_ptr());
        let h = h.as_ref().unwrap();
        let limbs = slice::from_raw_parts(l, h.nlimbs.into());
        (h, limbs)
      }
    }

    #[allow(dead_code)] // xxx
    pub(in super)
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
    let exp = p.hex16()?;

    let mut limbs = Vec::with_capacity(p.0.len() / CHARS_PER_LIMB);
    loop {
      match p.next() {
        None => break,
        Some(' ') => (),
        _ => None?,
      };
      limbs.push(Limb([
        p.hex16_()?,
        p.hex16_()?,
        p.hex16()?,
      ]));
    }
    if limbs.is_empty() { None? }
    Bigfloat::from_parts(sign, exp, &limbs)
  }

  fn to_string(&self) -> String {
    let (h, _) = self.as_parts();
    let n = CHARS_HEADER + CHARS_PER_LIMB * (h.nlimbs as usize);
    let mut s = String::with_capacity(n);
    write!(&mut s, "{}", self).unwrap();
    s
  }

/*
  fn add(&self, v: u32) -> Bigfloat {
    let (h, l) = self.as_parts();
    let l : Vec<_> = l.collect();

    fn add_to_limb(&self, v: u32, );
  }
*/
}

impl Display for Bigfloat {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    let (h,ls) = self.as_parts();
    write!(f, "{}{:04x}",
           match h.sign { Pos => '+', Neg => '!', },
           h.exp)?;
    for l in ls {
      write!(f, " {:04x}_{:04x}_{:04x}", l.0[0], l.0[1], l.0[2])?;
    }
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
    let (ah, al) = self.as_parts();
    let (bh, bl) = other.as_parts();

    {

      ah.sign.cmp(&bh.sign)

    }.then_with(||{

      let mut x = ah.exp.cmp(&bh.exp);
      if ah.sign == Neg { x = x.reverse() }
      x

    }.then_with(||{

      al.cmp(bl)

    }))
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
    let d = self.to_string();
    s.serialize_str(&d)
  }
}

#[cfg(test)]
mod test {
  use super::*;

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
    assert!( Bigfloat::from_str("!0000 ffff_ffff_fff0") <
             Bigfloat::from_str("!0000 ffff_ffff_fff3") );
    
    assert!( Bigfloat::from_str("!0001 ffff_ffff_ffff") <
             Bigfloat::from_str("!0000 ffff_ffff_0000") );
    
    assert!( Bigfloat::from_str("+0000 ffff_ffff_0000") <
             Bigfloat::from_str("+0001 ffff_ffff_ffff") );
    
    assert!( Bigfloat::from_str("+0000 ffff_ffff_0000") <
             Bigfloat::from_str("+0000 ffff_ffff_0000 1234_ffff_0000") );
  }
}
