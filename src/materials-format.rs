// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

#[derive(Debug,Copy,Clone,Display,Deref,Serialize,Deserialize,Into)]
#[derive(Eq,PartialEq,Ord,PartialOrd)]
#[display(fmt="{}", _0)]
#[serde(try_from="Raw", into="Raw")]
pub struct Version(Raw);

pub type Raw = u32;

impl Version {
  pub const MIN:     Version = Version(1);
  pub const MAX:     Version = Version(2);
  pub const CURRENT: Version = Version::MAX;
}

// The version from before we invented this versioning scheme
impl Default for Version {
  fn default() -> Self { Self(1) }
}

#[derive(Error,Debug,Copy,Clone,Serialize,Deserialize)]
#[derive(Eq,PartialEq,Ord,PartialOrd)]
#[error(r#"unsupported bundle, library or spec format version "format={0}""#)]
pub struct Unsupported(u32);

#[derive(Error,Debug,Copy,Clone,Serialize,Deserialize)]
pub struct Incompat<E> {
  mformat: Version,
  error: E,
}
impl<E> Display for Incompat<E> where E: Display {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    if self.mformat == 1 {
      write!(f,
             r#"{}: not supported in format 1, maybe you forgoot "format="#,
             self.error)?;
    } else {
      write!(f,
             r#"{}: not supported with "format={}""#,
             self.error, self.mformat)?;
    }
  }
}
impl Version {
  pub fn incompat<E>(self, error: E) -> Incompat<E> {
    Incompat { mformat: self, error }
  }

  pub fn err_mapper<E>(self) -> impl Fn(E) -> Incompat<E> {
    move |error| self.incompat(error)
  }
}

#[derive(Error,Debug,Clone,Copy,Serialize,Deserialize)]
#[derive(Eq,PartialEq)]
pub enum VersionError {
  #[error("{0}")]
  Unsupported(#[from] Unsupported),
  #[error("bad format version: {0}")]
  Other(&'static str),
}

impl TryFrom<Raw> for Version {
  type Error = Unsupported;
  #[throws(Unsupported)]
  fn try_from(raw: Raw) -> Self {
    if raw >= *Self::MIN && raw <= *Self::MAX {
      Version(raw)
    } else {
      throw!(Unsupported(raw));
    }
  }
}

impl Version {
  #[throws(VersionError)]
  pub fn try_from_integer<I>(i: I) -> Version
  where I: num::Integer + ToPrimitive {
    let v = i.to_u32().ok_or_else(|| MFVE::Other("integer out of range"))?;
    v.try_into()?
  }
}

impl FromStr for Version {
  type Err = VersionError;
  #[throws(VersionError)]
  fn from_str(s: &str) -> Version {
    let v: Raw = s.parse()
      .map_err(|_| MFVE::Other("could not parse as ingeger"))?;
    v.try_into()?
  }
}

macro_rules! impl_compar_i32 { {
  $(
    impl $Trait:ident { fn $method:ident -> $out:ty; }
  )*
} => {
  $(
    impl $Trait<i32> for Version {
      fn $method(&self, rhs: &i32) -> $out {
        let lhs = i64::from(self.0);
        let rhs = i64::from(*rhs);
        lhs.$method(&rhs)
      }
    }
  )*
} }
impl_compar_i32! {
  impl PartialEq { fn eq -> bool; }
  impl PartialOrd { fn partial_cmp -> Option<Ordering>; }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn ser_deser() {
    #[derive(Debug,Deserialize,Serialize,Eq,PartialEq,Copy,Clone)]
    struct S {
      mformat: Version,
    }
    #[derive(Debug,Deserialize,Serialize,Eq,PartialEq,Copy,Clone)]
    struct R {
      mformat: Raw,
    }

    fn check(raw: Raw, exp: Result<Version, Unsupported>) {
      let got = raw.try_into();
      assert_eq!(&got,&exp);

      let json_r = serde_json::to_string(&R { mformat: raw }).unwrap();
      let deser: Result<S,_> = serde_json::from_str(&json_r);
      assert_eq!{ exp.ok().map(|e| S { mformat: e }), deser.ok() }

      if let Ok(got) = got {
        let json_s = serde_json::to_string(&S { mformat: got }).unwrap();
        assert_eq!{ json_s, json_r }
      }
    }

    let eus = |raw| Err(Unsupported(raw));

    check(0, eus(0));
    check(1, Ok(Version(1)));
    check(2, Ok(Version(2)));
    check(3, eus(03));
  }

  #[test]
  fn compar() {
    let one = Version::try_from(1).unwrap();
    assert!{ one >  0 }     assert!{ !( one <= 0 ) }
    assert!{ one >= 1 }     assert!{ !( one <  1 ) }
    assert!{ one == 1 }     assert!{ !( one != 1 ) }
    assert!{ one <= 1 }     assert!{ !( one >  1 ) }
    assert!{ one <  2 }     assert!{ !( one >= 2 ) }
  }
}
