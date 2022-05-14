// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

//========== miscellany ==========
// (roughly in order of implementation length)

//---------- IpAddress ----------

pub trait IpAddress: Debug {
  fn with_port(&self, port: u16) -> SocketAddr;
}

impl<A> IpAddress for A where A: Into<IpAddr> + Debug + Clone {
  fn with_port(&self, port: u16) -> SocketAddr {
    match (self.clone().into(), port)
      .to_socket_addrs()
      .map(|i| i.at_most_one()) {
        Ok(Ok(Some(addr))) => addr,
        x => panic!("{:?},{} gave {:?}", self, port, x),
      }
  }
}

//========== toml ====================

#[derive(Debug,Copy,Clone,Eq,PartialEq,Ord,PartialOrd)]
pub struct TomlQuote<'s>(pub &'s str);

// We reimplement this because the toml crate doesn't expose it, and
// looking at the github issues etc. for that crate isn't encuraging.
impl<'s> Display for TomlQuote<'s> {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    for c in self.0.chars() {
      match c {
        '"' | '\\'=> write!(f, "\\{}", c)?,
        c if (c < ' ' && c != '\t') || c == '\x7f' => {
          write!(f, r#"\u{:04x}"#, c as u32).unwrap();
          continue;
        }
        c => write!(f, "{}", c)?,
      }
    }
  }
}

#[test]
fn toml_quote_string_test(){
  assert_eq!(TomlQuote(r#"w \ "	ƒ."#).to_string(),
                       r#"w \\ \"	\u0007\u007fƒ."#);
}

pub fn toml_merge<'u,
                  S: 'u + AsRef<str>,
                  KV: IntoIterator<Item=(&'u S, &'u toml::Value)>
                  >(
  table: &mut toml::value::Table,
  updates: KV,
) {
  use toml::value::{Table, Value};
  type TME<'e> = toml::map::Entry<'e>;

  let mut kv = updates.into_iter().map(|(k, v)| (k.as_ref(), v));
  inner(table, &mut kv);

  fn inner<'u>(
    table: &mut Table,
    updates: &'u mut dyn Iterator<Item=(&'u str, &'u Value)>
  ) {
    for (k, v) in updates {
      let e = table.entry(k);
      match e {
        TME::Vacant(ve) => {
          ve.insert(v.clone());
        }
        TME::Occupied(mut oe) => match (oe.get_mut(), v) {
          (Value::Table(old), Value::Table(new)) => {
            toml_merge(old, new);
          }
          (Value::Array(old), Value::Array(new)) => {
            old.extend(new.iter().cloned());
          }
          (old, new) => {
            *old = new.clone();
          }
        }
      }
    }
  }
}

//========== Timestamp ==========

#[derive(Copy,Clone,Debug,Serialize,Deserialize,Eq,Ord,PartialEq,PartialOrd)]
#[serde(transparent)]
pub struct Timestamp(pub u64); /* time_t */

impl Timestamp {
  /// Always >= previously
  pub fn now() -> Timestamp {
    use std::time::SystemTime;
    let now = SystemTime::now()
      .duration_since(SystemTime::UNIX_EPOCH)
      .unwrap()
      .as_secs();
    Timestamp(now)
  }

  pub fn render(&self, tz: &Timezone) -> String {
    tz.format(*self)
  }
}

//========== miscellaneous macros ==========

paste!{
  #[cfg(debug_assertions)]
  pub fn [<x x x>]<T>() -> T { panic!("todo item triggered") }
}

#[macro_export]
macro_rules! trace_dbg {
  ($msg:expr $(,$val:expr)*) => {
    if log_enabled!(log::Level::Trace) {
      #[allow(unused_mut)]
      let mut buf = format!("{}", &$msg);
      $( write!(&mut buf, " {}={:?}", stringify!($val), &$val).unwrap(); )*
      trace!("{}", buf);
    }
  }

}

//========== matches_doesnot ==========

#[macro_export] // <- otherwise bogus warning `unused_macros`
macro_rules! matches_doesnot_yn2bool {
  (=) => (true);
  (!) => (false);
}

#[macro_export]
macro_rules! matches_doesnot {
  ($v:expr,
   $(
     $yn:tt $p:pat
   ),* $(,)?
  ) => {
    match $v {
      $(
        $p => $crate::matches_doesnot_yn2bool!($yn),
      )*
    }
  }
}

#[test]
fn matches_doesnot_test() {
  assert!(
    matches_doesnot!(
      Some(42),
      = Some(_),
      ! None
    )
  );
  assert!(
    matches_doesnot!(
      Some(42),
      ! None,
      ! Some(3),
      = Some(_),
    )
  );
  assert!(
    matches_doesnot!(
      Some(1),
      = Some(1) | Some(2),
      ! Some(_) | None
    )
  );
  assert!(
    ! matches_doesnot!(
      Some(1),
      ! Some(1) | Some(2),
      = Some(_) | None
    )
  );
}

