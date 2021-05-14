// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

#[derive(Debug,Clone,Serialize,Deserialize,IntoOwned)]
pub struct ProgressInfo<'pi> {
  pub phase: Count<'pi>,
  pub item:  Count<'pi>,
}

#[derive(Debug,Clone,Serialize,Deserialize,IntoOwned)]
pub struct Count<'pi> {
  pub i: usize,
  pub n: usize,
  pub desc: Cow<'pi, str>,
}

pub trait Originator {
  fn report(&mut self, info: ProgressInfo<'_>);
  fn phase_begin_(&mut self, phase: Count<'_>, len: usize);
  fn item_(&mut self, item: usize, desc: Cow<'_, str>);
}

pub struct ResponseOriginator<'c,'w,W> where W: Write {
  chan: &'c mut ResponseWriter<'w,W>,
  phase: Count<'static>,
  len: usize,
}
impl<'c,'w,W> ResponseOriginator<'c,'w,W> where W: Write {
  pub fn new(chan: &'c mut ResponseWriter<'w,W>) -> Self { Self {
    chan,
    phase: Count { i:0, n:0, desc: Cow::Borrowed("") },
    len: 0,
  } }
}

impl<W> Originator for ResponseOriginator<'_,'_,W> where W: Write {
  fn report(&mut self, pi: ProgressInfo<'_>) {
    self.chan.progress(pi).unwrap_or(());
  }
  fn phase_begin_(&mut self, phase: Count<'_>, len: usize) {
    self.phase = phase.into_owned();
    self.len = len;
  }
  fn item_(&mut self, item: usize, desc: Cow<'_, str>) {
    self.report(ProgressInfo {
      phase: self.phase.clone(),
      item: Count { i: item, n: self.len, desc }
    })
  }
}

#[allow(unused_variables)]
impl Originator for () {
  fn report(&mut self, pi: ProgressInfo<'_>) { }
  fn phase_begin_(&mut self, phase: Count<'_>, len: usize) { }
  fn item_(&mut self, item: usize, desc: Cow<'_, str>) { }
}

pub trait Enum: EnumCount + ToPrimitive + EnumMessage { }
impl<T> From<T> for Count<'static> where T: Enum {
  fn from(t: T) -> Count<'static> {
    Count {
      i: t.to_usize().unwrap(),
      n: T::COUNT,
      // Show be Borrowed  https://github.com/Peternator7/strum/issues/159
      desc: Cow::Owned(t.get_message().unwrap_or("...").to_owned()),
    }
  }
}
impl<'t> From<&'t str> for Count<'t> {
  fn from(s: &'t str) -> Count<'t> {
    Count { i:0, n:0, desc: Cow::Borrowed(s) }
  }
}
impl From<String> for Count<'static> {
  fn from(s: String) -> Count<'static> {
    Count { i:0, n:0, desc: Cow::Owned(s) }
  }
}
impl<'t> From<()> for Count<'t> { fn from(_:()) -> Count<'t> {
    Count { i:0, n:0, desc: Cow::Borrowed("") }
} }

#[ext(pub, name=OriginatorExt)]
impl &mut dyn Originator {
  fn phase_item<'p,'e,P,E>(&mut self, phase: P, item: E)
  where P: Into<Count<'p>>,
        E: Into<Count<'e>>,
  {
    let phase = phase.into();
    let item  = item .into();
    self.report(ProgressInfo { phase, item });
  }

  fn phase<'p,P>(&mut self, phase: P, len: usize)
  where P: Into<Count<'p>>,
  {
    self.phase_begin_(phase.into(), len)
  }

  fn item<'s,S>(&mut self, item: usize, desc: S)
  where S: Into<Cow<'s, str>>,
  {
    self.item_(item, desc.into())
  }
}

pub struct ReadOriginator<'oo,'o,R:Read> {
  r: R,
  total_len: usize,
  orig: &'oo mut &'o mut dyn Originator,
  // state:
  counter: usize,
  last_report: usize,
}

impl<'oo,'o,R:Read> ReadOriginator<'oo,'o,R> {
  pub fn new<'p,P>(orig: &'oo mut &'o mut dyn Originator, phase: P,
                   total_len: usize, r: R) -> Self
  where P: Into<Count<'p>>
  {
    orig.phase(phase, total_len);
    let mut ro = ReadOriginator {
      r, orig, total_len,
      counter: 0,
      last_report: 0,
    };
    ro.report();
    ro
  }

  fn report(&mut self) {
    let t = self.total_len.to_string();
    let c = format!("{:>width$}", self.counter, width=t.len());
    let m = |s: String| {
      izip!( iter::once("").chain(["",""," "].iter().cloned().cycle()),
             s.chars().rev() )
        .map(|(s,c)| [s.chars().next(), Some(c)])
        .flatten()
        .flatten()
        .collect::<String>()
        .chars().rev()
        .collect::<String>()
    };
    let desc = format!("{} / {}", m(c), m(t));
    self.orig.item(self.counter, desc);
    self.last_report = self.counter;
  }
}

impl<R:Read> Read for ReadOriginator<'_,'_,R> {
  #[throws(io::Error)]
  fn read(&mut self, buf: &mut [u8]) -> usize {
    let got = self.r.read(buf)?;
    self.counter += got;
    if self.counter - self.last_report > 10000 {
      self.report();
    }
    got
  }
}
