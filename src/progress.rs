// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

#[derive(Debug,Clone,Serialize,Deserialize,IntoOwned)]
pub struct ProgressInfo<'pi> {
  phase: Count<'pi>,
  entry: Count<'pi>,
}

#[derive(Debug,Clone,Serialize,Deserialize,IntoOwned)]
pub struct Count<'pi> {
  pub i: usize,
  pub n: usize,
  pub desc: Cow<'pi, str>,
}

pub trait Reporter {
  fn report(&mut self, info: ProgressInfo<'_>);
  fn phase_begin_(&mut self, phase: Count<'_>, len: usize);
  fn item_(&mut self, entry: usize, desc: Cow<'_, str>);
}

pub struct ResponseReporter<'c,'w,W> where W: Write {
  chan: &'c mut ResponseWriter<'w,W>,
  phase: Count<'static>,
  len: usize,
}
impl<'c,'w,W> ResponseReporter<'c,'w,W> where W: Write {
  pub fn new(chan: &'c mut ResponseWriter<'w,W>) -> Self { Self {
    chan,
    phase: Count { i:0, n:0, desc: Cow::Borrowed("") },
    len: 0,
  } }
}

impl<W> Reporter for ResponseReporter<'_,'_,W> where W: Write {
  fn report(&mut self, pi: ProgressInfo<'_>) {
    self.chan.progress(pi).unwrap_or(());
  }
  fn phase_begin_(&mut self, phase: Count<'_>, len: usize) {
    self.phase = phase.into_owned();
    self.len = len;
  }
  fn item_(&mut self, entry: usize, desc: Cow<'_, str>) {
    self.report(ProgressInfo {
      phase: self.phase.clone(),
      entry: Count { i: entry, n: self.len, desc }
    })
  }
}

#[allow(unused_variables)]
impl Reporter for () {
  fn report(&mut self, pi: ProgressInfo<'_>) { }
  fn phase_begin_(&mut self, phase: Count<'_>, len: usize) { }
  fn item_(&mut self, entry: usize, desc: Cow<'_, str>) { }
}

impl<'t,T> From<&'t T> for Count<'t>
where T: EnumCount + ToPrimitive + EnumMessage
{
  fn from(t: &'t T) -> Count<'t> {
    Count {
      i: t.to_usize().unwrap(),
      n: T::COUNT,
      desc: Cow::Borrowed(t.get_message().unwrap_or("...")),
    }
  }
}

#[ext(pub, name=ReporterExt)]
impl &mut dyn Reporter {
  fn phase_entry<P,E>(&mut self, phase: P, entry: E)
  where for <'p> &'p P: Into<Count<'p>>,
        for <'e> &'e E: Into<Count<'e>>,
  {
    let phase = &phase; let phase = phase.into();
    let entry = &entry; let entry = entry.into();
    self.report(ProgressInfo { phase, entry });
  }

  fn phase_begin<P>(&mut self, phase: P, len: usize)
  where for <'p> &'p P: Into<Count<'p>>,
  {
    self.phase_begin_((&phase).into(), len)
  }

  fn item<'s,S>(&mut self, entry: usize, desc: S)
  where S: Into<Cow<'s, str>>,
  {
    self.item_(entry, desc.into())
  }
}
