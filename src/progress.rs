// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;
use crate::packetframe::ResponseWriter;

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
}

impl<W> Reporter for ResponseWriter<'_, W> where W: Write {
  fn report(&mut self, pi: ProgressInfo<'_>) {
    self.progress(pi).unwrap_or(());
  }
}

impl Reporter for () {
  fn report(&mut self, _pi: ProgressInfo<'_>) { }
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
}
