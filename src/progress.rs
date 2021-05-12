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
  fn report(&mut self, info: ProgressInfo<'_>)
            -> Result<(), MgmtChannelWriteError>;
}

impl<W> Reporter for ResponseWriter<'_, W> where W: Write {
  #[throws(MgmtChannelWriteError)]
  fn report(&mut self, pi: ProgressInfo<'_>) {
    self.progress(pi)?
  }
}

impl Reporter for () {
  #[throws(MgmtChannelWriteError)]
  fn report(&mut self, _pi: ProgressInfo<'_>) { }
}

impl<T> From<T> for Count<'static>
where T: EnumCount + ToPrimitive + AsStaticRef<str>
{
  fn from(t: T) -> Count<'static> {
    Count {
      i: t.to_usize().unwrap(),
      n: T::COUNT,
      desc: Cow::Borrowed(t.as_static()),
    }
  }
}

#[ext(pub)]
impl &mut dyn Reporter {
  #[throws(MgmtChannelWriteError)]
  fn phase_entry<'p,'e,P,E>(&mut self, phase: P, entry: E)
  where P: Into<Count<'p>>,
        E: Into<Count<'e>>,
  {
    self.report(ProgressInfo {
      phase: phase.into(),
      entry: entry.into(),
    })?
  }
}
