// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;
use crate::packetframe::ResponseWriter;

#[derive(Debug,Clone,Serialize,Deserialize,IntoOwned)]
pub struct ProgressInfo<'pi> {
  phase_num: usize,
  phases: usize,
  phase_desc: Cow<'pi, str>,
  count: usize,
  of: usize,
  desc: Cow<'pi, str>,
}

pub trait ProgressReporter {
  fn report(&mut self, info: ProgressInfo<'_>)
            -> Result<(), MgmtChannelWriteError>;
}

impl<W> ProgressReporter for ResponseWriter<'_, W> where W: Write {
  #[throws(MgmtChannelWriteError)]
  fn report(&mut self, pi: ProgressInfo<'_>) {
    self.progress(pi)?
  }
}

impl ProgressReporter for () {
  #[throws(MgmtChannelWriteError)]
  fn report(&mut self, _pi: ProgressInfo<'_>) { }
}
