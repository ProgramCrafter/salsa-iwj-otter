// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;
use crate::packetframe::ResponseWriter;

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct ProgressInfo {
  pub count: usize,
  pub of: usize,
}

pub trait ProgressReporter {
}

impl<W> ProgressReporter for ResponseWriter<'_, W> where W: Write {
}

impl ProgressReporter for () {
}
