// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::*;

type Ctx = UsualCtx;

impl Ctx {
}

#[throws(Explode)]
fn tests(_c: Ctx) {
}

#[throws(Explode)]
pub fn main() {
  tests(Ctx::setup()?)?;
}
