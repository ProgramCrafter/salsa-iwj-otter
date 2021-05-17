// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::*;

type Setup = Rc<RefCell<SetupCore>>;

#[allow(dead_code)]
struct Ctx {
  opts: Opts,
  su_rc: Setup,
}

impl Ctx {
}

#[throws(Explode)]
fn tests(_c: Ctx) {
}

#[throws(Explode)]
pub fn main() {
  let (opts, _instance, su) = setup_core(
    &[module_path!()],
  )?;

  let su_rc = Rc::new(RefCell::new(su));
  tests(Ctx { opts, su_rc })?;
}
