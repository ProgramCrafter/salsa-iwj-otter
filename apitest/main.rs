// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use otter_api_tests::*;

pub use std::cell::{RefCell, RefMut};
pub use std::rc::Rc;

type Setup = Rc<RefCell<SetupCore>>;

portmanteau_has!("at-otter.rs",   at_otter);
portmanteau_has!("at-bundles.rs", at_bundles);

#[throws(AE)]
fn main() { portmanteau_main("at")? }
