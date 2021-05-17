// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use otter_api_tests::*;

portmanteau_has!("at-otter.rs", at_otter);

#[throws(AE)]
fn main() { portmanteau_main("at")? }
