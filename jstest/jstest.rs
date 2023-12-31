// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

//! Otter game system (part thereeof)
//!
//! <https://www.chiark.greenend.org.uk/~ianmdlvl/otter/docs/README.html>
//!
//! This crate is intended for use only by other parts of Otter.

#![allow(clippy::or_fun_call)]
#![allow(clippy::unnecessary_operation)] // trips on #[throws(Explode)]

pub use otter::crates::*;
pub use otter::prelude::*;

pub use otter_api_tests::Explode;

pub use indexmap::IndexMap;
pub use indexmap::IndexSet;
pub use structopt::StructOpt;
