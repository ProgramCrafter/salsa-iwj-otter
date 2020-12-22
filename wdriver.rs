// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use fehler::throws;
pub use structopt::StructOpt;

pub use std::env;
pub use std::io::Write;
pub use std::os::unix::process::CommandExt;
pub use std::process::Command;

pub type AE = anyhow::Error;
