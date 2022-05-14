// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

//! Otter game system (part thereeof)
//!
//! <https://www.chiark.greenend.org.uk/~ianmdlvl/otter/docs/README.html>
//!
//! This crate is intended for use only by other parts of Otter.

#![allow(clippy::redundant_closure_call)]
#![allow(clippy::writeln_empty_string)]

pub mod crates;
pub mod prelude;

#[path="prelude-part.rs"]
pub mod prelude_part;

pub mod geometry;
pub mod html;
pub mod zcoord;
pub mod misc;
