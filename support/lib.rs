// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub mod imports;
pub mod prelude;

#[path="prelude-part.rs"]
pub mod prelude_part;

pub mod support;

#[path="../src/tz.rs"] pub mod tz;
