// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.


//! # Import Structure Doctrine:
//!
//! The purpose is to let us define our conventional namespace once,
//! without causing failures due to ambiguity.  (Importing the same name
//! via different import paths, both with `*`, does not work - the name is
//! regarded as ambiguous even though the referet is the same.  And the
//! error messages are hopeless: when this occurs the `*` imports are
//! simply ineffective for that name and nothing tells you there's an
//! ambiguity.)
//!
//! Each crate has these (public) modules:
//!
//!  * `crates`: One `pub use` statement for each dependency crate,
//!    including the otter crates, providing simply the crate name.
//!
//!  * `imports`: `pub use` statements for the names *within* crates that
//!    *this* crate wants to use, *excluding* any that are in the `imports`
//!    of this crate's dependencies.  This may `use` various `crates::*` or
//!    even `imports::*` but *must not* `pub use`.
//!
//!  * `prelude`: Brings all the parts together for the use of this crate.
//!    Should `pub use` the `crates::*` and `imports::*` for this crate
//!    and the dependencies.  Should not be used elsewhere.

pub use crate::crates::*;
pub use crate::imports::*;

pub use otter_base::crates::*;
pub use otter_base::imports::*;

pub use otter_support::crates::*;
pub use otter_support::imports::*;

