// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use std::borrow::Borrow;
pub use std::cmp::{max, Ordering};
pub use std::convert::{TryFrom, TryInto};
pub use std::f64::consts::TAU;
pub use std::fmt::{self, Debug, Display, Formatter, Write as _};
pub use std::hash::{Hash, Hasher};
pub use std::iter::{self, FusedIterator};
pub use std::mem;
pub use std::num::{TryFromIntError, Wrapping};
pub use std::ops::{Deref, DerefMut, Index, IndexMut};
pub use std::str;
pub use std::str::FromStr;

pub use arrayvec::ArrayVec;
pub use derive_more::*;
pub use extend::ext;
pub use fehler::{throw, throws};
pub use if_chain::if_chain;
pub use itertools::{chain, iproduct, izip, zip_eq, EitherOrBoth, Itertools};
pub use serde::{Deserialize, Serialize};
pub use serde_with::DeserializeFromStr;
pub use serde_with::SerializeDisplay;
pub use thiserror::Error;
pub use void::Void;

pub use crate::html::*;

pub use crate::{pos_zip_map, pos_zip_try_map};
pub use crate::geometry::{CoordinateOverflow, PosC, PosPromote};
pub use crate::{dbgc, hformat, hformat_as_display, hwrite};
pub use crate::misc::default;
pub use crate::misc::display_as_debug;

pub use crate::if_let;
