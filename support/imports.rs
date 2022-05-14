// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use std::collections::{hash_map, HashMap, HashSet};
pub use std::sync::Arc;

pub use log::{debug, error, info, trace, warn};
pub use serde::ser::SerializeTuple;
pub use serde::{de::DeserializeOwned, Deserialize, Serialize};
pub use serde::de::Error as _;
pub use serde::{Deserializer, Serializer};
pub use serde_with::DeserializeFromStr;
pub use serde_with::SerializeDisplay;

// No debug version of this
pub use parking_lot::{RwLock, RwLockReadGuard, RwLockWriteGuard};

// Swap this over for debugging
pub use parking_lot::{Mutex, MutexGuard};
//pub use crate::debugmutex::{Mutex, MutexGuard};

pub use crate::support::*;
pub use crate::tz::*;

