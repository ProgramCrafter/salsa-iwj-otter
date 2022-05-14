// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// See Import Structure Doctrine in src/prelude.rs

pub use std::borrow::Cow;
pub use std::cmp::{self, max, min, Ordering};
pub use std::collections::{hash_map, HashMap, HashSet};
pub use std::collections::VecDeque;
pub use std::collections::{btree_map, BTreeMap};
pub use std::collections::{btree_set, BTreeSet};
pub use std::env;
pub use std::fs;
pub use std::fs::File;
pub use std::io;
pub use std::io::ErrorKind;
pub use std::io::{BufRead, BufReader, BufWriter, Read, Write};
pub use std::os::linux::fs::MetadataExt as _; // todo why linux for st_mode??
pub use std::os::unix;
pub use std::os::unix::ffi::OsStrExt;
pub use std::os::unix::fs::{MetadataExt, OpenOptionsExt};
pub use std::os::unix::io::{AsRawFd, IntoRawFd, RawFd};
pub use std::os::unix::net::UnixStream;
pub use std::os::unix::process::{CommandExt, ExitStatusExt};
pub use std::process::{exit, Child, Command, Stdio};
pub use std::sync::Arc;

pub use anyhow::{anyhow, ensure, Context};
pub use byteorder::{BigEndian, LittleEndian, ReadBytesExt, WriteBytesExt};
pub use derive_into_owned::IntoOwned;
pub use log::{debug, error, info, trace, warn};
pub use nix::unistd::{self, Uid};
pub use nix::sys::time::TimeSpec;
pub use nix::time::clock_gettime;
pub use num_derive::{ToPrimitive, FromPrimitive};
pub use num_traits::{Bounded, FromPrimitive, ToPrimitive};
pub use serde::ser::SerializeTuple;
pub use serde::{de::DeserializeOwned, Deserialize, Serialize};
pub use serde::de::Error as _;
pub use serde::{Deserializer, Serializer};
pub use serde_with::DeserializeFromStr;
pub use serde_with::SerializeDisplay;
pub use strum::{EnumCount, EnumDiscriminants};
pub use strum::{EnumString, EnumIter, EnumMessage, EnumProperty};
pub use strum::{AsRefStr, IntoEnumIterator, IntoStaticStr};

// No debug version of this
pub use parking_lot::{RwLock, RwLockReadGuard, RwLockWriteGuard};

// Swap this over for debugging
pub use parking_lot::{Mutex, MutexGuard};
//pub use crate::debugmutex::{Mutex, MutexGuard};

pub use crate::matches_doesnot;

pub use crate::childio;
pub use crate::debugmutex::DebugIdentify;
pub use crate::packetframe::{FrameReader, FrameWriter, ReadFrame, WriteFrame};
pub use crate::packetframe::{ReadExt, ResponseWriter};
pub use crate::packetframe::{MgmtChannelReadError, MgmtChannelWriteError};
pub use crate::progress::{self, ProgressInfo, OriginatorExt as _};
pub use crate::support::*;
pub use crate::termprogress;
pub use crate::tz::*;

// ---------- type abbreviations ----------

pub type AE = anyhow::Error;
