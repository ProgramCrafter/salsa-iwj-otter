// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

pub use crate::imports::{flexi_logger, thiserror};

pub use std::any::Any;
pub use std::borrow::Borrow;
pub use std::borrow::Cow;
pub use std::cmp::{self, max, min, Ordering};
pub use std::collections::VecDeque;
pub use std::collections::{btree_set, BTreeSet};
pub use std::collections::{hash_map, HashMap, HashSet};
pub use std::convert::{TryFrom, TryInto};
pub use std::env;
pub use std::error::Error;
pub use std::fmt::Formatter;
pub use std::fmt::Write as _;
pub use std::fmt::{self, Debug, Display};
pub use std::fs;
pub use std::fs::File;
pub use std::hash::Hash;
pub use std::io;
pub use std::io::ErrorKind;
pub use std::io::{BufRead, BufReader, BufWriter, Read, Write};
pub use std::io::{Seek, SeekFrom};
pub use std::iter;
pub use std::iter::repeat_with;
pub use std::marker::PhantomData;
pub use std::mem;
pub use std::num::{NonZeroUsize, TryFromIntError, Wrapping};
pub use std::ops::{Deref, DerefMut};
pub use std::os::unix;
pub use std::os::unix::ffi::OsStrExt;
pub use std::os::unix::net::UnixStream;
pub use std::os::unix::process::CommandExt;
pub use std::path::PathBuf;
pub use std::process::{exit, Command};
pub use std::str;
pub use std::str::FromStr;
pub use std::string::ParseError;
pub use std::sync::{Arc, Condvar, Mutex, MutexGuard};
pub use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};
pub use std::thread::{self, sleep};
pub use std::time::Duration;
pub use std::time::Instant;

pub use anyhow::{anyhow, Context};
pub use arrayvec::ArrayVec;
pub use boolinator::Boolinator as _;
pub use delegate::delegate;
pub use derive_more::*;
pub use downcast_rs::{impl_downcast, Downcast};
pub use either::{Either, Left, Right};
pub use enum_dispatch::enum_dispatch;
pub use enum_map::{Enum, EnumMap};
pub use fehler::{throw, throws};
pub use flexi_logger::LogSpecification;
pub use fs2::FileExt;
pub use if_chain::if_chain;
pub use index_vec::{define_index_type, index_vec, IndexSlice, IndexVec};
pub use itertools::{izip, EitherOrBoth, Itertools};
pub use lazy_static::lazy_static;
pub use log::{debug, error, info, trace, warn};
pub use log::{log, log_enabled};
pub use nix::unistd::Uid;
pub use num_traits::{Bounded, FromPrimitive, ToPrimitive};
pub use ordered_float::OrderedFloat;
pub use percent_encoding::percent_decode_str;
pub use percent_encoding::utf8_percent_encode;
pub use percent_encoding::NON_ALPHANUMERIC;
pub use rand::distributions::Alphanumeric;
pub use rand::thread_rng;
pub use rand::Rng;
pub use regex::Regex;
pub use serde::ser::SerializeTuple;
pub use serde::{de::DeserializeOwned, Deserialize, Serialize};
pub use serde::{Deserializer, Serializer};
pub use serde_with::DeserializeFromStr;
pub use serde_with::SerializeDisplay;
pub use slotmap::{dense::DenseSlotMap, Key as _};
pub use strum::EnumString;
pub use thiserror::Error;
pub use url::Url;
pub use vecdeque_stableix::Deque as StableIndexVecDeque;
pub use void::Void;

pub use otter_base::zcoord::{self, ZCoord};
pub use otter_base::misc as base_misc;
pub use base_misc::default;

pub use crate::from_instance_lock_error;

pub use crate::accounts::loaded_acl::{self, EffectiveACL, LoadedAcl, PermSet};
pub use crate::accounts::*;
pub use crate::authproofs::{self, Authorisation, Unauthorised};
pub use crate::authproofs::AuthorisationSuperuser;
pub use crate::commands::{AccessTokenInfo, AccessTokenReport, MgmtError};
pub use crate::config::*;
pub use crate::debugreader::DebugReader;
pub use crate::error::*;
pub use crate::gamestate::*;
pub use crate::global::*;
pub use crate::hidden::*;
pub use crate::keydata::*;
pub use crate::mgmtchannel::*;
pub use crate::nwtemplates;
pub use crate::pieces::*;
pub use crate::shapelib;
pub use crate::slotmap_slot_idx::*;
pub use crate::spec::*;
pub use crate::spec::piece_specs::FaceColourSpecs;
pub use crate::sse;
pub use crate::toml_de;
pub use crate::tz::*;
pub use crate::updates::*;
pub use crate::utils::*;
pub use crate::ui::*;

pub type SecondarySlotMap<K,V> = slotmap::secondary::SecondaryMap<K,V>;
pub type SvgData = Vec<u8>;
pub type Colour = Html;

// ---------- type abbreviations ----------

pub type AE = anyhow::Error;

// accounts.rs
pub type AS = AccountScope;

// commands.rs
pub type ME = MgmtError;

// error.rs
pub type APOE = ApiPieceOpError;
pub type ESVU<POEPU> = ErrorSignaledViaUpdate<POEPU>;
pub type IE = InternalError;
pub type OE = OnlineError;
pub type POEPP = PieceOpErrorPartiallyProcessed;
pub type SvgE = SVGProcessingError;
pub type SpE = SpecError;

// updates.rs
pub type PUE = PreparedUpdateEntry;
pub type PUFOS = PieceUpdateFromOpSimple;
pub type PUO<NS,ZL> = PieceUpdateOp<NS,ZL>;
pub type PUOs = PieceUpdateOps;
pub type WRC = WhatResponseToClientOp;
