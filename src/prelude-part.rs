// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;
use otter_support::imports::*;
use otter_base::imports::*;

use otter_base::prelude_part::*;

pub use std::any::Any;
pub use std::borrow::Cow;
pub use std::cmp::{self, max, min, Ordering};
pub use std::collections::VecDeque;
pub use std::collections::{btree_map, BTreeMap};
pub use std::collections::{btree_set, BTreeSet};
pub use std::convert::{Infallible, TryFrom, TryInto};
pub use std::env;
pub use std::error::Error;
pub use std::ffi::OsStr;
pub use std::fmt::Formatter;
pub use std::fmt::Write as _;
pub use std::fmt::{self, Debug, Display};
pub use std::fs;
pub use std::fs::File;
pub use std::hash::Hash;
pub use std::io;
pub use std::io::ErrorKind;
pub use std::io::{BufRead, BufReader, BufWriter, Read, Write};
pub use std::iter;
pub use std::iter::{repeat_with};
pub use std::marker::PhantomData;
pub use std::num::{NonZeroUsize, TryFromIntError, Wrapping};
pub use std::os::linux::fs::MetadataExt as _; // todo why linux for st_mode??
pub use std::os::unix;
pub use std::os::unix::ffi::OsStrExt;
pub use std::os::unix::fs::{MetadataExt, OpenOptionsExt};
pub use std::os::unix::io::{AsRawFd, IntoRawFd, RawFd};
pub use std::os::unix::net::UnixStream;
pub use std::os::unix::process::{CommandExt, ExitStatusExt};
pub use std::net::{IpAddr, SocketAddr, ToSocketAddrs, Ipv6Addr, Ipv4Addr};
pub use std::path::PathBuf;
pub use std::process::{exit, Child, Command, Stdio};
pub use std::str;
pub use std::str::FromStr;
pub use std::string::ParseError;
pub use std::sync::atomic::AtomicBool;
pub use std::sync::mpsc;
pub use std::thread::{self, sleep};
pub use std::time::{self, Duration, Instant};

pub use anyhow::{anyhow, ensure, Context};
pub use async_condvar_fair::{Condvar, BatonExt as _};
pub use boolinator::Boolinator as _;
pub use byteorder::{BigEndian, LittleEndian, ReadBytesExt, WriteBytesExt};
pub use cast_trait_object::{dyn_upcast, DynCastExt};
pub use const_default::ConstDefault;
pub use delegate::delegate;
pub use derive_into_owned::IntoOwned;
pub use digest::Digest;
pub use downcast_rs::{impl_downcast, Downcast};
pub use educe::Educe;
pub use either::{Either, Left, Right};
pub use enum_dispatch::enum_dispatch;
pub use enum_map::{Enum, EnumMap};
pub use fehler::{throw, throws};
pub use flexi_logger::LogSpecification;
pub use fs2::FileExt;
pub use index_vec::{define_index_type, index_vec, IndexSlice, IndexVec};
pub use lazy_regex::regex;
pub use lazy_static::lazy_static;
pub use log::{log, log_enabled};
pub use nix::unistd::{self, Uid};
pub use nix::sys::time::TimeSpec;
pub use nix::time::clock_gettime;
pub use num_derive::{ToPrimitive, FromPrimitive};
pub use num_traits::{Bounded, FromPrimitive, ToPrimitive};
pub use ordered_float::OrderedFloat;
pub use paste::paste;
pub use percent_encoding::percent_decode_str;
pub use percent_encoding::utf8_percent_encode;
pub use percent_encoding::NON_ALPHANUMERIC;
pub use rand::distributions::Alphanumeric;
pub use rand::thread_rng;
pub use rand::Rng;
pub use rand::prelude::SliceRandom;
pub use regex::Regex;
pub use sha2::{Sha512, Sha512_256};
pub use slotmap::{dense::DenseSlotMap, SparseSecondaryMap, Key as _};
pub use strum::{EnumCount, EnumDiscriminants};
pub use strum::{EnumString, EnumIter, EnumMessage, EnumProperty};
pub use strum::{AsRefStr, IntoEnumIterator, IntoStaticStr};
pub use subtle::ConstantTimeEq;
pub use tempfile::{self, NamedTempFile};
pub use tera::Tera;
pub use unicase::UniCase;
pub use url::Url;
pub use vecdeque_stableix::Deque as StableIndexVecDeque;
pub use vecdeque_stableix::Offset as StableIndexOffset;
pub use void::{unreachable, Void, ResultVoidExt, ResultVoidErrExt};
pub use crate::imports::zipfile::{self, read::ZipFile, result::ZipError};

use nix::time::ClockId;
pub const CLOCK_REALTIME : ClockId = ClockId::CLOCK_REALTIME ;
pub const CLOCK_MONOTONIC: ClockId = ClockId::CLOCK_MONOTONIC;

pub use otter_base::{pos_zip_map, pos_zip_try_map};
pub use otter_base::geometry::{self,Coord,Pos,PosC,Rect,RectC};
pub use otter_base::geometry::{CoordinateOverflow,Region};
pub use otter_base::zcoord::{self, ZCoord};
pub use otter_base::misc as base_misc;
pub use otter_base::dbgc;
pub use base_misc::*;

pub use crate::{deref_to_field, deref_to_field_mut};
pub use crate::ensure_eq;
pub use crate::format_by_fmt_hex;
pub use crate::impl_via_ambassador;
pub use crate::matches_doesnot;
pub use crate::trace_dbg;
pub use crate::{want, wantok, wants, want_let, want_failed_internal};
pub use crate::serde_with_compat;

pub use crate::accounts::loaded_acl::{self, EffectiveACL, LoadedAcl, PermSet};
pub use crate::accounts::*;
pub use crate::authproofs::{self, Authorisation, Unauthorised};
pub use crate::authproofs::AuthorisationSuperuser;
pub use crate::asseturl::*;
pub use crate::bundles::{self, InstanceBundles, MgmtBundleListExt};
pub use crate::childio;
pub use crate::commands::{AccessTokenInfo, AccessTokenReport, MgmtError};
pub use crate::commands::{MgmtCommand, MgmtResponse};
pub use crate::commands::{MgmtGameInstruction, MgmtGameResponse};
pub use crate::commands::{MgmtBundleList, MgmtGameUpdateMode};
pub use crate::commands::{ProgressUpdateMode};
pub use crate::config::*;
pub use crate::debugmutex::DebugIdentify;
pub use crate::debugreader::DebugReader;
pub use crate::digestrw::{self, *};
pub use crate::error::*;
pub use crate::fake_rng::*;
pub use crate::fake_time::*;
pub use crate::fastsplit::*;
pub use crate::gamestate::*;
pub use crate::global::*;
pub use crate::hidden::*;
pub use crate::keydata::*;
pub use crate::nwtemplates;
pub use crate::materials_format;
pub use crate::mgmtchannel::*;
pub use crate::occultilks::*;
pub use crate::organise;
pub use crate::packetframe::{FrameReader, FrameWriter, ReadFrame, WriteFrame};
pub use crate::packetframe::{ReadExt, ResponseWriter};
pub use crate::pcaliases::*;
pub use crate::pcrender::*;
pub use crate::pieces::*;
pub use crate::progress::{self, ProgressInfo, OriginatorExt as _};
pub use crate::shapelib;
pub use crate::shapelib::{CircleShape, RectShape};
pub use crate::shapelib::{ItemEnquiryData, LibraryEnquiryData};
pub use crate::shapelib::{LibraryLoadError};
pub use crate::slotmap_slot_idx::*;
pub use crate::spec::*;
pub use crate::spec::piece_specs::{FaceColourSpecs, SimpleCommon};
pub use crate::toml_de;
pub use crate::timedfd::*;
pub use crate::termprogress;
pub use crate::updates::*;
pub use crate::utils::*;
pub use crate::ui::*;

pub use crate::gamestate::RefTraitObjectPieceTraitExt as _;
pub use crate::fastsplit::RefTraitObjectPieceTraitExt as _;

pub type SecondarySlotMap<K,V> = slotmap::secondary::SecondaryMap<K,V>;
pub type SvgData = Vec<u8>;
pub type Colour = Html;

pub const MS: time::Duration = time::Duration::from_millis(1);

// ---------- type abbreviations ----------

pub type AE = anyhow::Error;

// accounts.rs
pub type AS = AccountScope;

// commands.rs
pub type MC = MgmtCommand;
pub type ME = MgmtError;
pub type MGI = MgmtGameInstruction;
pub type MGR = MgmtGameResponse;
pub type MR = MgmtResponse;
pub type PUM = ProgressUpdateMode;

// error.rs
pub type APOE = ApiPieceOpError;
pub type ESVU<POEPU,EM> = ErrorSignaledViaUpdate<POEPU,EM>;
pub type IE = InternalError;
pub type Ia = Inapplicable;
pub type POEPP = PieceOpErrorPartiallyProcessed;
pub type SvgE = SVGProcessingError;
pub type SpE = SpecError;

// gamestate.rs
pub use PieceLoadArgs as PLA;

// hidden.rs
pub type OccK = OccultationKind;
pub use OccultationKindGeneral as OccKG;
pub use OccultationKindAlwaysOk as OccKA;

// materials-format.rs

pub use materials_format::VersionError as MFVE;

// occultilks.rs
pub type LOI = LOccultIlk;
pub type IOI = IOccultIlk;

// pcrender.rs
pub use PriOccultedGeneral as PriOG;

// updates.rs
pub use OpOutcomeThunkGeneric as OOTG;
pub type PUE = PreparedUpdateEntry;
pub type PUFOS = PieceUpdateFromOpSimple;
pub type PUO<NS,ZL> = PieceUpdateOp<NS,ZL>;
pub type PUOs = PieceUpdateOps;
pub type WRC = WhatResponseToClientOp;
#[allow(non_camel_case_types)] pub type PUE_P = PreparedUpdateEntry_Piece;

// utils.rs
pub use SVGSizeError as SvSE;
