// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// See Import Structure Doctrine in src/prelude.rs

use crate::crates::*;
use otter_support::crates::*;
use otter_base::crates::*;

use otter_base::imports::*;

pub use std::any::Any;
pub use std::convert::{Infallible, TryFrom, TryInto};
pub use std::error::Error;
pub use std::ffi::OsStr;
pub use std::fmt::Formatter;
pub use std::fmt::Write as _;
pub use std::fmt::{self, Debug, Display};
pub use std::hash::Hash;
pub use std::iter;
pub use std::iter::{repeat_with};
pub use std::num::{NonZeroUsize, TryFromIntError, Wrapping};
pub use std::path::PathBuf;
pub use std::str;
pub use std::str::FromStr;
pub use std::string::ParseError;
pub use std::sync::atomic::AtomicBool;
pub use std::sync::mpsc;
pub use std::thread::{self, sleep};

pub use async_condvar_fair::{Condvar, BatonExt as _};
pub use boolinator::Boolinator as _;
pub use cast_trait_object::{dyn_upcast, DynCastExt};
pub use const_default::ConstDefault;
pub use delegate::delegate;
pub use downcast_rs::{impl_downcast, Downcast};
pub use educe::Educe;
pub use either::{Either, Left, Right};
pub use enum_dispatch::enum_dispatch;
pub use enum_map::{Enum, EnumMap};
pub use fehler::{throw, throws};
pub use index_vec::{define_index_type, index_vec, IndexSlice, IndexVec};
pub use lazy_regex::regex;
pub use ordered_float::OrderedFloat;
pub use percent_encoding::percent_decode_str;
pub use percent_encoding::utf8_percent_encode;
pub use percent_encoding::NON_ALPHANUMERIC;
pub use regex::Regex;
pub use slotmap::{dense::DenseSlotMap, SparseSecondaryMap, Key as _};
pub use subtle::ConstantTimeEq;
pub use tempfile::{self, NamedTempFile};
pub use tera::Tera;
pub use unicase::UniCase;
pub use url::Url;
pub use vecdeque_stableix::Deque as StableIndexVecDeque;
pub use vecdeque_stableix::Offset as StableIndexOffset;
pub use void::{unreachable, Void, ResultVoidExt, ResultVoidErrExt};
pub use crate::crates::zipfile::{self, read::ZipFile, result::ZipError};

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
pub use crate::{want, wantok, wants, want_let, want_failed_internal};
pub use crate::serde_with_compat;

pub use crate::accounts::loaded_acl::{self, EffectiveACL, LoadedAcl, PermSet};
pub use crate::accounts::*;
pub use crate::asseturl::*;
pub use crate::bundles::{self, InstanceBundles, MgmtBundleListExt};
pub use crate::commands::{AccessTokenInfo, AccessTokenReport, MgmtError};
pub use crate::commands::{MgmtCommand, MgmtResponse};
pub use crate::commands::{MgmtGameInstruction, MgmtGameResponse};
pub use crate::commands::{MgmtBundleList, MgmtGameUpdateMode};
pub use crate::commands::{ProgressUpdateMode};
pub use crate::debugreader::DebugReader;
pub use crate::error::*;
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
pub use crate::pcaliases::*;
pub use crate::pcrender::*;
pub use crate::pieces::*;
pub use crate::shapelib;
pub use crate::shapelib::{CircleShape, RectShape};
pub use crate::shapelib::{ItemEnquiryData, LibraryEnquiryData};
pub use crate::shapelib::{LibraryLoadError};
pub use crate::slotmap_slot_idx::*;
pub use crate::spec::*;
pub use crate::spec::piece_specs::{FaceColourSpecs, SimpleCommon};
pub use crate::updates::*;
pub use crate::utils::*;
pub use crate::ui::*;

pub use crate::gamestate::RefTraitObjectPieceTraitExt as _;
pub use crate::fastsplit::RefTraitObjectPieceTraitExt as _;

pub type SecondarySlotMap<K,V> = slotmap::secondary::SecondaryMap<K,V>;
pub type SvgData = Vec<u8>;
pub type Colour = Html;

// ---------- type abbreviations ----------

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
