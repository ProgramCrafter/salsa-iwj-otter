// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use std::borrow::Borrow;
pub use std::borrow::Cow;
pub use std::cmp::{self, min, max, Ordering};
pub use std::collections::VecDeque;
pub use std::collections::{HashMap, hash_map, HashSet};
pub use std::convert::{TryFrom, TryInto};
pub use std::env;
pub use std::error::Error;
pub use std::fmt::Formatter;
pub use std::fmt::Write as _;
pub use std::fmt::{self, Display, Debug};
pub use std::fs::File;
pub use std::fs;
pub use std::hash::Hash;
pub use std::io;
pub use std::io::ErrorKind;
pub use std::io::{BufReader, Read, BufRead, BufWriter, Write};
pub use std::io::{SeekFrom};
pub use std::iter::repeat_with;
pub use std::iter;
pub use std::marker::PhantomData;
pub use std::mem;
pub use std::num::{Wrapping, TryFromIntError};
pub use std::ops::{Deref, DerefMut};
pub use std::os::unix::ffi::OsStrExt;
pub use std::os::unix;
pub use std::path::PathBuf;
pub use std::process::{exit, Command};
pub use std::str::FromStr;
pub use std::str;
pub use std::string::ParseError;
pub use std::sync::{Arc, Mutex, MutexGuard, Condvar};
pub use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};
pub use std::thread::{self, sleep};
pub use std::time::Duration;
pub use std::time::Instant;

pub use anyhow::{Context, anyhow};
pub use arrayvec::ArrayVec;
pub use boolinator::Boolinator as _;
pub use delegate::delegate;
pub use either::{Either, Left, Right};
pub use fehler::{throws, throw};
pub use flexi_logger::{self, LogSpecification};
pub use fs2::FileExt;
pub use if_chain::if_chain;
pub use index_vec::{define_index_type, index_vec, IndexVec, IndexSlice};
pub use itertools::{izip, EitherOrBoth, Itertools};
pub use lazy_static::lazy_static;
pub use log::{log, log_enabled};
pub use log::{trace, debug, info, warn, error};
pub use nix::unistd::Uid;
pub use num_traits::{Bounded, FromPrimitive, ToPrimitive};
pub use ordered_float::OrderedFloat;
pub use percent_encoding::NON_ALPHANUMERIC;
pub use percent_encoding::percent_decode_str;
pub use percent_encoding::utf8_percent_encode;
pub use rand::Rng;
pub use rand::distributions::Alphanumeric;
pub use rand::thread_rng;
pub use regex::Regex;
pub use rocket::http::Status;
pub use rocket::http::{RawStr, ContentType};
pub use rocket::request::Request;
pub use rocket::request::{FromParam, FromRequest, FromFormValue, LenientForm};
pub use rocket::response::NamedFile;
pub use rocket::response::{Response, Responder};
pub use rocket::response;
pub use rocket::{State, Rocket};
pub use rocket::{post, get, routes};
pub use rocket_contrib::helmet::*;
pub use rocket_contrib::json::Json;
pub use rocket_contrib::templates::Template;
pub use rocket_contrib::templates::Engines;
pub use rocket_contrib::templates::tera::{self, Value};
pub use serde::ser::SerializeTuple;
pub use serde::{Serialize, Deserialize, de::DeserializeOwned};
pub use serde::{Serializer, Deserializer};
pub use serde_with::DeserializeFromStr;
pub use serde_with::SerializeDisplay;
pub use slotmap::dense::DenseSlotMap;
pub use strum::EnumString;
pub use thiserror::Error;
pub use vecdeque_stableix::Deque as StableIndexVecDeque;
pub use zcoord::{self, ZCoord};

pub use crate::accounts::*;
pub use crate::accounts::loaded_acl::{self,LoadedAcl,EffectiveACL,PermSet};
pub use crate::api::{Lens,TransparentLens,ApiPieceOpError};
pub use crate::api::{PresentationLayout,AbbrevPresentationLayout};
pub use crate::cmdlistener::*;
pub use crate::commands::*;
pub use crate::config::*;
pub use crate::debugreader::DebugReader;
pub use crate::error::*;
pub use crate::gamestate::*;
pub use crate::global::*;
pub use crate::keydata::*;
pub use crate::mgmtchannel::*;
pub use crate::nwtemplates;
pub use crate::pieces::*;
pub use crate::shapelib;
pub use crate::slotmap_slot_idx::*;
pub use crate::spec::*;
pub use crate::sse;
pub use crate::toml_de;
pub use crate::tz::*;
pub use crate::updates::*;
pub use crate::utils::*;

pub type SecondarySlotMap<K,V> = slotmap::secondary::SecondaryMap<K,V>;
pub type SvgData = Vec<u8>;
pub type Colour = Html;

pub fn default<T:Default>() -> T { Default::default() }

#[derive(Debug,Copy,Clone)]
pub enum Impossible { }
display_as_debug!(Impossible);

pub type AE = anyhow::Error;
pub type OE = OnlineError;
