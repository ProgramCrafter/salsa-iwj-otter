
pub use std::io;
pub use std::io::{BufReader,Read,BufRead,BufWriter,Write};
pub use std::io::ErrorKind;
pub use std::fmt::Write as _;
pub use std::fmt::Formatter;
pub use std::fmt::{self,Display,Debug};
pub use std::thread;
pub use std::time::Duration;
pub use std::sync::{Arc,Mutex,MutexGuard,RwLock,Condvar};
pub use std::collections::{HashMap,hash_map,HashSet};
pub use std::borrow::Borrow;
pub use std::convert::{TryFrom,TryInto};
pub use std::str;
pub use std::str::FromStr;
pub use std::iter;
pub use std::iter::repeat_with;
pub use std::collections::VecDeque;
pub use std::num::Wrapping;
pub use std::cmp;
pub use std::error::Error;
pub use std::marker::PhantomData;
pub use std::ops::{Deref,DerefMut};
pub use std::fs;
pub use std::fs::File;
pub use std::mem;
pub use std::os::unix;
pub use std::time::Instant;
pub use std::path::PathBuf;
pub use std::os::unix::ffi::OsStrExt;
pub use std::env;
pub use std::process::exit;

pub use thiserror::Error;
pub use anyhow::{Context,anyhow};
pub use fehler::{throws,throw};

pub use serde::{Serialize,Deserialize,de::DeserializeOwned};
pub use serde::{Serializer,Deserializer};

pub use rocket_contrib::helmet::*;
pub use rocket_contrib::templates::Template;

pub use percent_encoding::utf8_percent_encode;
pub use percent_encoding::percent_decode_str;
pub use percent_encoding::NON_ALPHANUMERIC;

pub use rocket::{State,Rocket};
pub use rocket::http::{RawStr,ContentType};
pub use rocket::request::{FromParam,FromRequest,FromFormValue,LenientForm};
pub use rocket::response::NamedFile;
pub use rocket::response;

pub use rand::thread_rng;
pub use rand::Rng;
pub use rand::distributions::Alphanumeric;

pub use slotmap::dense::DenseSlotMap;
pub type SecondarySlotMap<K,V> = slotmap::secondary::SecondaryMap<K,V>;
pub use index_vec::{define_index_type,index_vec,IndexVec,IndexSlice};

pub use vecdeque_stableix::StableIndexVecDeque;

pub use fs2::FileExt;
pub use lazy_static::lazy_static;
pub use regex::Regex;

pub use arrayvec::ArrayVec;

pub use crate::global::*;
pub use crate::gamestate::*;
pub use crate::pieces::*;
pub use crate::keydata::*;
pub use crate::updates::*;
pub use crate::sse;
pub use crate::error::*;
pub use crate::commands::*;
pub use crate::slotmap_slot_idx::*;
pub use crate::cmdlistener::*;
pub use crate::mgmtchannel::*;
pub use crate::api::{Lens,TransparentLens};
pub use crate::utils::*;
pub use crate::spec::*;

pub use nix::unistd::Uid;

pub type E = anyhow::Error;
pub type AE = anyhow::Error;

pub type OE = OnlineError;

pub type SvgData = Vec<u8>;
pub type Colour = String;
