
pub use std::io;
pub use std::io::{BufReader,Read};
pub use std::fmt::{self,Display,Debug};
pub use std::thread;
pub use std::time::Duration;
pub use std::sync::{Arc,Mutex,RwLock};
pub use std::collections::HashMap;
pub use std::borrow::Borrow;
pub use std::convert::TryFrom;
pub use std::str::FromStr;

pub use thiserror::Error;
pub use anyhow::{Context,anyhow};
pub use fehler::{throws,throw};

pub use serde::Deserialize;
pub use serde::Serialize;
pub use serde::Serializer;

pub use rocket_contrib::helmet::*;
pub use rocket_contrib::templates::Template;

pub use rocket::State;
pub use rocket::http::{Status,RawStr,ContentType};
pub use rocket::request::{FromParam,FromRequest,FromFormValue,LenientForm};
pub use rocket::response::NamedFile;
pub use rocket::response;

pub use slotmap::dense::DenseSlotMap;
pub type SecondarySlotMap<K,V> = slotmap::secondary::SecondaryMap<K,V>;
pub use index_vec::{define_index_type,index_vec,IndexVec};

pub use crate::global::*;
pub use crate::gamestate::*;
pub use crate::pieces::*;
pub use crate::keydata::*;
pub use crate::updates::*;

pub type E = anyhow::Error;
pub type AE = anyhow::Error;

pub type SvgData = Vec<u8>;
pub type Coord = isize;
pub type Pos = [Coord; 2];
pub type Colour = String;
