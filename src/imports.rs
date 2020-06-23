
pub use std::io;
pub use std::io::{BufReader,Read};
pub use std::thread;
pub use std::time::Duration;
pub use std::sync::{Arc,Mutex,RwLock};
pub use std::collections::HashMap;
pub use std::borrow::Borrow;

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

pub use slotmap::dense::{DenseSlotMap};

pub use crate::global;
pub use crate::global::{lookup_token,InstanceAccess,InstanceAccessDetails};
pub use crate::global::{Client,ClientId};

pub type E = anyhow::Error;

pub type SvgData = Vec<u8>;
pub type Coord = isize;
pub type Pos = [Coord; 2];
pub type Colour = String;
