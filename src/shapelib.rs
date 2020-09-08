// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use crate::imports::*;

#[derive(Deserialize,Debug)]
#[serde(transparent)]
pub struct Library {
  pub sections: LinkedHashMap<String, Section>,
}

#[derive(Deserialize,Debug)]
pub struct Section {
  pub shape: Box<dyn OutlineSpec>,
  pub size: Vec<Coord>,
  pub middle: Option<Vec<f64>>,
  pub category: String,
  pub files: FileList,
  pub scraper: toml::Value,
}

#[derive(Deserialize,Debug)]
pub struct FileList (Vec<FileEntry>);

#[derive(Deserialize,Debug)]
pub struct FileEntry {
  pub filespec: String,
  pub desc: Html,
}

#[typetag::deserialize(tag="outline")]
pub trait OutlineSpec : Debug {
}

#[derive(Deserialize,Debug)]
pub struct Circle { }
#[typetag::deserialize]
impl OutlineSpec for Circle { }
