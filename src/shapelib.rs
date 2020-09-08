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
  pub outline: Box<dyn OutlineSpec>,
  pub size: Vec<Coord>,
  pub middle: Option<Vec<f64>>,
  pub category: String,
  pub files: FileList,
  pub scraper: Option<toml::Value>,
}

#[derive(Deserialize,Debug)]
#[serde(try_from="&str")]
pub struct FileList (Vec<FileEntry>);

#[derive(Deserialize,Debug)]
pub struct FileEntry {
  pub filespec: String,
  pub desc: Html,
}

#[typetag::deserialize]
pub trait OutlineSpec : Debug {
}

#[derive(Error,Debug)]
pub enum LibraryLoadError{ 
  #[error(transparent)]
  TomlParseError(#[from] toml::de::Error),
  #[error("error reading/opening library file: {0}")]
  FileError(#[from] io::Error),
  #[error("{:?}",&self)]
  FilesListLineMissingWhitespace(usize),
}

impl Library {
  #[throws(LibraryLoadError)]
  pub fn load(path: &str) -> Library {
    let f = File::open(path)?;
    let mut f = BufReader::new(f);
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    let l : shapelib::Library = toml::from_str(&s)?;
    l
  }
}

type LLE = LibraryLoadError;

#[derive(Deserialize,Debug)]
struct Circle { }
#[typetag::deserialize]
impl OutlineSpec for Circle { }

impl TryFrom<&str> for FileList {
  type Error = LLE;
  #[throws(LLE)]
  fn try_from(s: &str) -> FileList {
    let mut o = Vec::new();
    for (lno,l) in s.lines().enumerate() {
      let l = l.trim();
      if l=="" || l.starts_with("#") { continue }
      let sp = l.find(|c:char| c.is_ascii_whitespace())
        .ok_or(LLE::FilesListLineMissingWhitespace(lno))?;
      let (lhs, rhs) = l.split_at(sp);
      let rhs = rhs.trim();
      o.push(FileEntry{
        filespec: lhs.to_owned(),
        desc: Html(rhs.to_owned()),
      });
    }
    FileList(o)
  }
}
