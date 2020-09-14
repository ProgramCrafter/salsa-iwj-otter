// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use crate::imports::*;

#[derive(Debug)]
pub struct LibraryContents {
  pub directory: String,
  pub pieces: HashMap<String /* usvg path */, LibraryPieceInfo>,
}

#[derive(Debug)]
pub struct LibraryPieceInfo {
  pub desc: Html,
  pub info: Arc<LibraryGroupInfo>,
}

#[derive(Debug,Deserialize)]
pub struct LibraryGroupInfo {
  pub outline: Box<dyn OutlineSpec>,
  pub size: Vec<Coord>,
  #[serde(default="num_traits::identities::One::one")]
  pub scale: f64,
  #[serde(default)]
  pub centre: Option<Vec<f64>>,
  pub category: String,
}

#[derive(Debug,Deserialize)]
struct LibraryGroupSpec {
  #[serde(flatten)]
  pub info: Arc<LibraryGroupInfo>,
  #[serde(default)]
  pub stem_prefix: String,
  #[serde(default)]
  pub stem_suffix: String,
  pub files: FileList,
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
  ExpectedTable(String),
  #[error("{:?}",&self)]
  ExpectedString(String),
  #[error("{:?}",&self)]
  InheritMissingParent(String,String),
  #[error("{:?}",&self)]
  InheritDepthLimitExceeded(String),
  #[error("{:?}",&self)]
  DuplicateFile(String,LibraryPieceInfo,LibraryPieceInfo),
  #[error("{:?}",&self)]
  FilesListLineMissingWhitespace(usize),
}

const INHERIT_DEPTH_LIMIT : u8 = 20;

type LLE = LibraryLoadError;

#[throws(LibraryLoadError)]
fn resolve_inherit<'r>(depth: u8, groups: &toml::value::Table,
                       group_name: &str, group: &'r toml::Value)
                       -> Cow<'r, toml::value::Table> {
  let gn = || format!("{}", group_name);
  let gp = || format!("group.{}", group_name);

  let group = group.as_table().ok_or_else(|| LLE::ExpectedTable(gp()))?;

  let parent_name = match group.get("inherit") {
    None => { return Cow::Borrowed(group) },
    Some(p) => p,
  };
  let parent_name = parent_name
    .as_str().ok_or_else(|| LLE::ExpectedString(format!("group.{}.inherit",
                                                        group_name)))?;
  let parent = groups.get(parent_name)
    .ok_or_else(|| LLE::InheritMissingParent(gn(), parent_name.to_string()))?;

  let mut build = resolve_inherit(
    depth.checked_sub(1).ok_or_else(|| LLE::InheritDepthLimitExceeded(gn()))?,
    groups, parent_name, parent
  )?.into_owned();

  build.extend(group.iter().map(|(k,v)| (k.clone(), v.clone())));
  Cow::Owned(build)
}

impl LibraryContents {
  #[throws(LibraryLoadError)]
  pub fn load(directory: String) -> LibraryContents {
    let toml_path = format!("{}.toml", &directory);
    let f = File::open(toml_path)?;
    let mut f = BufReader::new(f);
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    let toplevel : toml::Value = s.parse()?;
    let mut l = LibraryContents {
      pieces: HashMap::new(),
      directory,
    };
    let empty_table = toml::value::Value::Table(Default::default());
    let groups =
      toplevel
      .as_table().ok_or_else(|| LLE::ExpectedTable(format!("toplevel")))?
      .get("group").unwrap_or(&empty_table)
      .as_table().ok_or_else(|| LLE::ExpectedTable(format!("group")))?;
    for (group_name, group_value) in groups {
      let resolved = resolve_inherit(INHERIT_DEPTH_LIMIT,
                                     &groups, group_name, group_value)?;
      let spec : LibraryGroupSpec = resolved.as_ref().try_into()?;
      for fe in spec.files {
        let usvgfile = format!("{}{}{}.usvg", spec.stem_prefix,
                               fe.filespec, spec.stem_suffix);
        let lp = LibraryPieceInfo { info: spec.info, desc: fe.desc };
        type H<'e,X,Y> = hash_map::Entry<'e,X,Y>;
        match l.entry(usvgfile) {
          H::Occupied(oe) => throw!(LLE::DuplicateFile(
            oe.key().clone(),
            oe.get().clone(),
            lp,
          )),
          H::Vacant(ve) => ve.insert(lp),
        };
      }
    }
    l
  }
}

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
