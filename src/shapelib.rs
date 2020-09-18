// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use crate::imports::*;

// Naming convention:
//  *Defn          read from library toml file
//  *Info, *List   from toml etc. but processed
//  *Details       some shared structure
//  Item           } once loaded and part of a game,
//  Outline        }  no Arc's as we serialise/deserialize during save/load

#[derive(Debug)]
pub struct ContentsDefn {
  dirname: String,
  items: HashMap<String /* item (name) */, LibraryItemInfo>,
}

#[derive(Debug,Clone)]
#[derive(Serialize,Deserialize)]
pub struct ItemDetails {
  desc: Html,
}

#[derive(Debug,Clone)]
pub struct LibraryItemDefn { // xxx ???
  details: Arc<LibraryItemDetails>,
  info: Arc<LibraryGroupDefn>,
}

#[derive(Debug,Deserialize,Serialize)]
pub struct GroupInfo {
  outline: Box<dyn OutlineDefn>,
  size: Vec<Coord>,
  #[serde(default="num_traits::identities::One::one")]
  scale: f64,
  #[serde(default)]
  centre: Option<Vec<f64>>,
  category: String,
}

#[derive(Debug,Deserialize)]
struct GroupDefn {
  #[serde(default)] item_prefix: String,
  #[serde(default)] item_suffix: String,
  #[serde(default)] stem_prefix: String,
  #[serde(default)] stem_suffix: String,
  #[serde(default)] flip: bool,
  files: FileList,
  #[serde(flatten)] info: Arc<LibraryGroupInfo>,
}

#[derive(Deserialize,Debug)]
#[serde(try_from="String")]
#[derive(Serialize)] // xxx
struct FileList (Vec<FileInfo>);

#[derive(Deserialize,Debug)]
#[derive(Serialize)] // xxx
struct FileInfo {
  item_spec: String,
  r_file_spec: String,
  desc: Html,
}

trait Outline { }

//#[typetag::deserialize]
#[typetag::serde] // xxx
trait OutlineDefn : Debug + Sync + Send {
  fn check(&self, lgi: &LibraryGroupInfo) -> Result<(),LibraryLoadError>;
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
  WrongNumberOfSizeDimensions { got: usize, expected: usize },
  #[error("{:?}",&self)]
  InheritMissingParent(String,String),
  #[error("{:?}",&self)]
  InheritDepthLimitExceeded(String),
  #[error("{:?}",&self)]
  DuplicateItem(String,LibraryItemInfo,LibraryItemInfo),
  #[error("{:?}",&self)]
  FilesListLineMissingWhitespace(usize),
}

const INHERIT_DEPTH_LIMIT : u8 = 20;

type LLE = LibraryLoadError;
type TV = toml::Value;
type SE = SpecError;

#[derive(Debug,Serialize,Deserialize)]
pub struct LibPieceSpec { // xxx rename, see above
  lib: String,
  item: String,
}

#[derive(Debug,Serialize,Deserialize)]
struct Item { // xxx totally redo the contents
  svg: Html,
  details: Arc<LibraryItemDetails>,
  info: Arc<LibraryGroupInfo>,
}

/*
#[typetag::serde(name="LP")]
impl Item for LibraryItem {
}
*/

/*
#[typetag::serde(name="Lib")]
impl PieceSpec for LibPieceSpec {
*/
impl LibPieceSpec {
  fn load(&self) -> Result<Box<dyn Piece>,SpecError> {
    let libs = GLOBAL.shapelibs.read().unwrap(); 
    let lib = libs.get(&self.lib)
      .ok_or(SE::LibraryNotFound)?;
    let lii = lib.items.get(&self.item)
      .ok_or(SE::LibraryItemNotFound)?;
    let svg_path = format!("{}/{}", lib.dirname, &self.item);
    let omg : &dyn Fn(&io::Error, _) -> SE = &|e,m:&str| {
      error!("{}: {}: {}", &m, &svg_path, &e);
      SE::InternalError(m.to_string())
    };
    let mut f = File::open(&svg_path)
      .map_err(|e| if e.kind() == ErrorKind::NotFound {
        SE::LibraryItemNotFound
      } else {
        omg(&e, "unable to access library itme data file")
      }
      )?;
    let mut svg_data = String::new();
    f.read_to_string(&mut svg_data).map_err(
      |e| omg(&e, "unable to read library item data file")
    )?;
    let lp = LibraryItem {
      svg: Html(svg_data),
      details: lii.details.clone(),
      info:    lii.info.clone(),
    };
    Box::new(lp);
    panic!();
  }
}

#[typetag::serde(name="Lib")]
impl PieceSpec for LibPieceSpec {
  fn load(&self) -> Result<Box<dyn Piece>,SpecError> {
    self.load()
  }
}

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

#[throws(LibraryLoadError)]
fn load_catalogue(dirname: String) -> LibraryContents {
  let toml_path = format!("{}.toml", &dirname);
  let f = File::open(toml_path)?;
  let mut f = BufReader::new(f);
  let mut s = String::new();
  f.read_to_string(&mut s).unwrap();
  let toplevel : toml::Value = s.parse()?;
  let mut l = LibraryContents {
    items: HashMap::new(),
    dirname,
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
    let resolved = TV::Table(resolved.into_owned());
    let spec : LibraryGroupSpec = resolved.try_into()?;
    for fe in spec.files.0 {
      let item = format!("{}{}{}.usvg", spec.item_prefix,
                         fe.item_spec, spec.item_suffix);
      let details = Arc::new(LibraryItemDetails { desc: fe.desc });
      let lp = LibraryItemInfo { info: spec.info.clone(), details };
      type H<'e,X,Y> = hash_map::Entry<'e,X,Y>;
      match l.items.entry(item) {
        H::Occupied(oe) => throw!(LLE::DuplicateItem(
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

#[throws(LibraryLoadError)]
pub fn load(libname: String, dirname: String) {
  let data = load_catalogue(dirname.clone())?;
  dbg!(&data);
  GLOBAL.shapelibs.write().unwrap().insert(libname.clone(), data);
  info!("loaded library {:?} from {:?}", libname, dirname);
}

#[derive(Deserialize,Debug)]
#[derive(Serialize)] // xxx
struct Circle { }
//#[typetag::deserialize]
#[typetag::serde] // xxx
impl OutlineSpec for Circle {
  #[throws(LibraryLoadError)]
  fn check(&self, lgi: &LibraryGroupInfo) {
    Self::get_size(lgi)?;
  }
}
impl Circle {
  #[throws(LibraryLoadError)]
  fn get_size(lgi: &LibraryGroupInfo) -> Coord {
    match lgi.size.as_slice() {
      &[c] => c,
      size => throw!(LLE::WrongNumberOfSizeDimensions
                     { got: size.len(), expected : 1 }),
    }
  }
}

impl TryFrom<String> for FileList {
  type Error = LLE;
//  #[throws(LLE)]
  fn try_from(s: String) -> Result<FileList,LLE> {
    let mut o = Vec::new();
    for (lno,l) in s.lines().enumerate() {
      let l = l.trim();
      if l=="" || l.starts_with("#") { continue }
      let mut words = l.splitn(3, |c:char| c.is_ascii_whitespace());
      let mut n = ||{
        words.next().ok_or(LLE::FilesListLineMissingWhitespace(lno))
          .map(|s| s.to_owned())
      };
      let item_spec = n()?;
      let r_file_spec = n()?;
      let desc = Html(n()?);
      assert!(!n().is_err());
      o.push(FileEntry{ item_spec, r_file_spec, desc  });
    }
    Ok(FileList(o))
  }
}
