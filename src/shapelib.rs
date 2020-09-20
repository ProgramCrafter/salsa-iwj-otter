// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use crate::imports::*;

// Naming convention:
//  *Data, *List   from toml etc. (processed if need be)
//  *Defn          raw read from library toml file (where different from Info)
//  *Details       some shared structure
//  Item           } once loaded and part of a game,
//  Outline        }  no Arc's as we serialise/deserialize during save/load

#[derive(Debug)]
pub struct Contents {
  dirname: String,
  items: HashMap<String /* item (name) */, ItemData>,
}

#[derive(Debug,Clone)]
#[derive(Serialize,Deserialize)]
pub struct ItemDetails {
  desc: Html,
}

#[derive(Debug,Clone)]
pub struct ItemData {
  d: Arc<ItemDetails>,
  group: Arc<GroupData>,
}

#[derive(Debug,Deserialize)]
pub struct GroupDetails {
  size: Vec<f64>,
  category: String,
  #[serde(default)] centre: [f64; 2],
  #[serde(default)] flip: bool,
  #[serde(default="num_traits::identities::One::one")] scale: f64,
  #[serde(flatten)] outline: Box<dyn OutlineDefn>,
}

#[derive(Debug)]
pub struct GroupData {
  groupname: String,
  d: GroupDetails,
}

#[derive(Debug,Deserialize)]
struct GroupDefn {
  files: FileList,
  #[serde(default)] item_prefix: String,
  #[serde(default)] item_suffix: String,
  #[serde(flatten)] info: Arc<GroupDetails>,
}

#[derive(Deserialize,Debug)]
#[serde(try_from="String")]
struct FileList (Vec<FileData>);

#[derive(Deserialize,Debug)]
struct FileData {
  item_spec: String,
  r_file_spec: (), // string, in the actual source file
  desc: Html,
}

type IE = InternalError;

#[typetag::deserialize(tag="outline")]
trait OutlineDefn : Debug + Sync + Send {
  fn check(&self, lgi: &GroupData) -> Result<(),LLE>;
  fn load(&self, lgi: &GroupData) -> Result<Box<dyn Outline>,IE>;
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
  DuplicateItem { item: String, group1: String, group2: String },
  #[error("{:?}",&self)]
  FilesListLineMissingWhitespace(usize),
}

const INHERIT_DEPTH_LIMIT : u8 = 20;

type LLE = LibraryLoadError;
type TV = toml::Value;
type SE = SpecError;

#[derive(Debug,Serialize,Deserialize)]
pub struct ItemSpec {
  lib: String,
  item: String,
}

define_index_type!{ pub struct DescId = u8; }

#[derive(Debug,Serialize,Deserialize)]
struct ItemFace {
  svg: Html,
  desc: DescId,
  centre: [f64; 2],
  scale: f64,
}

#[derive(Debug,Serialize,Deserialize)]
struct Item {
  faces: IndexVec<FaceId, ItemFace>,
  desc_hidden: DescId,
  descs: IndexVec<DescId, Html>,
  outline: Box<dyn Outline>,
}

#[typetag::serde(name="Lib")]
impl Outline for Item { delegate! { to self.outline {
  fn surround_path(&self, pri : &PieceRenderInstructions) -> Result<Html, IE>;
  fn thresh_dragraise(&self, pri : &PieceRenderInstructions)
                      -> Result<Option<Coord>, IE>;
}}}

#[typetag::serde(name="Lib")]
impl Piece for Item {
  #[throws(SpecError)]
  fn resolve_spec_face(&self, face: Option<FaceId>) -> FaceId {
    let face = face.unwrap_or_default();
    self.faces.get(face).ok_or(SpecError::FaceNotFound)?;
    face
  }

  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, pri: &PieceRenderInstructions) {
    let face = &self.faces[pri.face];
    write!(&mut f.0,
           r##"<g transform="scale({}) translate({} {})">{}</g>"##,
           face.scale, face.centre[0], face.centre[1],
           face.svg.0)?;
  }
  #[throws(IE)]
  fn svg_x_defs(&self, _f: &mut Html, _pri : &PieceRenderInstructions) {
  }
  fn describe_html(&self, face : Option<FaceId>) -> Html {
    Html(format!("a {}", self.descs[ match face {
      Some(face) => self.faces[face].desc,
      None => self.desc_hidden,
    }]))
  }
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
impl ItemSpec {
  fn load(&self) -> Result<Box<dyn Piece>,SpecError> {
    let libs = GLOBAL.shapelibs.read().unwrap(); 
    let lib = libs.get(&self.lib).ok_or(SE::LibraryNotFound)?;
    let lii = lib.items.get(&self.item).ok_or(SE::LibraryItemNotFound)?;

    let svg_path = format!("{}/{}", lib.dirname, &self.item);
    let svg_data = fs::read_to_string(&svg_path)
      .map_err(|e| if e.kind() == ErrorKind::NotFound {
        SE::LibraryItemNotFound
      } else {
        let m = "error accessing/reading library item data file";
        error!("{}: {}: {}", &m, &svg_path, &e);
        SE::InternalError(m.to_string())
      })?;

    let o_checked = lii.group.outline.check(&lii.group)
      .map_err(|e| SE::InternalError(format!("rechecking outline: {}",&e)))?;
    let outline = lii.group.outline.load(&lii.group)?;

    // xxx do something with flip

    let descs = index_vec![ ];
    let desc = descs.push_back(lii.info.desc.clone());
    let face = ItemFace { svg: Html(svg_data), desc };
    let faces = index_vec![ face ];
    let it = Item { faces, descs, outline };
    Box::new(it);
    panic!();
  }
}

#[typetag::serde(name="Lib")]
impl PieceSpec for ItemSpec {
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
fn load_catalogue(dirname: String) -> Contents {
  let toml_path = format!("{}.toml", &dirname);
  let f = File::open(toml_path)?;
  let mut f = BufReader::new(f);
  let mut s = String::new();
  f.read_to_string(&mut s).unwrap();
  let toplevel : toml::Value = s.parse()?;
  let mut l = Contents {
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
    let spec : GroupDefn = resolved.try_into()?;
    for fe in spec.files.0 {
      let item = format!("{}{}{}.usvg", spec.item_prefix,
                         fe.item_spec, spec.item_suffix);
      let details = Arc::new(ItemDetails { desc: fe.desc });
      let lp = ItemData { info: spec.info.clone(), details };
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

#[derive(Serialize,Deserialize,Debug)]
struct Circle { diam: f64 }

#[typetag::serde(name="Circle")]
impl Outline for Circle {
  #[throws(IE)]
  fn surround_path(&self, _pri : &PieceRenderInstructions) -> Html {
    svg_circle_path(self.diam * SELECT_SCALE)?
  }
  #[throws(IE)]
  fn thresh_dragraise(&self, _pri : &PieceRenderInstructions)
                      -> Option<Coord> {
    Some(self.diam / 2)
  }
}

#[derive(Deserialize,Debug)]
struct CircleDefn { }
#[typetag::deserialize(name="Circle")]
impl OutlineDefn for CircleDefn {
  #[throws(LibraryLoadError)]
  fn check(&self, lgd: &GroupData) { Self::get_size(lgd)?; }
  #[throws(InternalError)]
  fn load(&self, lgd: &GroupData) -> Box<dyn Outline> {
    Box::new(Circle {
      diam: Self::get_size(lgd).unrap()
    })
  }
}
impl CircleDefn {
  #[throws(LibraryLoadError)]
  fn get_size(lgd: &GroupData) -> f64 {
    match lgd.size.as_slice() {
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
      o.push(FileData{ item_spec, r_file_spec, desc  });
    }
    Ok(FileList(o))
  }
}
