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
struct ItemDetails {
  desc: Html,
}

#[derive(Debug,Clone)]
struct ItemData {
  d: Arc<ItemDetails>,
  group: Arc<GroupData>,
}

#[derive(Debug,Deserialize)]
struct GroupDetails {
  size: Vec<f64>, // scaled when put into GroupData
  #[serde(default)] centre: [f64; 2],
  #[serde(default)] flip: bool,
  #[serde(default="num_traits::identities::One::one")] scale: f64,
  #[serde(flatten)] outline: Box<dyn OutlineDefn>,
}

#[derive(Debug)]
struct GroupData {
  groupname: String,
  d: GroupDetails,
}

#[derive(Debug,Deserialize)]
struct GroupDefn {
  files: FileList,
  #[serde(default)] item_prefix: String,
  #[serde(default)] item_suffix: String,
  #[serde(flatten)] d: GroupDetails,
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
  #[error("error reading/opening library file: {0}: {1}")]
  FileError(String, io::Error),
  #[error("OS error globbing for files: {0}")]
  GlobFileError(#[from] glob::GlobError),
  #[error("bad glob pattern: {pat:?} (near char {pos}): {msg}")]
  BadGlobPattern { pat: String, msg: &'static str, pos: usize },
  #[error("glob pattern {pat:?} matched non-utf-8 filename {actual:?}")]
  GlobNonUTF8 { pat: String, actual: PathBuf },
  #[error("glob pattern {pat:?} matched filename with no extension {path:?}")]
  GlobNoExtension { pat: String, path: String },
  #[error("{:?}",&self)]
  ExpectedTable(String),
  #[error("{:?}",&self)]
  ExpectedString(String),
  #[error("{:?}",&self)]
  WrongNumberOfSizeDimensions { got: usize, expected: [usize;2] },
  #[error("{:?}",&self)]
  InheritMissingParent(String,String),
  #[error("{:?}",&self)]
  InheritDepthLimitExceeded(String),
  #[error("{:?}",&self)]
  DuplicateItem { item: String, group1: String, group2: String },
  #[error("{:?}",&self)]
  FilesListLineMissingWhitespace(usize),
}

impl LibraryLoadError {
  fn ought(self) -> InternalError {
    InternalError::InternalLogicError(format!("{:?}",self))
  }
}

const INHERIT_DEPTH_LIMIT : u8 = 20;

type LLE = LibraryLoadError;
type TV = toml::Value;
type SE = SpecError;

#[derive(Debug,Serialize,Deserialize)]
struct ItemSpec {
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
  itemname: String,
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
           face.scale, -face.centre[0], -face.centre[1],
           face.svg.0)?;
  }
  #[throws(IE)]
  fn svg_x_defs(&self, _f: &mut Html, _pri : &PieceRenderInstructions) {
  }
  fn describe_html(&self, face : Option<FaceId>) -> Html {
    self.descs[ match face {
      Some(face) => self.faces[face].desc,
      None => self.desc_hidden,
    }].clone()
  }

  fn itemname(&self) -> &str { &self.itemname }
}

impl ItemSpec {
  fn load(&self) -> Result<Box<dyn Piece>,SpecError> {
    let libs = GLOBAL.shapelibs.read().unwrap(); 
    let lib = libs.get(&self.lib).ok_or(SE::LibraryNotFound)?;
    let idata = lib.items.get(&self.item).ok_or(SE::LibraryItemNotFound)?;

    let svg_path = format!("{}/{}.usvg", lib.dirname, &self.item);
    let svg_data = fs::read_to_string(&svg_path)
      .map_err(|e| if e.kind() == ErrorKind::NotFound {
        warn!("library item lib={} itme={} data file {:?} not found",
              &self.lib, &self.item, &svg_path);
        SE::LibraryItemNotFound
      } else {
        let m = "error accessing/reading library item data file";
        error!("{}: {}: {}", &m, &svg_path, &e);
        SE::InternalError(m.to_string())
      })?;

    idata.group.d.outline.check(&idata.group)
      .map_err(|e| SE::InternalError(format!("rechecking outline: {}",&e)))?;
    let outline = idata.group.d.outline.load(&idata.group)?;

    // xxx do something with flip

    let mut descs = index_vec![ ];
    let desc = descs.push(idata.d.desc.clone());
    let desc_hidden = desc; // todo
    let centre = idata.group.d.centre;
    let scale = idata.group.d.scale;
    let face = ItemFace { svg: Html(svg_data), desc, centre, scale };
    let faces = index_vec![ face ];
    let it = Item { faces, descs, outline, desc_hidden,
                    itemname: self.item.clone() };
    Ok(Box::new(it))
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
fn load_catalogue(libname: &str, dirname: &str, toml_path: &str) -> Contents {
  let ioe = |io| LLE::FileError(toml_path.to_string(), io);
  let f = File::open(toml_path).map_err(ioe)?;
  let mut f = BufReader::new(f);
  let mut s = String::new();
  f.read_to_string(&mut s).map_err(ioe)?;
  let toplevel : toml::Value = s.parse()?;
  let mut l = Contents {
    items: HashMap::new(),
    dirname: dirname.to_string(),
  };
  let empty_table = toml::value::Value::Table(Default::default());
  let groups =
    toplevel
    .as_table().ok_or_else(|| LLE::ExpectedTable(format!("toplevel")))?
    .get("group").unwrap_or(&empty_table)
    .as_table().ok_or_else(|| LLE::ExpectedTable(format!("group")))?;
  for (groupname, gdefn) in groups {
    let gdefn = resolve_inherit(INHERIT_DEPTH_LIMIT,
                                &groups, groupname, gdefn)?;
    let gdefn : GroupDefn = TV::Table(gdefn.into_owned()).try_into()?;
    let d = GroupDetails {
      size: gdefn.d.size.iter().map(|s| s * gdefn.d.scale).collect(),
      ..gdefn.d
    };
    let group = Arc::new(GroupData {
      groupname: groupname.clone(),
      d,
    });
    for fe in gdefn.files.0 {
      let item_name = format!("{}{}{}", gdefn.item_prefix,
                              fe.item_spec, gdefn.item_suffix);
      let idata = ItemData {
        group: group.clone(),
        d: Arc::new(ItemDetails { desc: fe.desc.clone() }),
      };
      type H<'e,X,Y> = hash_map::Entry<'e,X,Y>;
      match l.items.entry(item_name.clone()) {
        H::Occupied(oe) => throw!(LLE::DuplicateItem {
          item: item_name.clone(),
          group1: oe.get().group.groupname.clone(),
          group2: groupname.clone(),
        }),
        H::Vacant(ve) => {
          debug!("loaded shape {} {}", libname, item_name);
          ve.insert(idata);
        },
      };
    }
  }
  l
}

#[derive(Deserialize,Debug,Clone)]
#[serde(untagged)]
pub enum Config1 {
  PathGlob(String),
  Explicit(Explicit1),
}

#[derive(Deserialize,Debug,Clone)]
pub struct Explicit1 {
  pub name: String,
  pub catalogue: String,
  pub dirname: String,
}

#[throws(LibraryLoadError)]
pub fn load1(l: &Explicit1) {
  let data = load_catalogue(&l.name, &l.dirname, &l.catalogue)?;
  let count = data.items.len();
  GLOBAL.shapelibs.write().unwrap().insert(l.name.clone(), data);
  info!("loaded {} shapes in library {:?} from {:?} and {:?}",
        count, &l.name, &l.catalogue, &l.dirname);
}

impl Config1 {
  fn resolve(&self) -> Result<Box<dyn ExactSizeIterator<Item=Explicit1>>, LibraryLoadError> {
    use Config1::*;
    Ok(match self {
      Explicit(e) => Box::new(iter::once(e.clone())),
      PathGlob(pat) => {

        #[throws(LLE)]
        fn resolve_globresult(pat: &str, globresult: glob::GlobResult)
                              -> Explicit1 {
          let path = globresult?;
          let path = path.to_str().ok_or_else(
            || LLE::GlobNonUTF8
            { pat: pat.to_string(), actual: path.clone() })?
            .to_string();

          let dirname = path.rsplitn(2,'.').nth(1).ok_or_else(
            || LLE::GlobNoExtension
            { pat: pat.to_string(), path: path.clone() })?;

          let base = dirname.rsplit('/').next().unwrap();

          Explicit1 {
            name: base.to_string(),
            dirname: dirname.to_string(),
            catalogue: path,
          }
        };

        let results = glob::glob_with(pat, glob::MatchOptions {
          require_literal_separator: true,
          require_literal_leading_dot: true,
          .. Default::default()
        })
          .map_err(
            |glob::PatternError { pos, msg, .. }|
            LLE::BadGlobPattern { pat: pat.clone(), pos, msg }
          )?
          .map(|globresult| resolve_globresult(pat, globresult))
          .collect::<Result<Vec<_>, LLE>>()?;

        Box::new(results.into_iter())
      },
    })
  }
}

#[throws(LibraryLoadError)]
pub fn load() {
  for l in &config().shapelibs {
    let libs = l.resolve()?;
    let n = libs.len();
    for e in libs {
      load1(&e)?;
    }
    info!("loaded {} shape libraries from {:?}", n, &l);
          
  }
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
    Some((self.diam * 0.5) as Coord)
  }
}

#[derive(Deserialize,Debug)]
struct CircleDefn { }
#[typetag::deserialize(name="Circle")]
impl OutlineDefn for CircleDefn {
  #[throws(LibraryLoadError)]
  fn check(&self, lgd: &GroupData) { Self::get_size(lgd)?; }
  fn load(&self, lgd: &GroupData) -> Result<Box<dyn Outline>,IE> {
    Ok(Box::new(Circle {
      diam: Self::get_size(lgd).map_err(|e| e.ought())?,
    }))
  }
}
impl CircleDefn {
  #[throws(LibraryLoadError)]
  fn get_size(group: &GroupData) -> f64 {
    match group.d.size.as_slice() {
      &[c] => c,
      size => throw!(LLE::WrongNumberOfSizeDimensions
                     { got: size.len(), expected : [1,1] }),
    }
  }
}

#[derive(Serialize,Deserialize,Debug)]
struct Square { xy: [f64;2] }

#[typetag::serde(name="Square")]
impl Outline for Square {
  #[throws(IE)]
  fn surround_path(&self, _pri : &PieceRenderInstructions) -> Html {
    let size : ArrayVec<_> =
      self.xy.iter().map(|s| s * SELECT_SCALE)
      .collect();
    svg_rectangle_path(size.into_inner().unwrap())?
  }
  #[throws(IE)]
  fn thresh_dragraise(&self, _pri : &PieceRenderInstructions)
                      -> Option<Coord> {
    let smallest : f64 = self.xy.iter().cloned()
      .map(OrderedFloat::from).min().unwrap().into();
    Some((smallest * 0.5) as Coord)
  }
}

#[derive(Deserialize,Debug)]
struct SquareDefn { }
#[typetag::deserialize(name="Square")]
impl OutlineDefn for SquareDefn {
  #[throws(LibraryLoadError)]
  fn check(&self, lgd: &GroupData) { Self::get(lgd)?; }
  fn load(&self, lgd: &GroupData) -> Result<Box<dyn Outline>,IE> {
    Ok(Box::new(
      Self::get(lgd).map_err(|e| e.ought())?
    ))
  }
}
impl SquareDefn {
  #[throws(LibraryLoadError)]
  fn get(group: &GroupData) -> Square {
    match group.d.size.as_slice() {
      &[s] => Square { xy: [s,s] },
      s if s.len() == 2 => Square { xy: s.try_into().unwrap() },
      size => throw!(LLE::WrongNumberOfSizeDimensions
                     { got: size.len(), expected : [1,2]}),
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
      let _r_file_spec = n()?;
      let desc = Html(n()?);
      assert!(n().is_err());
      o.push(FileData{ item_spec, r_file_spec: (), desc  });
    }
    Ok(FileList(o))
  }
}
