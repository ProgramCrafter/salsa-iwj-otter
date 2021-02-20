// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use crate::prelude::*;
pub use crate::shapelib_toml::*;

use parking_lot::{const_rwlock, RwLock};
use parking_lot::{MappedRwLockReadGuard, RwLockReadGuard};

// Naming convention:
//  *Data, *List   from toml etc. (processed if need be)
//  *Defn          raw read from library toml file (where different from Info)
//  *Details       some shared structure
//  Item           } once loaded and part of a game,
//  Outline        }  no Arc's as we serialise/deserialize during save/load

pub type Registry = HashMap<String, shapelib::Contents>;

#[derive(Debug)]
pub struct GroupData {
  pub groupname: String,
  pub d: GroupDetails,
}

#[typetag::deserialize(tag="outline")]
pub trait OutlineDefn: Debug + Sync + Send {
  fn check(&self, lgi: &GroupData) -> Result<(),LLE>;
  fn load(&self, lgi: &GroupData) -> Result<OutlineRepr,IE>;
}

#[derive(Debug)]
pub struct Contents {
  libname: String,
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

#[derive(Error,Debug)]
pub enum LibraryLoadError {
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
  InheritMissingParent(String, String),
  #[error("{:?}",&self)]
  InheritDepthLimitExceeded(String),
  #[error("{:?}",&self)]
  DuplicateItem { item: String, group1: String, group2: String },
  #[error("{:?}",&self)]
  FilesListLineMissingWhitespace(usize),
  #[error("{:?}",&self)]
  MissingSubstituionToken(&'static str),
  #[error("{:?}",&self)]
  RepeatedSubstituionToken(&'static str),
}

impl LibraryLoadError {
  fn ought(&self) -> InternalError {
    internal_error_bydebug(self)
  }
}

const INHERIT_DEPTH_LIMIT: u8 = 20;

type TV = toml::Value;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ItemSpec {
  pub lib: String,
  pub item: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MultiSpec {
  pub lib: String,
  #[serde(default)]
  pub prefix: String,
  #[serde(default)]
  pub suffix: String,
  pub items: Vec<String>,
}

define_index_type! { pub struct DescId = u8; }
define_index_type! { pub struct SvgId = u8; }

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
struct ItemFace {
  svg: SvgId,
  desc: DescId,
  centre: [f64; 2],
  scale: [f64; 2],
}

#[derive(Debug,Serialize,Deserialize)]
struct Item {
  itemname: String,
  faces: IndexVec<FaceId, ItemFace>,
  desc_hidden: DescId,
  svgs: IndexVec<SvgId, Html>,
  descs: IndexVec<DescId, Html>,
  outline: OutlineRepr,
}

#[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Ord,PartialOrd)]
pub struct ItemEnquiryData {
  pub itemname: String,
  pub f0desc: Html,
  pub f0bbox: [Pos; 2],
}

impl ItemEnquiryData {
  pub fn line_for_list(&self) -> String {
    format!("{:20}  {}", self.itemname, self.f0desc.0)
  }
}

impl Outline for Item { delegate! { to self.outline {
  fn surround_path(&self, pri: &PieceRenderInstructions) -> Result<Html, IE>;
  fn thresh_dragraise(&self, pri: &PieceRenderInstructions)
                      -> Result<Option<Coord>, IE>;
  fn bbox_approx(&self) -> [Pos; 2];
}}}

#[typetag::serde(name="Lib")]
impl Piece for Item {
  fn nfaces(&self) -> RawFaceId { self.faces.len().try_into().unwrap() }

  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, _gpc: &PieceState,
               pri: &PieceRenderInstructions) {
    let face = &self.faces[pri.face];
    let svgd = &self.svgs[face.svg];
    write!(&mut f.0,
           r##"<g transform="scale({} {}) translate({} {})">{}</g>"##,
           face.scale[0], face.scale[1], -face.centre[0], -face.centre[1],
           svgd.0)?;
  }
  #[throws(IE)]
  fn describe_html(&self, face: Option<FaceId>, _gpc: &PieceState) -> Html {
    self.descs[ match face {
      Some(face) => self.faces[face].desc,
      None => self.desc_hidden,
    }].clone()
  }

  fn itemname(&self) -> &str { &self.itemname }
}

static SHAPELIBS: RwLock<Option<Registry>> = const_rwlock(None);

pub fn libs_list() -> Vec<String> {
  let libs = SHAPELIBS.read();
  libs.as_ref().map(
    |l| l.keys().cloned().collect()
  ).unwrap_or_default()
}

#[throws(SpecError)]
pub fn libs_lookup(libname: &str)
                   -> MappedRwLockReadGuard<'static, Contents> {
  let libs = SHAPELIBS.read();
  RwLockReadGuard::try_map( libs, |libs: &Option<Registry>| -> Option<_> {
    (|| Some({
      libs.as_ref()?.get(libname)?
    }))()
  })
    .map_err(|_| SpE::LibraryNotFound)
    ?
}

impl ItemSpec {
  #[throws(SpecError)]
  pub fn load(&self) -> Box<dyn Piece> {
    let lib = libs_lookup(&self.lib)?;
    let idata = lib.items.get(&self.item)
      .ok_or(SpE::LibraryItemNotFound(self.item.clone()))?;
    lib.load1(idata, &self.item)?
  }
}

impl Contents {
  fn load1(&self, idata: &ItemData, name: &str)
           -> Result<Box<dyn Piece>,SpecError> {
    let svg_path = format!("{}/{}.usvg", self.dirname, &name);
    let svg_data = fs::read_to_string(&svg_path)
      .map_err(|e| if e.kind() == ErrorKind::NotFound {
        warn!("library item lib={} itme={} data file {:?} not found",
              &self.libname, &name, &svg_path);
        SpE::LibraryItemNotFound(name.to_owned())
      } else {
        let m = "error accessing/reading library item data file";
        error!("{}: {}: {}", &m, &svg_path, &e);
        SpE::InternalError(m.to_string())
      })?;

    idata.group.d.outline.check(&idata.group)
      .map_err(|e| SpE::InternalError(format!("rechecking outline: {}",&e)))?;
    let outline = idata.group.d.outline.load(&idata.group)?;

    let mut svgs = IndexVec::with_capacity(1);
    let svg = svgs.push(Html(svg_data));

    let mut descs = index_vec![ ];
    let desc = descs.push(idata.d.desc.clone());
    let desc_hidden = desc; // todo
    descs.shrink_to_fit();

    let centre = idata.group.d.centre;
    let scale = idata.group.d.scale;

    let mut face = ItemFace { svg, desc, centre, scale: [scale,scale] };
    let mut faces = index_vec![ face ];
    if idata.group.d.flip {
      face.scale[0] *= -1.;
      faces.push(face);
    }
    faces.shrink_to_fit();

    let it = Item { faces, descs, svgs, outline, desc_hidden,
                    itemname: name.to_string() };
    Ok(Box::new(it))
  }

  #[throws(MgmtError)]
  pub fn list_glob(&self, pat: &str) -> Vec<ItemEnquiryData> {
    let pat = glob::Pattern::new(pat).map_err(|pe| ME::BadGlob {
      pat: pat.to_string(), msg: pe.msg.to_string() })?;
    let mut out = vec![];
    for (k,v) in &self.items {
      if !pat.matches(&k) { continue }
      let loaded = match self.load1(v, &k) {
        Err(SpecError::LibraryItemNotFound(_)) => continue,
        e@ Err(_) => e?,
        Ok(r) => r,
      };
      let f0bbox = loaded.bbox_approx();
      let ier = ItemEnquiryData {
        itemname: k.clone(),
        f0bbox,
        f0desc: loaded.describe_html(Some(default()), &PieceState::dummy())?,
      };
      out.push(ier);
    }
    out
  }
}

#[typetag::serde(name="Lib")]
impl PieceSpec for ItemSpec {
  fn load(&self, _: usize) -> Result<Box<dyn Piece>, SpecError> {
    self.load()
  }
}

#[typetag::serde(name="LibList")]
impl PieceSpec for MultiSpec {
  fn count(&self) -> usize { self.items.len() }
  fn load(&self, i: usize) -> Result<Box<dyn Piece>,SpecError> {
    let item = self.items.get(i).ok_or_else(
      || SpE::InternalError(format!("item {:?} from {:?}", i, &self))
    )?;
    let item = format!("{}{}{}", &self.prefix, item, &self.suffix);
    let lib = self.lib.clone();
    ItemSpec { lib, item }.load()
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
    None => { return Cow::Borrowed(group) }
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
  let toplevel: toml::Value = s.parse()?;
  let mut l = Contents {
    libname: libname.to_string(),
    items: HashMap::new(),
    dirname: dirname.to_string(),
  };
  let empty_table = toml::value::Value::Table(default());
  let groups =
    toplevel
    .as_table().ok_or_else(|| LLE::ExpectedTable(format!("toplevel")))?
    .get("group").unwrap_or(&empty_table)
    .as_table().ok_or_else(|| LLE::ExpectedTable(format!("group")))?;
  for (groupname, gdefn) in groups {
    let gdefn = resolve_inherit(INHERIT_DEPTH_LIMIT,
                                &groups, groupname, gdefn)?;
    let gdefn: GroupDefn = TV::Table(gdefn.into_owned()).try_into()?;
    let d = GroupDetails {
      size: gdefn.d.size.iter().map(|s| s * gdefn.d.scale).collect(),
      ..gdefn.d
    };
    let group = Arc::new(GroupData {
      groupname: groupname.clone(),
      d,
    });
    for fe in gdefn.files.0 {
      let mut add1 = |item_name: &str, desc| {
        let idata = ItemData {
          group: group.clone(),
          d: Arc::new(ItemDetails { desc }),
        };
        type H<'e,X,Y> = hash_map::Entry<'e,X,Y>;
        match l.items.entry(item_name.to_owned()) {
          H::Occupied(oe) => throw!(LLE::DuplicateItem {
            item: item_name.to_owned(),
            group1: oe.get().group.groupname.clone(),
            group2: groupname.clone(),
          }),
          H::Vacant(ve) => {
            debug!("loaded shape {} {}", libname, item_name);
            ve.insert(idata);
          }
        };
        Ok::<_,LLE>(())
      };

      let item_name = format!("{}{}{}", gdefn.item_prefix,
                              fe.item_spec, gdefn.item_suffix);

      if group.d.colours.is_empty() {
        add1(&item_name, fe.desc.clone())?;
      } else {

        #[throws(LLE)]
        fn subst(before: &str, needle: &'static str, replacement: &str)
                 -> String {
          let mut matches = before.match_indices(needle);
          let m1 = matches.next()
            .ok_or(LLE::MissingSubstituionToken(needle))?;
          if matches.next().is_some() {
            Err(LLE::RepeatedSubstituionToken(needle))?;
          }
          before[0.. m1.0].to_owned()
            + replacement
            + &before[m1.0 + m1.1.len() ..]
        }

        for (colour, recolourdata) in &group.d.colours {
          let t_item_name = subst(&item_name, "_c", &recolourdata.abbrev)?;
          let t_desc = Html(subst(&fe.desc.0, "_colour", colour)?);
          add1(&t_item_name, t_desc)?;
        }

      }
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
  SHAPELIBS.write()
    .get_or_insert_with(default)
    .insert(l.name.clone(), data);
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
        }

        let results = glob::glob_with(pat, glob::MatchOptions {
          require_literal_separator: true,
          require_literal_leading_dot: true,
          ..default()
        })
          .map_err(
            |glob::PatternError { pos, msg, .. }|
            LLE::BadGlobPattern { pat: pat.clone(), pos, msg }
          )?
          .map(|globresult| resolve_globresult(pat, globresult))
          .collect::<Result<Vec<_>, LLE>>()?;

        Box::new(results.into_iter())

      }
    })
  }
}

#[throws(LibraryLoadError)]
pub fn load(libs: &Vec<Config1>) {
  for l in libs {
    let libs = l.resolve()?;
    let n = libs.len();
    for e in libs {
      load1(&e)?;
    }
    info!("loaded {} shape libraries from {:?}", n, &l);
          
  }
}

#[derive(Clone,Copy,Debug,Serialize,Deserialize)]
pub struct Circle { pub diam: f64 }

impl Outline for Circle {
  #[throws(IE)]
  fn surround_path(&self, _pri: &PieceRenderInstructions) -> Html {
    svg_circle_path(self.diam * SELECT_SCALE)?
  }
  #[throws(IE)]
  fn thresh_dragraise(&self, _pri: &PieceRenderInstructions) -> Option<Coord> {
    Some((self.diam * 0.5) as Coord)
  }
  fn bbox_approx(&self) -> [Pos;2] {
    let d = (self.diam * 0.5).ceil() as Coord;
    [PosC([-d,-d]), PosC([d, d])]
  }
}

#[derive(Deserialize,Debug)]
struct CircleDefn { }
#[typetag::deserialize(name="Circle")]
impl OutlineDefn for CircleDefn {
  #[throws(LibraryLoadError)]
  fn check(&self, lgd: &GroupData) { Self::get_size(lgd)?; }
  fn load(&self, lgd: &GroupData) -> Result<OutlineRepr,IE> {
    Ok(Circle {
      diam: Self::get_size(lgd).map_err(|e| e.ought())?,
    }.into())
  }
}
impl CircleDefn {
  #[throws(LibraryLoadError)]
  fn get_size(group: &GroupData) -> f64 {
    match group.d.size.as_slice() {
      &[c] => c,
      size => throw!(LLE::WrongNumberOfSizeDimensions
                     { got: size.len(), expected: [1,1] }),
    }
  }
}

#[derive(Clone,Copy,Debug,Serialize,Deserialize)]
pub struct Rectangle { pub xy: PosC<f64> }

impl Outline for Rectangle {
  #[throws(IE)]
  fn surround_path(&self, _pri: &PieceRenderInstructions) -> Html {
    let size = self.xy * SELECT_SCALE;
    svg_rectangle_path(size)?
  }
  #[throws(IE)]
  fn thresh_dragraise(&self, _pri: &PieceRenderInstructions)
                      -> Option<Coord> {
    let smallest: f64 = self.xy.0.iter().cloned()
      .map(OrderedFloat::from).min().unwrap().into();
    Some((smallest * 0.5) as Coord)
  }
  fn bbox_approx(&self) -> [Pos;2] {
    let pos: Pos = self.xy.map(
      |v| ((v * 0.5).ceil()) as Coord
    );
    let neg = -pos;
    [ neg, pos ]
  }
}

#[derive(Deserialize,Debug)]
struct SquareDefn { }
#[typetag::deserialize(name="Square")]
impl OutlineDefn for SquareDefn {
  #[throws(LibraryLoadError)]
  fn check(&self, lgd: &GroupData) { Self::get(lgd)?; }
  fn load(&self, lgd: &GroupData) -> Result<OutlineRepr,IE> {
    Ok(
      Self::get(lgd).map_err(|e| e.ought())?.into()
    )
  }
}
impl SquareDefn {
  #[throws(LibraryLoadError)]
  fn get(group: &GroupData) -> Rectangle {
    Rectangle { xy: PosC(
      match group.d.size.as_slice() {
        &[s] => [s,s],
        s if s.len() == 2 => s.try_into().unwrap(),
        size => throw!(LLE::WrongNumberOfSizeDimensions
                       { got: size.len(), expected: [1,2]}),
      }
    )}
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
      let mut remain = &*l;
      let mut n = ||{
        let ws = remain.find(char::is_whitespace)
          .ok_or(LLE::FilesListLineMissingWhitespace(lno))?;
        let (l, r) = remain.split_at(ws);
        remain = r.trim_start();
        Ok::<_,LLE>(l.to_owned())
      };
      let item_spec = n()?;
      let _r_file_spec = n()?;
      let desc = Html(remain.to_owned());
      o.push(FileData{ item_spec, r_file_spec: (), desc  });
    }
    Ok(FileList(o))
  }
}
