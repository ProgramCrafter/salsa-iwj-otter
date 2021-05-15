// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use crate::prelude::*;
pub use crate::shapelib_toml::*;

use parking_lot::{const_rwlock, RwLock};
use parking_lot::RwLockReadGuard;

// Naming convention:
//  *Data, *List   from toml etc. (processed if need be)
//  *Defn          raw read from library toml file (where different from Info)
//  *Details       some shared structure
//  Item           } once loaded and part of a game,
//  Outline        }  no Arc's as we serialise/deserialize during save/load

static GLOBAL_SHAPELIBS: RwLock<Option<Registry>> = const_rwlock(None);

#[derive(Default)]
pub struct Registry {
  libs: HashMap<String, Vec<shapelib::Contents>>,
}

#[derive(Debug)]
pub struct GroupData {
  pub groupname: String,
  pub d: GroupDetails,
}

#[typetag::deserialize(tag="outline")]
pub trait OutlineDefn: Debug + Sync + Send + 'static {
  fn check(&self, lgi: &GroupData) -> Result<(),LLE>;
  fn load(&self, lgi: &GroupData) -> Result<Outline,IE>;
}

#[derive(Debug)]
pub struct Contents {
  libname: String,
  dirname: String,
  items: HashMap<SvgBaseName<GoodItemName>, ItemData>,
}

#[derive(Debug,Clone)]
#[derive(Serialize,Deserialize)]
struct ItemDetails {
  desc: Html,
}

#[derive(Debug,Clone)]
struct ItemData {
  d: Arc<ItemDetails>,
  sort: Option<String>,
  group: Arc<GroupData>,
  outline: Outline,
  occ: OccData,
}

#[derive(Debug,Clone)]
enum OccData {
  None,
  Internal(Arc<OccData_Internal>),
  Back(OccultIlkName),
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
struct OccData_Internal {
  item_name: SvgBaseName<Arc<GoodItemName>>,
  outline: Outline,
  desc: Html,
  xform: FaceTransform,
  svgd: parking_lot::Mutex<Option<Arc<Html>>>,
}

#[derive(Error,Debug)]
pub enum LibraryLoadError {
  #[error(transparent)]
  TomlParseError(#[from] toml::de::Error),
  #[error("error reading/opening library file: {0}: {1}")]
                                                  FileError(String, io::Error),
  #[error("OS error globbing for files: {0}")]
                                      GlobFileError(#[from] glob::GlobError),
  #[error("internal error: {0}")]     InternalError(#[from] InternalError),
  #[error("bad glob pattern: {pat:?} (near char {pos}): {msg}")]
                 BadGlobPattern { pat: String, msg: &'static str, pos: usize },
  #[error("glob pattern {pat:?} matched non-utf-8 filename {actual:?}")]
                                  GlobNonUTF8 { pat: String, actual: PathBuf },
  #[error("glob pattern {pat:?} matched filename with no extension {path:?}")]
                                 GlobNoExtension { pat: String, path: String },
  #[error("occultation colour missing: {0:?}")]
                                            OccultationColourMissing(String),
  #[error("back missing for occultation")]  BackMissingForOccultation,
  #[error("expected TOML table: {0:?}")]    ExpectedTable(String),
  #[error("expected TOML string: {0:?}")]   ExpectedString(String),
  #[error("wrong number of size dimensions {got}, expected {expected:?}")]
              WrongNumberOfSizeDimensions { got: usize, expected: [usize;2] },
  #[error("group {0:?} inherits from nonexistent parent {1:?}")]
                                         InheritMissingParent(String, String),
  #[error("inheritance depth limit exceeded: {0:?}")]
                                            InheritDepthLimitExceeded(String),
  #[error("duplicate item {item:?} in groups {group1:?} and {group2:?}")]
               DuplicateItem { item: String, group1: String, group2: String },
  #[error("files list line {0} missing whitespace")]
                                        FilesListLineMissingWhitespace(usize),
  #[error("files list line {0}, field must be at start")]
                                        FilesListFieldsMustBeAtStart(usize),
  #[error("missing or unrecognised substitution token {0}")]
                                        MissingSubstituionToken(&'static str),
  #[error("repeated substitution token {0}")]
                                        RepeatedSubstituionToken(&'static str),
  #[error("piece defines multiple faces in multiple ways")]
                                        MultipleMultipleFaceDefinitions,
  #[error("{0}")] UnsupportedColourSpec(#[from] UnsupportedColourSpec),
  #[error("bad item name (invalid characters) in {0:?}")] BadItemName(String),
}

impl LibraryLoadError {
  fn ought(&self) -> InternalError {
    internal_error_bydebug(self)
  }
}

const INHERIT_DEPTH_LIMIT: u8 = 20;

type TV = toml::Value;

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

#[derive(Copy,Clone,Debug,Serialize,Deserialize)]
struct ItemFace {
  svg: SvgId,
  desc: DescId,
  #[serde(flatten)]
  xform: FaceTransform,
}

#[derive(Copy,Clone,Debug,Serialize,Deserialize)]
struct FaceTransform {
  centre: [f64; 2],
  scale: [f64; 2],
}

#[derive(Debug,Serialize,Deserialize)]
pub struct Item {
  itemname: String,
  sort: Option<String>,
  faces: IndexVec<FaceId, ItemFace>,
  svgs: IndexVec<SvgId, Html>,
  descs: IndexVec<DescId, Html>,
  outline: Outline,
  #[serde(default)]
  back: Option<Arc<dyn OccultedPieceTrait>>,
}

#[derive(Debug,Serialize,Deserialize)]
struct ItemOccultable {
  desc: Html,
  svgd: Arc<Html>,
  xform: FaceTransform,
  outline: Outline,
}

/// Represents a `T` which is an SVG basename which has been noted
/// for processing during bundle load.
#[derive(Debug,Copy,Clone,Hash,Eq,PartialEq,Ord,PartialOrd)]
#[repr(transparent)]
struct SvgBaseName<T:?Sized>(T);
impl<T> Display for SvgBaseName<T> where T: Display + ?Sized {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) { write!(f, "{}", &self.0)? }
}
impl<T> Borrow<str> for SvgBaseName<T> where T: Borrow<str> {
  fn borrow(&self) -> &str { self.0.borrow() }
}
impl<T> SvgBaseName<T> {
  fn into_inner(self) -> T { self.0 }
}
impl<T> SvgBaseName<T> where T: ?Sized {
  fn as_str(&self) -> &str where T: Borrow<str> { self.0.borrow() }
  fn unnest<'l,U>(&'l self) -> &SvgBaseName<U> where U: ?Sized, T: Borrow<U> {
    let s: &'l U = self.0.borrow();
    let u: &'l SvgBaseName<U> = unsafe { mem::transmute(s) };
    u
  }
}
deref_to_field!{{ T: ?Sized } SvgBaseName<T>, T, 0 }
impl<T> SvgBaseName<T> where T: Borrow<GoodItemName> {
  fn note(src: &mut dyn LibrarySource, i: T) -> Self {
    src.note_svg(i.borrow());
    SvgBaseName(i)
  }
}

#[dyn_upcast]
impl OutlineTrait for ItemOccultable { delegate! { to self.outline {
  fn outline_path(&self, scale: f64) -> Result<Html, IE>;
  fn thresh_dragraise(&self) -> Result<Option<Coord>, IE>;
  fn bbox_approx(&self) -> Result<Rect, IE>;
}}}
#[typetag::serde(name="Lib")]
impl OccultedPieceTrait for ItemOccultable {
  #[throws(IE)]
  fn svg(&self, f: &mut Html, _: VisiblePieceId) {
    self.xform.write_svgd(f, &self.svgd)?;
  }
  #[throws(IE)]
  fn describe_html(&self) -> Html { self.desc.clone() }
}

#[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Ord,PartialOrd)]
pub struct ItemEnquiryData {
  pub lib: LibraryEnquiryData,
  pub itemname: GoodItemName,
  pub sortkey: Option<String>,
  pub f0desc: Html,
  pub f0bbox: Rect,
}

impl Display for ItemEnquiryData {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    write!(f, "{:<10} {:20}  {}", &self.lib, &self.itemname,
           self.f0desc.as_html_str())?;
  }
}

#[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Ord,PartialOrd)]
pub struct LibraryEnquiryData {
  pub libname: String,
}
impl Display for LibraryEnquiryData {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    if self.libname.chars().all(|c| {
      c.is_alphanumeric() || c=='-' || c=='_' || c=='.'
    }) {
      Display::fmt(&self.libname, f)?;
    } else {
      Debug::fmt(&self.libname, f)?;
    }
  }
}

#[dyn_upcast]
impl OutlineTrait for Item { delegate! { to self.outline {
  fn outline_path(&self, scale: f64) -> Result<Html, IE>;
  fn thresh_dragraise(&self) -> Result<Option<Coord>, IE>;
  fn bbox_approx(&self) -> Result<Rect, IE>;
}}}

impl FaceTransform {
  #[throws(LLE)]
  fn from_group(d: &GroupDetails) -> Self {
    // by this point d.size has already been scaled by scale
    let scale = if ! d.orig_size.is_empty() && ! d.size.is_empty() {
      izip!(&d.orig_size, &d.size)
        .map(|(&orig_size, &target_size)| {
          target_size / orig_size
        })
        .cycle()
        .take(2)
        .collect::<ArrayVec<[_;2]>>()
        .into_inner()
        .unwrap()
    } else {
      let s = d.scale;
      [s,s]
    };
    let centre = d.centre.map(Ok).unwrap_or_else(|| Ok::<_,LLE>({
      match d.size.as_slice() {
        [a] => [a,a],
        [a,b] => [a,b],
        x => throw!(LLE::WrongNumberOfSizeDimensions {
          got: x.len(),
          expected: [1,2],
        }),
      }.iter().cloned().zip(&scale).map(|(size,scale)| {
        size * 0.5 / scale
      })
        .collect::<ArrayVec<[_;2]>>()
        .into_inner()
        .unwrap()
    }))?;
    FaceTransform { centre, scale }
  }

  #[throws(IE)]
  fn write_svgd(&self, f: &mut Html, svgd: &Html) {
    hwrite!(f,
           r##"<g transform="scale({} {}) translate({} {})">{}</g>"##,
           self.scale[0], self.scale[1], -self.centre[0], -self.centre[1],
           svgd)?;
  }
}

impl Item {
  #[throws(IE)]
  fn svg_face(&self, f: &mut Html, face: FaceId, vpid: VisiblePieceId) {
    if let Some(face) = self.faces.get(face) {
      let svgd = &self.svgs[face.svg];
      face.xform.write_svgd(f, svgd)?;
    } else if let Some(back) = &self.back {
      back.svg(f, vpid)?;
    } else {
      throw!(internal_error_bydebug(&(self, face)))
    }
  }

  #[throws(IE)]
  fn describe_face(&self, face: FaceId) -> Html {
    // When we are not occulted, we can show are true identity
    // even if we have a back.
    let face = self.faces.get(face).unwrap_or(&self.faces[0]);
    self.descs[ face.desc ].clone()
  }
}

#[typetag::serde(name="Lib")]
impl PieceTrait for Item {
  fn nfaces(&self) -> RawFaceId {
    (self.faces.len()
     + self.back.iter().count())
      .try_into().unwrap()
  }

  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, gpc: &GPiece,
               _gs: &GameState, vpid: VisiblePieceId) {
    self.svg_face(f, gpc.face, vpid)?;
  }
  #[throws(IE)]
  fn describe_html(&self, gpc: &GPiece, _goccults: &GameOccults) -> Html {
    self.describe_face(gpc.face)?
  }

  fn itemname(&self) -> &str { &self.itemname }
  fn sortkey(&self) -> Option<&str> { self.sort.as_ref().map(AsRef::as_ref) }
}

#[typetag::serde(name="LibItem")]
impl OccultedPieceTrait for Item {
  #[throws(IE)]
  fn svg(&self, f: &mut Html, id: VisiblePieceId) {
    self.svg_face(f, default(), id)?;
  }
  #[throws(IE)]
  fn describe_html(&self) -> Html {
    self.describe_face(default())?
  }
}

impl Registry {
  pub fn add(&mut self, data: Contents) {
    self.libs
      .entry(data.libname.clone()).or_default()
      .push(data);
  }

  pub fn clear(&mut self) {
    self.libs.clear()
  }

  pub fn iter(&self) -> impl Iterator<Item=&[Contents]> {
    self.libs.values().map(|v| v.as_slice())
  }
}

pub struct AllRegistries<'ig> {
  global: RwLockReadGuard<'static, Option<Registry>>,
  ig: &'ig Instance,
}
pub struct AllRegistriesIterator<'i> {
  regs: &'i AllRegistries<'i>,
  count: u8,
}

impl<'i> Iterator for AllRegistriesIterator<'i> {
  type Item = &'i Registry;
  fn next(&mut self) -> Option<&'i Registry> {
    loop {
      let r = match self.count {
        0 => self.regs.global.as_ref(),
        1 => Some(&self.regs.ig.local_libs),
        _ => return None,
      };
      self.count += 1;
      if r.is_some() { return r }
    }
  }
}

impl Instance {
  pub fn all_shapelibs(&self) -> AllRegistries<'_> {
    AllRegistries {
      global: GLOBAL_SHAPELIBS.read(),
      ig: self,
    }
  }
} 
impl<'ig> AllRegistries<'ig> {
  pub fn iter(&'ig self) -> AllRegistriesIterator<'ig> {
    AllRegistriesIterator {
      regs: self,
      count: 0,
    }
  }
}

pub fn lib_name_list(ig: &Instance) -> Vec<String> {
  // xxx put bundle id in here or something show it shows up in libs list
  ig.all_shapelibs().iter().map(
    |reg| reg.libs.keys().cloned()
  ).flatten().collect()
}

impl<'ig> AllRegistries<'ig> {
  pub fn all_libs(&self) -> impl Iterator<Item=&[Contents]> {
    self.iter().map(|reg| &reg.libs).flatten().map(
      |(_libname, lib)| lib.as_slice()
    )
  }
  pub fn lib_name_lookup(&self, libname: &str) -> Result<&[Contents], SpE> {
    for reg in self.iter() {
      if let Some(r) = reg.libs.get(libname) { return Ok(r) }
    }
    return Err(SpE::LibraryNotFound);
  }
}

pub type ItemSpecLoaded = (Box<Item>, PieceSpecLoadedOccultable);

impl From<ItemSpecLoaded> for PieceSpecLoaded {
  fn from((p, occultable):  ItemSpecLoaded) -> PieceSpecLoaded {
    PieceSpecLoaded {
      p,
      occultable,
      loaded_via_alias: None,
    }
  }
}

impl ItemSpec {
  #[throws(SpecError)]
  pub fn find_load(&self, ig: &Instance, depth: SpecDepth) -> ItemSpecLoaded {
    let regs = ig.all_shapelibs();
    let libs = regs.lib_name_lookup(&self.lib)?;
    let (lib, (item, idata)) = libs.iter().rev().find_map(
      |lib| Some((lib, lib.items.get_key_value(self.item.as_str())?))
    )
      .ok_or(SpE::LibraryItemNotFound(self.clone()))?;
    lib.load1(idata, &self.lib, item.unnest::<str>(), ig, depth)?
  }

  fn from_strs<L,I>(lib: &L, item: &I) -> Self
    where L: ToOwned<Owned=String> + ?Sized,
          I: ToOwned<Owned=String> + ?Sized,
  {
    let lib  = lib .to_owned();
    let item = item.to_owned();
    ItemSpec{ lib, item }
  }
}

impl Contents {
  #[throws(SpecError)]
  fn load_svg(&self, item_name: &SvgBaseName<str>,
              lib_name_for: &str, item_for: &str)
              -> Html {
    let svg_path = format!("{}/{}.usvg", self.dirname, item_name);
    let svg_data = fs::read_to_string(&svg_path)
      .map_err(|e| if e.kind() == ErrorKind::NotFound {
        warn!("library item lib={} itme={} for={:?} data file {:?} not found",
              &self.libname, item_name, item_for, &svg_path);
        let spec_for = ItemSpec::from_strs(lib_name_for, item_for);
        SpE::LibraryItemNotFound(spec_for)
      } else {
        let m = "error accessing/reading library item data file";
        error!("{}: {} {:?}: {}", &m, &svg_path, item_for, &e);
        SpE::InternalError(m.to_string())
      })?;

    Html::from_html_string(svg_data)
  }

  #[throws(SpecError)]
  fn load1(&self, idata: &ItemData, lib_name: &str,
           name: &SvgBaseName<str>,
           ig: &Instance, depth:SpecDepth)
           -> ItemSpecLoaded {
    let svg_data = self.load_svg(name, lib_name, &**name)?;

    idata.group.d.outline.check(&idata.group)
      .map_err(|e| SpE::InternalError(format!("rechecking outline: {}",&e)))?;
    let outline = idata.outline.clone();

    let mut svgs = IndexVec::with_capacity(1);
    let svg = svgs.push(svg_data);

    let mut descs = index_vec![ ];
    let desc = descs.push(idata.d.desc.clone());
    descs.shrink_to_fit();

    let xform = FaceTransform::from_group(&idata.group.d)
      .map_err(|e| SpE::InternalError(format!("reckoning transform: {}",&e)))?;
    let mut face = ItemFace { svg, desc, xform };
    let mut faces = index_vec![ face ];
    let mut back = None::<Arc<dyn OccultedPieceTrait>>;
    if idata.group.d.flip {
      face.xform.scale[0] *= -1.;
      faces.push(face);
    } else if let Some(back_spec) = &idata.group.d.back {
      match back_spec.load_occult(ig, depth) {
        Err(SpecError::AliasNotFound) => { },
        Err(e) => throw!(e),
        Ok(p) => {
          let p = p.into();
          back = Some(p);
        }
      }
    }
    faces.shrink_to_fit();

    let occultable = match &idata.occ {
      OccData::None => None,
      OccData::Back(ilk) => {
        if let Some(back) = &back {
          let back = back.clone();
          Some((ilk.clone(), back))
        } else {
          None // We got AliasNotFound, ah well
        }
      },
      OccData::Internal(occ) => {
        let occ_name = occ.item_name.clone();
        let svgd = {
          let mut svgd = occ.svgd.lock();
          let svgd = &mut *svgd;
          let svgd = match svgd {
            Some(svgd) => svgd.clone(),
            None => {
              let occ_data = self.load_svg(
                occ.item_name.unnest::<GoodItemName>().unnest(),
                /* original: */ lib_name, name.as_str()
              )?;
              let occ_data = Arc::new(occ_data);
              *svgd = Some(occ_data.clone());
              occ_data
            },
          };
          svgd
        };
        let it = Arc::new(ItemOccultable {
          svgd,
          xform: occ.xform.clone(),
          desc: occ.desc.clone(),
          outline: occ.outline.clone(),
        }) as Arc<dyn OccultedPieceTrait>;
        Some((OccultIlkName(occ_name.into_inner()), it))
      },
    };

    let sort = idata.sort.clone();
    let it = Item { faces, sort, descs, svgs, outline, back,
                    itemname: name.to_string() };
    (Box::new(it), occultable)
  }

  pub fn enquiry(&self) -> LibraryEnquiryData {
    LibraryEnquiryData {
      libname: self.libname.clone(),
    }
  }

  #[throws(MgmtError)]
  pub fn list_glob(&self, pat: &str) -> Vec<ItemEnquiryData> {
    let pat = glob::Pattern::new(pat).map_err(|pe| ME::BadGlob {
      pat: pat.to_string(), msg: pe.msg.to_string() })?;
    let mut out = vec![];
    for (k,v) in &self.items {
      if !pat.matches(k.as_str()) { continue }
      let (loaded, _) = match
        self.load1(v, &self.libname, k.unnest(),
                   &Instance::dummy(), SpecDepth::zero())
      {
        Err(SpecError::LibraryItemNotFound(_)) => continue,
        e@ Err(_) => e?,
        Ok(r) => r,
      };
      let f0bbox = loaded.bbox_approx()?;
      let ier = ItemEnquiryData {
        lib: self.enquiry(),
        itemname: (**k).to_owned(),
        sortkey: v.sort.to_owned(),
        f0bbox,
        f0desc: loaded.describe_face(default())?,
      };
      out.push(ier);
    }
    out
  }
}

#[typetag::serde(name="Lib")]
impl PieceSpec for ItemSpec {
  #[throws(SpecError)]
  fn load(&self, _: usize, _: &mut GPiece, ig: &Instance, depth: SpecDepth)
          -> PieceSpecLoaded {
    self.find_load(ig,depth)?.into()
  }
  #[throws(SpecError)]
  fn load_occult(&self, ig: &Instance, depth: SpecDepth)
                 -> Box<dyn OccultedPieceTrait> {
    self.find_load(ig,depth)?.0 as Box<dyn OccultedPieceTrait>
  }
}

#[typetag::serde(name="LibList")]
impl PieceSpec for MultiSpec {
  #[throws(SpecError)]
  fn count(&self, _pcaliases: &PieceAliases) -> usize { self.items.len() }

  #[throws(SpecError)]
  fn load(&self, i: usize, _: &mut GPiece, ig: &Instance, depth:SpecDepth)
          -> PieceSpecLoaded
  {
    let item = self.items.get(i).ok_or_else(
      || SpE::InternalError(format!("item {:?} from {:?}", i, &self))
    )?;
    // xxx dedupe
    let item = format!("{}{}{}", &self.prefix, item, &self.suffix);
    let lib = self.lib.clone();
    ItemSpec { lib, item }.find_load(ig,depth)?.into()
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

pub trait LibrarySource {
  fn catalogue_data(&self) -> &str;
  fn svg_dir(&self) -> String;
  fn note_svg(&mut self, _basename: &GoodItemName) { }
}

struct BuiltinLibrary<'l> {
  catalogue_data: &'l str,
  dirname: &'l str,
}

impl LibrarySource for BuiltinLibrary<'_> {
  fn catalogue_data(&self) -> &str { self.catalogue_data }
  fn svg_dir(&self) -> String { self.dirname.to_string() }
}

#[throws(LibraryLoadError)]
pub fn load_catalogue(libname: &str, src: &mut dyn LibrarySource) -> Contents {
  let toplevel: toml::Value = src.catalogue_data().parse()?;
  let mut l = Contents {
    libname: libname.to_string(),
    items: HashMap::new(),
    dirname: src.svg_dir(),
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
    group.d.outline.check(&group)?;
    if [
      group.d.flip,
      group.d.back.is_some(),
    ].iter().filter(|x|**x).count() > 1 {
      throw!(LLE::MultipleMultipleFaceDefinitions)
    }
    for fe in gdefn.files.0 {
      #[throws(LLE)]
      fn subst(before: &str, needle: &'static str, replacement: &str)
               -> String {
        let mut matches = before.match_indices(needle);
        let m1 = matches.next()
          .ok_or(LLE::MissingSubstituionToken(needle))?;
        if matches.next().is_some() {
          Err(LLE::RepeatedSubstituionToken(needle))?;
        }
        let mut lhs = &before[0.. m1.0];
        let mut rhs = &before[m1.0 + m1.1.len() ..];
        if replacement.is_empty() {
          let lhs_trimmed = lhs.trim_end();
          if lhs_trimmed.len() != lhs.len() {
            lhs = lhs_trimmed;
          } else {
            rhs = rhs.trim_start();
          } 
        }
        lhs
          .to_owned()
          + replacement
          + rhs
      }

      let outline = group.d.outline.load(&group)?;

      let item_name = format!("{}{}{}", gdefn.item_prefix,
                              fe.item_spec, gdefn.item_suffix);
      let item_name: GoodItemName = item_name.try_into()?;

      let sort = match (gdefn.sort.as_str(), fe.extra_fields.get("sort")) {
        ("", None) => None,
        (gd,  None) => Some(gd.to_string()),
        ("", Some(ef)) => Some(ef.to_string()),
        (gd, Some(ef)) => Some(subst(gd, "_s", ef)?),
      };

      let occ = match &group.d.occulted {
        None => OccData::None,
        Some(OccultationMethod::ByColour { colour }) => {
          if ! group.d.colours.contains_key(colour.0.as_str()) {
            throw!(LLE::OccultationColourMissing(colour.0.clone()));
          }
          let item_name = subst(item_name.as_str(), "_c", &colour.0)?;
          let item_name: GoodItemName = item_name.try_into()?;
          let item_name = SvgBaseName::note(src, Arc::new(item_name));
          let desc = subst(&fe.desc, "_colour", "")?.to_html();
          OccData::Internal(Arc::new(OccData_Internal {
            item_name,
            outline: outline.clone(),
            xform: FaceTransform::from_group(&group.d)?,
            svgd: default(),
            desc,
          }))
        },
        Some(OccultationMethod::ByBack { ilk }) => {
          if group.d.back.is_none() {
            throw!(LLE::BackMissingForOccultation)
          }
          OccData::Back(ilk.clone())
        },
      };

      let mut add1 = |item_name: &GoodItemName, sort, desc: &str| {
        let desc = if let Some(desc_template) = &group.d.desc_template {
          subst(desc_template, "_desc", &desc)?.to_html()
        } else {
          desc.to_html()
        };
        let idata = ItemData {
          group: group.clone(),
          occ: occ.clone(),
          outline: outline.clone(),
          sort,
          d: Arc::new(ItemDetails { desc }),
        };
        type H<'e,X,Y> = hash_map::Entry<'e,X,Y>;
        let new_item = SvgBaseName::note(src, item_name.clone());
        match l.items.entry(new_item) {
          H::Occupied(oe) => throw!(LLE::DuplicateItem {
            item: item_name.as_str().to_owned(),
            group1: oe.get().group.groupname.clone(),
            group2: groupname.clone(),
          }),
          H::Vacant(ve) => {
            debug!("loaded shape {} {}", libname, item_name.as_str());
            ve.insert(idata);
          }
        };
        Ok::<_,LLE>(())
      };

      if group.d.colours.is_empty() {
        add1(&item_name, sort, &fe.desc.clone())?;
      } else {
        for (colour, recolourdata) in &group.d.colours {
          let t_sort = sort.as_ref().map(
            |s| subst(&s, "_c", colour)).transpose()?;
          let t_item_name = subst(item_name.as_str(), "_c", &recolourdata.abbrev)?;
          let t_desc = subst(&fe.desc, "_colour", colour)?;
          add1(&t_item_name.try_into()?, t_sort, &t_desc)?;
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
pub fn load_1_global_library(l: &Explicit1) {
  let toml_path = &l.catalogue;
  let catalogue_data = {
    let ioe = |io| LLE::FileError(toml_path.to_string(), io);
    let f = File::open(toml_path).map_err(ioe)?;
    let mut f = BufReader::new(f);
    let mut s = String::new();
    f.read_to_string(&mut s).map_err(ioe)?;
    s
  };

  let catalogue_data = catalogue_data.as_str();
  let mut src = BuiltinLibrary { dirname: &l.dirname, catalogue_data };

  let data = load_catalogue(&l.name, &mut src)?;
  let count = data.items.len();
  GLOBAL_SHAPELIBS.write()
    .get_or_insert_with(default)
    .add(data);
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
pub fn load_global_libs(libs: &Vec<Config1>) {
  for l in libs {
    let libs = l.resolve()?;
    let n = libs.len();
    for e in libs {
      load_1_global_library(&e)?;
    }
    info!("loaded {} shape libraries from {:?}", n, &l);
          
  }
}

#[derive(Clone,Copy,Debug,Serialize,Deserialize)]
pub struct CircleShape { pub diam: f64 }

#[dyn_upcast]
impl OutlineTrait for CircleShape {
  #[throws(IE)]
  fn outline_path(&self, scale: f64) -> Html {
    svg_circle_path(self.diam * scale)?
  }
  #[throws(IE)]
  fn thresh_dragraise(&self) -> Option<Coord> {
    Some((self.diam * 0.5) as Coord)
  }
  #[throws(IE)]
  fn bbox_approx(&self) -> Rect {
    let d = (self.diam * 0.5).round() as Coord;
    Rect{ corners: [PosC::new(-d,-d), PosC::new(d, d)]}
  }
}

#[derive(Deserialize,Debug)]
struct CircleDefn { }
#[typetag::deserialize(name="Circle")]
impl OutlineDefn for CircleDefn {
  #[throws(LibraryLoadError)]
  fn check(&self, lgd: &GroupData) { Self::get_size(lgd)?; }
  fn load(&self, lgd: &GroupData) -> Result<Outline,IE> {
    Ok(CircleShape {
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
pub struct RectShape { pub xy: PosC<f64> }

impl RectShape {
  #[throws(CoordinateOverflow)]
  pub fn rect(&self, centre: Pos) -> RectC<Coord> {
    let offset = (self.xy * 0.5)?;
    let offset = offset.try_map(
      |c| c.floor().to_i32().ok_or(CoordinateOverflow)
    )?;
    let rect = RectC{ corners:
      [-1,1].iter().map(|&signum| Ok::<_,CoordinateOverflow>({
        (centre + (offset * signum)?)?
      }))
        .collect::<Result<ArrayVec<_>,_>>()?
        .into_inner().unwrap()
    };
    rect
  }

  #[throws(CoordinateOverflow)]
  pub fn region(&self, centre: Pos) -> Region {
    Region::Rect(self.rect(centre)?)
  }
}

#[dyn_upcast]
impl OutlineTrait for RectShape {
  #[throws(IE)]
  fn outline_path(&self, scale: f64) -> Html {
    let xy = (self.xy * scale)?;
    svg_rectangle_path(xy)?
  }
  #[throws(IE)]
  fn thresh_dragraise(&self) -> Option<Coord> {
    let smallest: f64 = self.xy.coords.iter().cloned()
      .map(OrderedFloat::from).min().unwrap().into();
    Some((smallest * 0.5) as Coord)
  }
  #[throws(IE)]
  fn bbox_approx(&self) -> Rect {
    let pos: Pos = self.xy.map(
      |v| ((v * 0.5).round()) as Coord
    );
    let neg = (-pos)?;
    Rect{ corners: [ neg, pos ] }
  }
}

#[derive(Deserialize,Debug)]
struct RectDefn { }
#[typetag::deserialize(name="Rect")]
impl OutlineDefn for RectDefn {
  #[throws(LibraryLoadError)]
  fn check(&self, lgd: &GroupData) { Self::get(lgd)?; }
  fn load(&self, lgd: &GroupData) -> Result<Outline,IE> {
    Ok(
      Self::get(lgd).map_err(|e| e.ought())?.into()
    )
  }
}
impl RectDefn {
  #[throws(LibraryLoadError)]
  fn get(group: &GroupData) -> RectShape {
    RectShape { xy: PosC{ coords:
      match group.d.size.as_slice() {
        &[s] => [s,s],
        s if s.len() == 2 => s.try_into().unwrap(),
        size => throw!(LLE::WrongNumberOfSizeDimensions
                       { got: size.len(), expected: [1,2]}),
      }
    }}
  }
}

impl TryFrom<String> for FileList {
  type Error = LLE;
//  #[throws(LLE)]
  fn try_from(s: String) -> Result<FileList,LLE> {
    let mut o = Vec::new();
    let mut xfields = Vec::new();
    for (lno,l) in s.lines().enumerate() {
      let l = l.trim();
      if l=="" || l.starts_with("#") { continue }
      if let Some(xfields_spec) = l.strip_prefix(':') {
        if ! (o.is_empty() && xfields.is_empty()) {
          throw!(LLE::FilesListFieldsMustBeAtStart(lno));
        }
        xfields = xfields_spec.split_ascii_whitespace()
          .filter(|s| !s.is_empty())
          .map(|s| s.to_owned())
          .collect::<Vec<_>>();
        continue;
      }
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
      let extra_fields = xfields.iter()
        .map(|field| Ok::<_,LLE>((field.to_owned(), n()?.to_owned())))
        .collect::<Result<_,_>>()?;
      let desc = remain.to_owned();
      o.push(FileData{ item_spec, r_file_spec: (), extra_fields, desc });
    }
    Ok(FileList(o))
  }
}
