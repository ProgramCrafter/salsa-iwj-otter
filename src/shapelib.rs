// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;
pub use crate::shapelib_toml::*;

pub use crate::prelude::GoodItemName; // not sure why this is needed

use parking_lot::{const_rwlock, RwLock};
use parking_lot::RwLockReadGuard;

//==================== structs and definitions ====================

// Naming convention:
//  *Data, *List   from toml etc. (processed if need be)
//  *Defn          raw read from library toml file (where different from Info)
//  *Details       some shared structure
//  Item           } once loaded and part of a game,
//  Outline        }  no Arc's as we serialise/deserialize during save/load

static GLOBAL_SHAPELIBS: RwLock<Option<Registry>> = const_rwlock(None);

#[derive(Default)]
pub struct Registry {
  libs: HashMap<String, Vec<shapelib::Catalogue>>,
}

#[derive(Debug)]
pub struct GroupData {
  groupname: String,
  d: GroupDetails,
  #[allow(dead_code)] /*TODO*/ mformat: materials_format::Version,
}

#[derive(Debug,Clone,Copy)]
pub struct ShapeCalculable { }

#[derive(Debug)]
pub struct Catalogue {
  libname: String,
  dirname: String,
  bundle: Option<bundles::Id>,
  items: HashMap<SvgBaseName<GoodItemName>, CatalogueEntry>,
}

#[derive(Debug,Clone)]
#[derive(Serialize,Deserialize)]
struct ItemDetails {
  desc: Html,
}

#[derive(Debug,Clone)]
enum CatalogueEntry {
  Item(ItemData),
  Magic { group: Arc<GroupData>, spec: Arc<dyn PieceSpec> },
}
use CatalogueEntry as CatEnt;

#[derive(Debug,Clone)]
struct ItemData {
  d: Arc<ItemDetails>,
  sort: Option<String>,
  group: Arc<GroupData>,
  occ: OccData,
  shape_calculable: ShapeCalculable,
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
  item_name: SvgBaseName<GoodItemName>,
  desc: Html,
  loaded: lazy_init::Lazy<Result<ImageLoaded,SpecError>>,
}

#[allow(non_camel_case_types)]
#[derive(Debug,Clone)]
struct ImageLoaded {
  svgd: Html,
  xform: FaceTransform,
  outline: Outline,
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
  #[error("piece defines multiple faces in multiple ways")]
                                        MultipleMultipleFaceDefinitions,
  #[error("outline specified both size and numeric scale")]
                                        OutlineContradictoryScale,
  #[error("{0}")] CoordinateOverflow(#[from] CoordinateOverflow),

  #[error("{0}")]
  MaterialsFormatIncompat(#[from] materials_format::Incompat<
    LibraryLoadMFIncompat
  >),
  #[error("{0}")]                       BadSubstitution(#[from] SubstError),
  #[error("{0}")] UnsupportedColourSpec(#[from] UnsupportedColourSpec),
  #[error("bad item name (invalid characters) in {0:?}")] BadItemName(String),
  #[error("{0}")] MaterialsFormatVersionError(#[from] MFVE),

  #[error("could not parse template-expaneded TOML: {error} (in {toml:?}")]
  TemplatedTomlError { toml: String, error: toml_de::Error },

  #[error("group {group}: {error}")]
  InGroup { group: String, error: Box<LLE> },

  #[error("library {lib}: {error}")]
  InLibrary { lib: String, error: Box<LLE> },
}

#[derive(Error,Debug,Clone,Copy,Serialize,Deserialize)]
pub enum LibraryLoadMFIncompat {
  #[error("bad scale definition")] Scale,
  #[error("size not specified")] SizeRequired,
  #[error("orig_size no longer supported")] OrigSizeForbidden,
  #[error("specified both size and numeric scale")] ContradictoryScale,
}
pub use LibraryLoadMFIncompat as LLMI;

#[derive(Error,Copy,Clone,Debug,Eq,PartialEq)]
pub enum SubstErrorKind {
  #[error("missing or unrecognised token {0}")] MissingToken (&'static str),
  #[error("repeated token {0}")]                RepeatedToken(&'static str),
}

#[derive(Error,Clone,Debug)]
#[error("bad substitution: {input:?} {kind}")]
pub struct SubstError {
  pub kind: SubstErrorKind,
  pub input: String,
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
  back: Option<Arc<dyn InertPieceTrait>>,
}

#[derive(Debug,Serialize,Deserialize)]
struct ItemInertForOcculted {
  itemname: GoodItemName,
  desc: Html,
  svgd: Html,
  xform: FaceTransform,
  outline: Outline,
}

//==================== SvgBsseName ====================

/// Represents a `T` which is an SVG basename which has been noted
/// for processing during bundle load.
#[derive(Debug,Copy,Clone,Hash,Eq,PartialEq,Ord,PartialOrd,Deref)]
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
impl<T> SvgBaseName<T> where T: Borrow<GoodItemName> {
  #[throws(SubstError)]
  fn note(src: &mut dyn LibrarySvgNoter, i: T,
          src_name: Result<&str, &SubstError>) -> Self {
    src.note_svg(i.borrow(), src_name)?;
    SvgBaseName(i)
  }
}

//==================== impls for ItemInertForOcculted ====================

#[dyn_upcast]
impl OutlineTrait for ItemInertForOcculted { delegate! { to self.outline {
  fn outline_path(&self, scale: f64) -> Result<Html, IE>;
  fn thresh_dragraise(&self) -> Result<Option<Coord>, IE>;
  fn bbox_approx(&self) -> Result<Rect, IE>;
}}}
#[dyn_upcast]
impl PieceBaseTrait for ItemInertForOcculted {
  fn nfaces(&self) -> RawFaceId { 1 }
  fn itemname(&self) -> &str { self.itemname.as_str() }
}
#[typetag::serde(name="Lib")]
impl InertPieceTrait for ItemInertForOcculted {
  #[throws(IE)]
  fn svg(&self, f: &mut Html, _: VisiblePieceId, face: FaceId,
         _: &PieceXDataState) {
    if face != FaceId::default() {
      throw!(internal_logic_error("ItemInertForOcculted non-default face"))
    }
    self.xform.write_svgd(f, &self.svgd)?;
  }
  #[throws(IE)]
  fn describe_html(&self, _: FaceId) -> Html { self.desc.clone() }
}

//---------- ItemEnquiryData, LibraryEnquiryData ----------

#[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Ord,PartialOrd)]
pub struct ItemEnquiryData {
  pub lib: LibraryEnquiryData,
  pub itemname: GoodItemName,
  pub sortkey: Option<String>,
  pub f0desc: Html,
  pub f0bbox: Rect,
}

impl From<&ItemEnquiryData> for ItemSpec {
  fn from(it: &ItemEnquiryData) -> ItemSpec {
    ItemSpec {
      lib: it.lib.libname.clone(),
      item: it.itemname.as_str().to_owned(),
    }
  }
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
  pub bundle: Option<bundles::Id>,
  pub libname: String,
}
impl Display for LibraryEnquiryData {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    if let Some(id) = self.bundle.as_ref() {
      write!(f, "[{}] ", id)?;
    }
    if self.libname.chars().all(|c| {
      c.is_alphanumeric() || c=='-' || c=='_' || c=='.'
    }) {
      Display::fmt(&self.libname, f)?;
    } else {
      Debug::fmt(&self.libname, f)?;
    }
  }
}

//==================== Item ====================

#[dyn_upcast]
impl OutlineTrait for Item { delegate! { to self.outline {
  fn outline_path(&self, scale: f64) -> Result<Html, IE>;
  fn thresh_dragraise(&self) -> Result<Option<Coord>, IE>;
  fn bbox_approx(&self) -> Result<Rect, IE>;
}}}

impl Item {
  #[throws(IE)]
  fn svg_face(&self, f: &mut Html, face: FaceId, vpid: VisiblePieceId,
              xdata: &PieceXDataState) {
    if let Some(face) = self.faces.get(face) {
      let svgd = &self.svgs[face.svg];
      face.xform.write_svgd(f, svgd)?;
    } else if let Some(back) = &self.back {
      back.svg(f, vpid, default(), xdata)?;
    } else {
      throw!(internal_error_bydebug(&(self, face)))
    }
  }

  #[throws(IE)]
  fn describe_face(&self, face: FaceId) -> Html {
    self.descs[
      if let Some(face) = self.faces.get(face) {
        face.desc
      } else if let Some(back) = &self.back {
        return back.describe_html(default())?;
      } else {
        self.faces[0].desc
      }
    ].clone()
  }
}

#[dyn_upcast]
impl PieceBaseTrait for Item {
  fn nfaces(&self) -> RawFaceId {
    (self.faces.len()
     + self.back.iter().count())
      .try_into().unwrap()
  }

  fn itemname(&self) -> &str { &self.itemname }
}

#[typetag::serde(name="Lib")]
impl PieceTrait for Item {
  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, gpc: &GPiece,
               _gs: &GameState, vpid: VisiblePieceId) {
    self.svg_face(f, gpc.face, vpid, &gpc.xdata)?;
  }
  #[throws(IE)]
  fn describe_html(&self, gpc: &GPiece, _goccults: &GOccults) -> Html {
    self.describe_face(gpc.face)?
  }

  fn sortkey(&self) -> Option<&str> { self.sort.as_ref().map(AsRef::as_ref) }
}

#[typetag::serde(name="LibItem")]
impl InertPieceTrait for Item {
  #[throws(IE)]
  fn svg(&self, f: &mut Html, id: VisiblePieceId, face: FaceId,
         xdata: &PieceXDataState) {
    self.svg_face(f, face, id, xdata)?;
  }
  #[throws(IE)]
  fn describe_html(&self, _: FaceId) -> Html {
    self.describe_face(default())?
  }
}

//==================== ItemSpec and item loading ====================

//---------- ItemSpec, MultiSpec ----------

type ItemSpecLoaded = (Box<Item>, PieceSpecLoadedOccultable);

impl From<ItemSpecLoaded> for SpecLoaded {
  fn from((p, occultable):  ItemSpecLoaded) -> SpecLoaded {
    SpecLoaded {
      p,
      occultable,
      special: default(),
    }
  }
}

impl ItemSpec {
  #[throws(SpecError)]
  fn find_then<F,T>(&self, ig: &Instance, then: F) -> T
  where F: FnOnce(&Catalogue, &SvgBaseName<GoodItemName>, &CatalogueEntry)
                  -> Result<T, SpecError>
  {
    let regs = ig.all_shapelibs();
    let libs = regs.lib_name_lookup(&self.lib)?;
    let (lib, (item, idata)) = libs.iter().rev().find_map(
      |lib| Some((lib, lib.items.get_key_value(self.item.as_str())?))
    )
      .ok_or_else(|| SpE::LibraryItemNotFound(self.clone()))?;
    then(lib, item, idata)?
  }

  #[throws(SpecError)]
  fn find_load_general<MAG,MUN,T>(&self, ig: &Instance, depth: SpecDepth,
                                      mundanef: MUN, magicf: MAG) -> T
  where MUN: FnOnce(ItemSpecLoaded) -> Result<T, SpE>,
        MAG: FnOnce(&Arc<dyn PieceSpec>) -> Result<T, SpE>,
  {
    self.find_then(ig, |lib, item, catent| Ok(match catent {
      CatEnt::Item(idata) => {
        let loaded = lib.load1(idata, &self.lib, item.unnest::<str>(),
                               ig, depth)?;
        mundanef(loaded)?
      },
      CatEnt::Magic { spec,.. } => {
        magicf(spec)?
      },
    }))?
  }

  #[throws(SpecError)]
  pub fn find_load_mundane(&self, ig: &Instance,
                           depth: SpecDepth) -> ItemSpecLoaded {
    self.find_load_general(
      ig, depth, |loaded| Ok(loaded),
      |_| Err(SpE::ComplexPieceWhereInertRequired)
    )?
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

#[typetag::serde(name="Lib")]
impl PieceSpec for ItemSpec {
  #[throws(SpecError)]
  fn load(&self, pla: PLA) -> SpecLoaded {
    self.find_load_general(
      pla.ig, pla.depth,
      |loaded| Ok(loaded.into()),
      |magic| magic.load(pla),
    )?
  }
  #[throws(SpecError)]
  fn load_inert(&self, ig: &Instance, depth: SpecDepth) -> SpecLoadedInert {
    let (p, occultable) = self.find_load_mundane(ig,depth)?;
    SpecLoadedInert { p: p as _, occultable }
  }
}

#[typetag::serde(name="LibList")]
impl PieceSpec for MultiSpec {
  #[throws(SpecError)]
  fn count(&self, _pcaliases: &PieceAliases) -> usize { self.items.len() }

  #[throws(SpecError)]
  fn load(&self, pla: PLA) -> SpecLoaded
  {
    let PLA { i,.. } = pla;
    let item = self.items.get(i).ok_or_else(
      || SpE::InternalError(format!("item {:?} from {:?}", i, &self))
    )?;
    let item = format!("{}{}{}", &self.prefix, item, &self.suffix);
    let lib = self.lib.clone();
    ItemSpec { lib, item }.load(pla)?
  }
}

//---------- Loading ----------

impl Catalogue {
  #[throws(SpecError)]
  fn load_image(&self, item_name: &SvgBaseName<str>,
                lib_name_for: &str, item_for: &str,
                group: &GroupData, shape_calculable: ShapeCalculable)
              -> ImageLoaded {
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

    let svg_data = Html::from_html_string(svg_data);

    let sz = svg_parse_size(&svg_data).map_err(|error| SpE::SVGError {
      error,
      item_name: item_name.as_str().into(),
      item_for_lib: lib_name_for.into(),
      item_for_item: item_for.into(),
    })?;

    let (xform, outline) = group.load_shape(sz)
      .map_err(shape_calculable.err_mapper())?;

    ImageLoaded {
      svgd: svg_data,
      outline,
      xform,
    }
  }

  #[throws(SpecError)]
  fn load1(&self, idata: &ItemData, lib_name: &str,
           name: &SvgBaseName<str>,
           ig: &Instance, depth:SpecDepth)
           -> ItemSpecLoaded {
    let ImageLoaded { svgd: svg_data, outline, xform } =
      self.load_image(name, lib_name, &**name,
                       &idata.group, idata.shape_calculable)?;

    let mut svgs = IndexVec::with_capacity(1);
    let svg = svgs.push(svg_data);

    let mut descs = index_vec![ ];
    let desc = descs.push(idata.d.desc.clone());
    descs.shrink_to_fit();

    let mut face = ItemFace { svg, desc, xform };
    let mut faces = index_vec![ face ];
    let mut back = None::<Arc<dyn InertPieceTrait>>;
    if idata.group.d.flip {
      face.xform.scale[0] *= -1.;
      faces.push(face);
    } else if let Some(back_spec) = &idata.group.d.back {
      match back_spec.load_inert(ig, depth) {
        Err(SpecError::AliasNotFound) => { },
        Err(e) => throw!(e),
        Ok(p) => {
          let p = p.p.into();
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
          Some((LOI::Mix(ilk.clone()), back))
        } else {
          None // We got AliasNotFound, ah well
        }
      },
      OccData::Internal(occ) => {
        let occ_name = occ.item_name.clone();
        let ImageLoaded {
          svgd, outline, xform
        } = occ.loaded.get_or_create(
          || self.load_image(
            occ.item_name.unnest::<GoodItemName>().unnest(),
            /* original: */ lib_name, name.as_str(),
            &idata.group, idata.shape_calculable,
          )
        ).clone()?;

        let it = Arc::new(ItemInertForOcculted {
          svgd, outline, xform,
          itemname: occ_name.clone().into_inner(),
          desc: occ.desc.clone(),
        }) as Arc<dyn InertPieceTrait>;
        Some((LOI::Mix(occ_name.into_inner()), it))
      },
    };

    let sort = idata.sort.clone();
    let it = Item { faces, sort, descs, svgs, outline, back,
                    itemname: name.to_string() };
    (Box::new(it), occultable)
  }
}

//==================== size handling, and outlines ====================

impl FaceTransform {
  #[throws(LLE)]
  fn from_group_mf1(group: &GroupData) -> Self {
    let d = &group.d;
    // by this point d.size has already been scaled by scale
    let scale = if ! d.orig_size.is_empty() && ! d.size.is_empty() {
      izip!(&d.orig_size, &d.size)
        .map(|(&orig_size, &target_size)| {
          target_size / orig_size
        })
        .cycle()
        .take(2)
        .collect::<ArrayVec<_,2>>()
        .into_inner()
        .unwrap()
    } else {
      let s = group.d.scale_mf1(group.mformat)?;
      [s,s]
    };
    let centre = d.centre.map(Ok).unwrap_or_else(|| Ok::<_,LLE>({
      resolve_square_size(&d.size)?
        .ok_or_else(|| group.mformat.incompat(LLMI::SizeRequired))?
        .coords.iter().cloned().zip(&scale).map(|(size,scale)| {
          size * 0.5 / scale
        })
        .collect::<ArrayVec<_,2>>()
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

#[throws(LLE)]
fn resolve_square_size<T:Copy>(size: &[T]) -> Option<PosC<T>> {
  Some(PosC{ coords: match size {
    [] => return None,
    &[s] => [s,s],
    &[w,h] => [w,h],
    _ => throw!(LLE::WrongNumberOfSizeDimensions
                { got: size.len(), expected: [1,2]}),
  } })
}

impl CatalogueEntry {
  fn group(&self) -> &Arc<GroupData> { match self {
    CatEnt::Item(item) => &item.group,
    CatEnt::Magic{group,..} => group,
  } }
}

//---------- Outlines ----------

impl ShapeCalculable {
  pub fn err_mapper(&self) -> impl Fn(LLE) -> IE + Copy {
    |e| internal_logic_error(format!(
      "outline calculable but failed {} {:?}",&e,&e
    ))
  }
}

impl GroupData {
  #[throws(LibraryLoadError)]
  fn check_shape(&self) -> ShapeCalculable {
    let _ = self.load_shape(PosC::new(
      1.,1. /* dummy value, suffices for error check */
    ))?;
    ShapeCalculable{}
  }

  #[throws(LibraryLoadError)]
  /// As with OutlineDefn::load, success must not depend on svg_sz value
  fn load_shape(&self, svg_sz: PosC<f64>) -> (FaceTransform, Outline) {
    if self.mformat >= 2 {

      if self.d.orig_size.len() > 0 {
        throw!(self.mformat.incompat(LLMI::OrigSizeForbidden))
      }

      let centre: PosC<f64> = self.d.centre
        .map(|coords| PosC { coords })
        .unwrap_or_else(|| geometry::Mean::mean(&svg_sz, &PosC::zero()));

      let size = resolve_square_size(&self.d.size)?;

      use ScaleDetails as SD;
      let scale = self.d.scale.unwrap_or(SD::Fit(ScaleFitDetails::Fit));

      let of_stretch = |scale| {
        let scale = PosC { coords: scale };
        let size = pos_zip_map!( svg_sz, scale => |(sz,sc)| sz * sc );
        (size, scale)
      };

      let (size, scale) = match (size, scale) {
        (Some(size), SD::Fit(fit)) => {
          let scale = pos_zip_map!( size, svg_sz => |(a,b)| a/b );
          type Of = OrderedFloat<f64>;
          let of = |minmax: fn(Of,Of) -> Of| {
            let v = minmax(scale.coords[0].into(),
                           scale.coords[1].into()).into_inner();
            PosC::new(v,v)
          };
          let scale = match fit {
            ScaleFitDetails::Fit => of(min),
            ScaleFitDetails::Cover => of(max),
            ScaleFitDetails::Stretch => scale,
          };
          (size, scale)
        },
        (Some(_), SD::Scale(_)) |
        (Some(_), SD::Stretch(_))
          => throw!(self.mformat.incompat(LLMI::ContradictoryScale)),
        (None, SD::Fit(_)) => (svg_sz, PosC::new(1.,1.)),
        (None, SD::Scale(s)) => of_stretch([s,s]),
        (None, SD::Stretch(s)) => of_stretch(s),
      };

      let osize = {
        let (osize, oscale) = self.d.outline.size_scale();
        let osize = resolve_square_size(&osize)?;
        match (osize, oscale) {
          (Some(osize), None         ) => osize,
          (None,        Some(&oscale)) => (size * oscale)?,
          (None,        None         ) => size,
          (Some(_),     Some(_)      ) =>
            throw!(LLE::OutlineContradictoryScale)
        }
      };

      let outline = self.d.outline.shape().load(osize);
      (FaceTransform { scale: scale.coords, centre: centre.coords }, outline)

    } else {
      let xform = FaceTransform::from_group_mf1(self)?;
      let outline = self.d.outline.shape().load_mf1(&self)?;
      (xform, outline)
    }
  }
}

impl GroupDetails {
  #[throws(materials_format::Incompat<LLMI>)]
  fn scale_mf1(&self, mformat: materials_format::Version) -> f64 {
    match self.scale {
      None => 1.,
      Some(ScaleDetails::Scale(s)) => s,
      _ => throw!(mformat.incompat(LLMI::Scale)),
    }
  }
}

//---------- OutlineDefn etc. ----------

#[ambassador::delegatable_trait]
pub trait OutlineDefnTrait: Debug + Sync + Send + 'static {
  /// Success or failure must not depend on `svg_sz`
  ///
  /// Called to *check* the group configuration before load, but
  /// with a dummy svg_gz of `[1,1]`.  That must correctly predict
  /// success with other sizes.
  fn load(&self, size: PosC<f64>) -> Outline {
    RectShape { xy: size }.into()
  }

  fn load_mf1(&self, group: &GroupData) -> Result<Outline,LLE>;
}

// We used to do this via typetag and Box<dyn OutlineDefn>
//
// But I didnt manage to get typetag to deserialise these unit variants.
// Instead, we have this enum and a cheesy macro to impl OutlineDefn
// by delegating to a freshly made (static) unit struct value.
macro_rules! outline_defns {
  { $( $Shape:ident ),* } => { paste!{

    #[derive(Deserialize,Debug,Copy,Clone,Eq,PartialEq)]
    pub enum OutlineDefnEnum {
      $( $Shape, )*
    }

    impl OutlineDefnEnum {
      fn defn(self) -> &'static dyn OutlineDefnTrait {
        match self { $(
          Self::$Shape => &[< $Shape Defn >] as _,
        )* }
      }
    }

  } }
}

outline_defns! { Circle, Rect }
impl_via_ambassador!{
  impl OutlineDefnTrait for OutlineDefnEnum { defn() }
}

//---------- RectShape ----------

#[derive(Clone,Copy,Debug,Serialize,Deserialize)]
pub struct RectShape { pub xy: PosC<f64> }

impl RectShape {
  // Used by code elsewhere eg deck.rs for occultation boundaries etc.
  #[throws(CoordinateOverflow)]
  pub fn rect(&self, nominal: Pos) -> RectC<Coord> {
    let offset = (self.xy * 0.5)?;
    let offset = offset.try_map(
      |c| c.floor().to_i32().ok_or(CoordinateOverflow)
    )?;
    let rect = RectC{ corners:
      [-1,1].iter().map(|&signum| Ok::<_,CoordinateOverflow>({
        (nominal + (offset * signum)?)?
      }))
        .collect::<Result<ArrayVec<_,2>,_>>()?
        .into_inner().unwrap()
    };
    rect
  }

  #[throws(CoordinateOverflow)]
  pub fn region(&self, nominal: Pos) -> Region {
    Region::Rect(self.rect(nominal)?)
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
struct RectDefn;
impl OutlineDefnTrait for RectDefn {
  fn load(&self, size: PosC<f64>) -> Outline {
    RectShape { xy: size }.into()
  }

  #[throws(LibraryLoadError)]
  fn load_mf1(&self, group: &GroupData) -> Outline {
    let size = resolve_square_size(&group.d.size)?
        .ok_or_else(|| group.mformat.incompat(LLMI::SizeRequired))?;
    self.load(size)
  }
}

//---------- CircleShape ----------

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
struct CircleDefn;
impl OutlineDefnTrait for CircleDefn {
  fn load(&self, size: PosC<f64>) -> Outline {
    let diam = size
      .coords.into_iter()
      .map(OrderedFloat)
      .max().unwrap().
      into_inner();
    CircleShape {
      diam,
    }.into()
  }

  #[throws(LibraryLoadError)]
  fn load_mf1(&self, group: &GroupData) -> Outline {
    let diam = match group.d.size.as_slice() {
      &[c] => c,
      size => throw!(LLE::WrongNumberOfSizeDimensions
                     { got: size.len(), expected: [1,1] }),
    };
    CircleShape {
      diam,
    }.into()
  }
}

//==================== Catalogues ====================

//---------- enquiries etc. ----------

impl Catalogue {
  pub fn enquiry(&self) -> LibraryEnquiryData {
    LibraryEnquiryData {
      libname: self.libname.clone(),
      bundle: self.bundle,
    }
  }

  #[throws(MgmtError)]
  pub fn list_glob(&self, pat: &str) -> Vec<ItemEnquiryData> {
    let pat = glob::Pattern::new(pat).map_err(|pe| ME::BadGlob {
      pat: pat.to_string(), msg: pe.msg.to_string() })?;
    let mut out = vec![];
    let ig_dummy = Instance::dummy();
    for (k,v) in &self.items {
      if !pat.matches(k.as_str()) { continue }
      let mut gpc = GPiece::dummy();
      let loaded = match (|| Ok(match v {
        CatEnt::Item(item) => {
          let (loaded, _) =
            self.load1(item, &self.libname, k.unnest(),
                       &Instance::dummy(), SpecDepth::zero())?;
          loaded as Box<dyn PieceTrait>
        },
        CatEnt::Magic{spec,..} => {
          spec.load(PieceLoadArgs {
            i: 0,
            gpc: &mut gpc,
            ig: &ig_dummy,
            depth: SpecDepth::zero(),
          })?.p
        }
      }))() {
        Err(SpecError::LibraryItemNotFound(_)) => continue,
        e@ Err(_) => e?,
        Ok(r) => r,
      };
      let f0bbox = loaded.bbox_approx()?;
      let ier = ItemEnquiryData {
        lib: self.enquiry(),
        itemname: (**k).to_owned(),
        sortkey: loaded.sortkey().map(|s| s.to_owned()),
        f0bbox,
        f0desc: loaded.describe_html(&gpc, &default())?,
      };
      out.push(ier);
    }
    out
  }
}

pub trait LibrarySvgNoter {
  #[throws(SubstError)]
  fn note_svg(&mut self, _basename: &GoodItemName,
              _src_name: Result<&str, &SubstError>) { }
}
pub trait LibrarySource: LibrarySvgNoter {
  fn catalogue_data(&self) -> &str;
  fn svg_dir(&self) -> String;
  fn bundle(&self) -> Option<bundles::Id>;

  fn default_materials_format(&self)
                              -> Result<materials_format::Version, MFVE>;

  // Sadly dyn_upcast doesn't work because it doesn't support the
  // non-'static lifetime on BuiltinLibrary
  fn svg_noter(&mut self) -> &mut dyn LibrarySvgNoter;
}

pub struct NullLibrarySvgNoter;
impl LibrarySvgNoter for NullLibrarySvgNoter { }

struct BuiltinLibrary<'l> {
  catalogue_data: &'l str,
  dirname: &'l str,
}

impl LibrarySvgNoter for BuiltinLibrary<'_> {
}
impl<'l> LibrarySource for BuiltinLibrary<'l> {
  fn catalogue_data(&self) -> &str { self.catalogue_data }
  fn svg_dir(&self) -> String { self.dirname.to_string() }
  fn bundle(&self) -> Option<bundles::Id> { None }

  #[throws(materials_format::VersionError)]
  fn default_materials_format(&self) -> materials_format::Version {
    throw!(MFVE::Other("builtin libraries must have explicit version now!"));
  }

  fn svg_noter(&mut self) -> &mut dyn LibrarySvgNoter { self }
}

//---------- reading ----------

#[throws(LibraryLoadError)]
pub fn load_catalogue(libname: &str, src: &mut dyn LibrarySource)
                      -> Catalogue {
  (||{

  let toplevel: toml::Value = src.catalogue_data().parse()?;
  let toplevel = toplevel
    .as_table().ok_or_else(|| LLE::ExpectedTable(format!("toplevel")))?;
  let mformat = match toplevel.get("format") {
    None => src.default_materials_format()?,
    Some(v) => {
      let v = v.as_integer().ok_or_else(|| MFVE::Other("not an integer"))?;
      materials_format::Version::try_from_integer(v)?
    },
  };

  let mut l = Catalogue {
    bundle: src.bundle(),
    libname: libname.to_string(),
    items: HashMap::new(),
    dirname: src.svg_dir(),
  };
  let empty_table = toml::value::Value::Table(default());
  let groups = toplevel
    .get("group").unwrap_or(&empty_table)
    .as_table().ok_or_else(|| LLE::ExpectedTable(format!("group")))?;
  for (groupname, gdefn) in groups {
    (||{

    let gdefn = resolve_inherit(INHERIT_DEPTH_LIMIT,
                                groups, groupname, gdefn)?;
    let gdefn: GroupDefn = TV::Table(gdefn.into_owned()).try_into()?;
    let d = if mformat == 1 {
      let scale = gdefn.d.scale_mf1(mformat)?;
      GroupDetails {
        size: gdefn.d.size.iter().map(|s| s * scale).collect(),
        ..gdefn.d
      }
    } else {
      gdefn.d // v2 isn't going to do this, do this right now
    };
    let group = Arc::new(GroupData {
      groupname: groupname.clone(),
      d, mformat,
    });

    // We do this here rather than in the files loop because
    //  1. we want to check it even if there are no files specified
    //  2. this is OK because the group doesn't change from here on
    let shape_calculable = group.check_shape()?.into();

    if [
      group.d.flip,
      group.d.back.is_some(),
    ].iter().filter(|x|**x).count() > 1 {
      throw!(LLE::MultipleMultipleFaceDefinitions)
    }

    for fe in gdefn.files.0 {
      process_files_entry(
        src.svg_noter(), &mut l,
        &gdefn.item_prefix, &gdefn.item_suffix, &gdefn.sort,
        &group, shape_calculable, fe
      )?;
    }

    Ok(())
    })().map_err(|error| LLE::InGroup {
      group: groupname.to_string(),
      error: Box::new(error),
    })?
  }

  Ok(l)
  })().map_err(|error| LLE::InLibrary {
    lib: libname.into(),
    error: Box::new(error),
  })?
}

#[throws(SubstError)]
fn subst_general(input: &str, needle: &'static str, replacement: &str)
                 -> (String, usize) {
  let mut count = 0;
  let mut work = input.to_owned();
  for m in input.rmatch_indices(needle) {
    count += 1;
    let mut lhs = &work[0.. m.0];
    let mut rhs = &work[m.0 + m.1.len() ..];
    if replacement.is_empty() {
      let lhs_trimmed = lhs.trim_end();
      if lhs_trimmed.len() != lhs.len() {
        lhs = lhs_trimmed;
      } else {
        rhs = rhs.trim_start();
      } 
    }
    work = lhs
      .to_owned()
      + replacement
      + rhs
  }
  (work, count)
}

#[throws(SubstError)]
fn subst(before: &str, needle: &'static str, replacement: &str) -> String {
  use SubstErrorKind as SEK;
  let err = |kind| SubstError { kind, input: before.to_string() };
  let (out, count) = subst_general(before, needle, replacement)?;
  if count == 0 { throw!(err(SEK::MissingToken(needle))) }
  if count > 1 { throw!(err(SEK::RepeatedToken(needle))) }
  out
}

#[throws(SubstError)]
fn substn(before: &str, needle: &'static str, replacement: &str) -> String {
  subst_general(before, needle, replacement)?.0
}

#[test]
fn test_subst() {
  use SubstErrorKind as SEK;
  assert_eq!(subst("a _colour die", "_colour", "blue").unwrap(),
             "a blue die");
  assert_eq!(subst("a _colour die", "_colour", "").unwrap(),
             "a die");
  assert_eq!(subst("a die", "_colour", "").unwrap_err().kind,
             SEK::MissingToken("_colour"));
  assert_eq!(subst("a _colour _colour die", "_colour", "").unwrap_err().kind,
             SEK::RepeatedToken("_colour"));

  assert_eq!(substn("a _colour die being _colour", "_colour", "blue").unwrap(),
             "a blue die being blue");
  assert_eq!(subst_general("a _colour _colour die", "_colour", "").unwrap(),
             ("a die".to_owned(), 2));
}

#[throws(LibraryLoadError)]
fn format_item_name(item_prefix: &str, fe: &FileData, item_suffix: &str)
                    -> GoodItemName {
  format!("{}{}{}", item_prefix, fe.item_spec, item_suffix)
    .try_into()?
}

#[throws(LibraryLoadError)]
fn process_files_entry(
  src: &mut dyn LibrarySvgNoter, l: &mut Catalogue,
  item_prefix: &str, item_suffix: &str, sort: &str,
  group: &Arc<GroupData>, shape_calculable: ShapeCalculable,
  fe: FileData
) {
  let item_name = format_item_name(item_prefix, &fe, item_suffix)?;

  let sort = match (sort, fe.extra_fields.get("sort")) {
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
      let src_name  = subst(&fe.src_file_spec, "_c", &colour.0);
      let item_name: GoodItemName = item_name.try_into()?;
      let item_name = SvgBaseName::note(
        src, item_name, src_name.as_ref().map(|s| s.as_str()),
      )?;
      let desc = subst(&fe.desc, "_colour", "")?.to_html();
      OccData::Internal(Arc::new(OccData_Internal {
        item_name,
        loaded: default(),
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

  fn colour_subst_1<'s, S>(subst: S, kv: Option<(&'static str, &'s str)>)
      -> impl for <'i> Fn(&'i str) -> Result<Cow<'i, str>, SubstError> + 's
  where S: Fn(&str, &'static str, &str) -> Result<String, SubstError> + 's
  {
    move |input| Ok(
      if let Some((keyword, val)) = kv {
        subst(input, keyword, val)?.into()
      } else {
        input.into()
      }
    )
  }

  let mut add1 = |
    c_colour: Option<(&'static str, &str)>,
    c_abbrev: Option<(&'static str, &str)>,
  | {
    let c_colour_all = colour_subst_1(substn, c_colour);
    let c_colour = colour_subst_1(subst, c_colour);
    let c_abbrev = colour_subst_1(subst, c_abbrev);

    let sort = sort.as_deref().map(|v| c_abbrev(v)).transpose()?;
    let sort = sort.map(|s| s.into_owned());

    let subst_item_name = |item_name: &GoodItemName| {
      let item_name = c_abbrev(item_name.as_str())?;
      let item_name = item_name.into_owned().try_into()?;
      Ok::<_,LLE>(item_name)
    };
    let item_name = subst_item_name(&item_name)?;

    let src_name = c_abbrev(&fe.src_file_spec);
    let src_name = src_name.as_deref();

    let desc = c_colour(&fe.desc)?;

    let desc = if let Some(desc_template) = &group.d.desc_template {
      subst(desc_template, "_desc", &desc)?.to_html()
    } else {
      (&*desc).to_html()
    };

    let idata = ItemData {
      group: group.clone(),
      occ: occ.clone(),
      sort,
      shape_calculable,
      d: Arc::new(ItemDetails { desc }),
    };
    l.add_item(src, src_name, &item_name, CatEnt::Item(idata))?;

    if let Some(magic) = &group.d.magic { 
      // Ideally the toml crate would have had let us build an inline
      // table.  But in fact it won't even toml-escape the strings without
      // a fuss, so we bodge it with strings:
      let image_table = format!(
        r#"{{ type="Lib", lib="{}", item="{}" }}"#,
        TomlQuote(&l.libname), TomlQuote(item_name.as_str())
      );

      let item_name = subst_item_name(&format_item_name(
        &magic.item_prefix, &fe, &magic.item_suffix)?)?;

      let spec = regex!(r#"(?m)(^[\sA-Za-z0-9._-]+=\s*)!\s*$"#)
        .replace_all(&magic.template, |caps: &regex::Captures| {
          format!("{}{}", caps.get(1).unwrap().as_str(), &image_table)
        });
      let spec = c_colour_all(&spec)?;

      trace!("magic item {}\n\n{}\n", &item_name, &spec);

      let spec: Box<dyn PieceSpec> = toml_de::from_str(&spec)
        .map_err(|error| LLE::TemplatedTomlError {
          toml: spec.into_owned(),
          error,
        })?;

      l.add_item(&mut NullLibrarySvgNoter, // there's no SVG for *this* item
                 src_name, &item_name, CatEnt::Magic {
        group: group.clone(),
        spec: spec.into(),
      })?;
    }

    Ok::<_,LLE>(())
  };

  if group.d.colours.is_empty() {
    add1(None, None)?;
  } else {
    for (colour, recolourdata) in &group.d.colours {
      add1(Some(("_colour", colour)),
           Some(("_c", &recolourdata.abbrev)))?;
    }
  }
}

impl Catalogue {
  #[throws(LLE)]
  fn add_item(&mut self,
              src: &mut dyn LibrarySvgNoter,
              src_name: Result<&str,&SubstError>,
              item_name: &GoodItemName, catent: CatalogueEntry) {
    type H<'e,X,Y> = hash_map::Entry<'e,X,Y>;

    let new_item = SvgBaseName::note(
      src, item_name.clone(), src_name.clone()
    )?;

    match self.items.entry(new_item) {
      H::Occupied(oe) => throw!(LLE::DuplicateItem {
        item: item_name.as_str().to_owned(),
        group1: oe.get().group().groupname.clone(),
        group2: catent.group().groupname.clone(),
      }),
      H::Vacant(ve) => {
        debug!("loaded shape {} {}", &self.libname, item_name.as_str());
        ve.insert(catent);
      }
    };
  }
}

//---------- reading, support functions ----------

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

impl TryFrom<String> for FileList {
  type Error = LLE;
//  #[throws(LLE)]
  fn try_from(s: String) -> Result<FileList,LLE> {
    let mut o = Vec::new();
    let mut xfields = Vec::new();
    for (lno,l) in s.lines().enumerate() {
      let l = l.trim();
      if l=="" || l.starts_with('#') { continue }
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
      let src_file_spec = n()?;
      let extra_fields = xfields.iter()
        .map(|field| Ok::<_,LLE>((field.to_owned(), n()?)))
        .collect::<Result<_,_>>()?;
      let desc = remain.to_owned();
      o.push(FileData{ item_spec, src_file_spec, extra_fields, desc });
    }
    Ok(FileList(o))
  }
}

//==================== Registry ====================

impl Registry {
  pub fn add(&mut self, data: Catalogue) {
    self.libs
      .entry(data.libname.clone()).or_default()
      .push(data);
  }

  pub fn clear(&mut self) {
    self.libs.clear()
  }

  pub fn iter(&self) -> impl Iterator<Item=&[Catalogue]> {
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
  ig.all_shapelibs().iter().map(
    |reg| reg.libs.keys().cloned()
  ).flatten().collect()
}

impl<'ig> AllRegistries<'ig> {
  pub fn all_libs(&self) -> impl Iterator<Item=&[Catalogue]> {
    self.iter().map(|reg| &reg.libs).flatten().map(
      |(_libname, lib)| lib.as_slice()
    )
  }
  pub fn lib_name_lookup(&self, libname: &str) -> Result<&[Catalogue], SpE> {
    for reg in self.iter() {
      if let Some(r) = reg.libs.get(libname) { return Ok(r) }
    }
    return Err(SpE::LibraryNotFound);
  }
}

//==================== configu and loading global libs ====================

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
pub fn load_global_libs(libs: &[Config1]) {
  for l in libs {
    let libs = l.resolve()?;
    let n = libs.len();
    for e in libs {
      load_1_global_library(&e)?;
    }
    info!("loaded {} shape libraries from {:?}", n, &l);
          
  }
}
