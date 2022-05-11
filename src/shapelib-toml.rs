// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use crate::prelude::*;

#[doc(hidden)] pub type LLE = shapelib::LibraryLoadError;

// At the implementation level, each loaded item contains an
// `Arc<GroupDetails>`, which is simply stored directly.  The
// `GroupDefn` is processed.
#[derive(Debug,Deserialize)]
pub struct GroupDefn {
  pub files: FileList,
  #[serde(default)] pub item_prefix: String,
  #[serde(default)] pub item_suffix: String,
  #[serde(default)] pub sort: String,
  #[serde(flatten)] pub d: GroupDetails,
}

#[derive(Debug,Deserialize)]
pub struct GroupDetails {
  #[cfg(doc)] inherit: String, // handled specially
  #[serde(default)] pub size: Vec<f64>, // scaled in GroupData in mf1
  #[serde(default)] pub orig_size: Vec<f64>,
  #[serde(default)] pub centre: Option<[f64; 2]>,
  #[serde(default)] pub flip: bool,
  #[serde(default)] pub back: Option<Box <dyn PieceSpec>>,
  #[serde(default)] pub scale: Option<ScaleDetails>,
  #[serde(default)] pub colours: HashMap<String, RecolourData>,
  pub desc_template: Option<String>,
  pub occulted: Option<OccultationMethod>,
  #[serde(flatten)] pub outline: OutlineDetails,
}

#[derive(Debug,Deserialize,Copy,Clone)]
#[serde(untagged)]
pub enum ScaleDetails {
  Fit(ScaleFitDetails),
  Scale(f64),
  Stretch([f64;2]),
}

#[derive(Debug,Deserialize,Copy,Clone)]
pub enum ScaleFitDetails { Fit, Cover, Stretch }

#[derive(Debug,Deserialize)]
#[serde(untagged)]
pub enum OutlineDetails {
  Full(FullOutlineDetails), // introduced with mformat=2
  Shape(Box<dyn shapelib::OutlineDefn>),
}

#[derive(Debug,Deserialize)]
pub struct FullOutlineDetails {
  shape: Box<dyn shapelib::OutlineDefn>,
  #[serde(default)] size: Vec<f64>,
  #[serde(default)] scale: Option<f64>,
}

impl OutlineDetails {
  // enum_access could perhaps do this but controlling the serde
  // would become confusing
  pub fn shape(&self) -> &dyn shapelib::OutlineDefn { match self {
    OutlineDetails::Full(full) => &*full.shape,
    OutlineDetails::Shape(shape) => &**shape,
  }}
  pub fn size_scale(&self) -> (&[f64], Option<&f64>) { match self {
    OutlineDetails::Full(full) => (&full.size, full.scale.as_ref()),
    OutlineDetails::Shape(_) => default(),
  }}
}

#[derive(Deserialize,Clone,Debug)]
#[serde(tag="method")]
pub enum OccultationMethod {
  ByColour {
    colour: ColourSpec,
  },
  ByBack {
    ilk: OccultIlkName,
  },
}

#[derive(Debug,Deserialize)]
pub struct RecolourData {
  pub abbrev: String,
  #[cfg(doc)] pub map: HashMap<String, String>,
}

#[doc(hidden)]
#[derive(Deserialize,Debug)]
#[serde(try_from="String")]
pub struct FileList(pub Vec<FileData>);

#[derive(Deserialize,Debug)]
pub struct FileData {
  pub item_spec: String,
  pub src_file_spec: String,
  pub extra_fields: HashMap<String, String>,
  pub desc: String,
}

#[cfg(doc)]
/// `scraper`, specifying where and how to get updated piece SVGs.
pub struct Scraper {
  /// Determines which scraper is run and the rest of the table
  /// `scraper` is interpreted.
  ///
  /// There are two methods available:
  ///
  ///  * `"none"`: Do not scrape anything.  The SVGs in the Otter
  ///  source tree are hand-edited.  The 2nd field in each
  ///  [`files`](GroupDefn::files) line
  ///  ([`src_file_spec`](FileData::src_file_spec)) is ignored.
  ///
  ///  * `"wikimedia"`: Scrape a site that uses Mediawiki the way that
  ///  Wikimedia does.  In this case
  ///  `scraper` is a table containing the
  ///  fields of [`WikimediaScraper`], not just `method`.
  ///
  ///  * `"cards-oxymoron`": Special for that subdirectory.
  pub method: String,
}

#[cfg(doc)]
/// Settings to go alongside `scraper = "wikimedia"`
///
/// TODO: Most fields here yet to be documented!
pub struct WikimediaScraper {
  /// See [`method` in `Scraper`](Scraper::method).  `"wikimedia"`
  pub method: String,

}

