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
  #[cfg(doc)]inherit: String, // handled specially
  pub size: Vec<f64>,  // scaled when put into GroupData
  #[serde(default)] pub orig_size: Vec<f64>,
  #[serde(default)] pub centre: Option<[f64; 2]>,
  #[serde(default)] pub flip: bool,
  #[serde(default)] pub back: Option<Box <dyn PieceSpec>>,
  #[serde(default="num_traits::identities::One::one")] pub scale: f64,
  #[serde(default)] pub colours: HashMap<String, RecolourData>,
  pub desc_template: Option<String>,

  /// If specified, pieces in this group can be occulted.
  pub occulted: Option<OccultationMethod>,

  /// One of `"Circle"` or `"Rect"`, to define the outline shape.
  /// The size is taken from `size`.
  ///
  /// This value is a string, not some weird Rust type, despite
  /// what you see here.
  #[serde(flatten)] pub outline: Box<dyn shapelib::OutlineDefn>,
}

/// How pieces may be occulted.  Currently only one supported way.
#[derive(Deserialize,Clone,Debug)]
#[serde(tag="method")]
pub enum OccultationMethod {
  /// When occulted, display as a piece of a particular colour.
  /// `colour` refers to one of the entries in
  /// `GroupDetails::colours`.
  ///
  /// The description will be different too: `_colour` will be elided,
  /// along with up to one of any spaces either side of it.
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
  /// [`scraper`](LibraryTomlFile::scraper) is interpreted.
  ///
  /// There are two methods available:
  ///
  ///  * `"none"`: Do not scrape anything.  The SVGs in the Otter
  ///  source tree are hand-edited.  The 2nd field in each
  ///  [`files`](GroupDefn::files) line
  ///  ([`r_file_spec`](FileData::r_file_spec)) is ignored.
  ///
  ///  * `"wikimedia"`: Scrape a site that uses Mediawiki the way that
  ///  Wikimedia does.  In this case
  ///  [`scraper`](LibraryTomlFile::scraper) is a table containing the
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

