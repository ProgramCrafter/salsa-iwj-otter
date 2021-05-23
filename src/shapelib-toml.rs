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

  /// See the discussioin of the item name.
  /// These two fields default to `""`.
  #[serde(default)] pub item_prefix: String,
  #[serde(default)] pub item_suffix: String,

  /// The sort key (used for item sorting in hands).
  ///
  /// If neither the group property, nor the `files` extra field, are
  /// specified, the item name is used.
  ///
  /// If both are specified, the group property is used as a template.
  /// `_s` is replaced by the sort extra field from the `files` list
  /// `_c` is replaced by the colour, if applicable.
  #[serde(default)] pub sort: String,

  #[doc(hidden)]
  #[serde(flatten)] pub d: GroupDetails,
}

/// Details for a group of pieces. See also [`GroupDefn`].
///
/// This is separate from `GroupDefn` only to make the
/// implementation convenient (for complicated reasons).  At the
/// library TOML file level, there is no meaningful difference between
/// the fields in this struct, and the ones in `GroupDefn`
#[derive(Debug,Deserialize)]
pub struct GroupDetails {
  /// Causes this group to inherit every setting from the `GroupDefn`
  /// and `GroupDetails` of the group named by `inherit` (recursively,
  /// if applicable).
  ///
  /// When inheritance is happening, there is of course a difference
  /// between leaving a value unspecified, and specifying it to have
  /// the usual default value: the latter would override any inherited
  /// setting.
  #[cfg(doc)]
  inherit: String,

  /// Should be either a 1- or 2- element list, x and y, or just one
  /// number for both.
  // scaled when put into GroupData
  pub size: Vec<f64>,
  
  #[serde(default)]
  /// If specified, the input is first scaled from `orig_size` to
  /// `size`.  If both `size` and `orig_size` are 2 elements, may
  /// scale by different amounts in x and y.
  pub orig_size: Vec<f64>,

  #[serde(default)]
  /// Default if not supplied is the centre according to the size.
  pub centre: Option<[f64; 2]>,

  #[serde(default)]
  /// Default is `false`
  pub flip: bool,

  /// The back of this is some other item.  Doesn't make sense
  /// with `flip`.
  #[serde(default)]
  pub back: Option<Box <dyn PieceSpec>>,

  /// `size` and `centre` are in the SVG's own internal coordinate
  /// system, not the Otter scaled coordinates which result from
  /// multiplying by by this scale factor.
  /// If not specified, default is `1.0`.
  #[serde(default="num_traits::identities::One::one")]
  pub scale: f64,

  /// If specified and non-empty, specifies that this group should be
  /// instantiated multiple times, each time with a recolouring.
  ///
  /// For each entry in this table, the recolouring is applied to
  /// every item in the [`files`](GroupDefn::files) list.
  /// 
  /// When recolouring is being done, every effective item name must
  /// contain the substring `_c` exactly once, and every item
  /// description must contain the substring `_colour` exactly once.
  /// `_c` will be replaced with the recoluring's `abbrev`, and
  /// `_colour` with the recolouring name (the key of the `colours`
  /// table).
  #[serde(default)]
  pub colours: HashMap<String, RecolourData>,

  /// If specified, provides a template for the description.  The
  /// description previously calculated replaces `_desc` in this
  /// string.
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

/// An entry in the `colours` table, specifying one recolouring.
#[derive(Debug,Deserialize)]
pub struct RecolourData {
  /// The replacement for `_c`.  See `colours` in [`GroupDetails`].
  pub abbrev: String,

  #[cfg(doc)]
  /// Each entry is `"from" = "to"` where both are 6-character
  /// RGB hex strings (without the leading `#`).
  pub map: HashMap<String, String>,
}

#[doc(hidden)]
#[derive(Deserialize,Debug)]
#[serde(try_from="String")]
pub struct FileList(pub Vec<FileData>);

/// Contents of each line in [`files`](GroupDefn::files)
///
/// This is not a key value list.  The leading fields are found by
/// splitting on whitespace, and the final field is the rest of the
/// line.
#[derive(Deserialize,Debug)]
pub struct FileData {
  /// The core of the item name.  See `GroupDefn`.
  pub item_spec: String,

  /// The core of the remote file name, for pieces which are scraped.
  ///
  /// In bundles, is the source file name if it isn't `-`.
  /// In builtin libs,
  /// not used by Otter during runtime or when loading.
  ///
  /// When [`scraper.method`](LibraryTomlFile::scraper) is `"none"`,
  /// this field is not used and is conventionally set to "`-`".
  pub src_file_spec: String,

  /// Extra fields, normally not present.
  pub extra_fields: HashMap<String, String>,

  /// Desscription.  (Shown hn the game log, for example.)
  /// Will be HTML-escaped.
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

