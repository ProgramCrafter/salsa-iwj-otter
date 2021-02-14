// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

//! This module has the documentation for the `library/*.toml`
//! library spec files.
//!
//! It is in the form of Rust data structures, as will be parsed from
//! the [TOML](https://toml.io/en/) by Rust's `serde` facility.  Each
//! Rust `struct` corresponds to a TOML table.  A `HashMap` is also a
//! TOML Table (ie, a `key = value` mapping).  A `Vec` is a TOML Array
//! (ie, a list).  (There are a number of wrinkles where the parsing
//! deviates from these conventions; these are documented explicitly.)
//!
//! Each `*.toml` file contains the information in
//! [`LibraryTomlFile`], so start there.
//!
//! It is probably best to read this documentation in conjuncton with
//! the provided `library/wikimedia.toml` and `library/edited.toml`,
//! which demonstrate the use of the various features.

pub use crate::prelude::*;

#[doc(hidden)] pub type LLE = shapelib::LibraryLoadError;

#[cfg(doc)]
/// Each file `library/*.toml` contains this.
///
/// (Ignore the "Trait implementations" and everything that follows.)
pub struct LibraryTomlFile {
  /// A TOML table of groups.  Each group has a name, a can specify
  /// various (inheritable) properties, and also gives a list of
  /// indvidual SVG files which should be processed this way.
  pub groups: HashMap<String, GroupDefn>,

  /// Configuration for the scraper.
  ///
  /// The scraper is never automatically run during the build.  If you
  /// updated the TOML file in a way that means files should be
  /// re-downloaded, you should re-run `./media-scraper
  /// library/`_lib_`.toml`
  pub scraper: Scraper
}

/// Details for a group of pieces. See also [`GroupDetails`].
///
/// This is separate from `GroupDetails` only to make the
/// implementation convenient (for complicated reasons).  At the
/// library TOML file level, there is no meaningful difference between
/// the fields in this struct, and the ones in `GroupDetails`
#[derive(Debug,Deserialize)]
pub struct GroupDefn {
  /// `files` is a multi-line string, each line of which has three
  /// fields (the first two terminated by whitespace).  The fields
  /// are those in [`FileData`].  `#` comments are supported.
  ///
  /// Each non-empty non-comment line in `files` specifies a single
  /// SVG to be made aavailable as a piece.
  ///
  /// The piece has am **item name** which is used within Otter to
  /// refer to the piece (for example, with `otter library-add`).
  /// The SVG filename is derived from the item name, as follows:
  /// `library/`_lib_`/`_itemname_`.svg`.
  ///
  /// The `itemname` is the `item_spec` from each entry in `files`
  /// (see [`FileData`] sandwiched between `item_prefix` and
  /// `item_suffix`
  ///
  /// Item names are conventionally structured using a hierarchical
  /// name with `-` between the components.  Do not put `/` or `_` in
  /// item names.
  pub files: FileList,

  /// See the discussioin of the item name.
  /// These two fields default to `""`.
  #[serde(default)] pub item_prefix: String,
  #[serde(default)] pub item_suffix: String,

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
  /// Default if not supplied is `[0,0]`.
  pub centre: [f64; 2],

  #[serde(default)]
  /// Default is `false`
  pub flip: bool,

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

  /// One of `"Circle"` or `"Square"`, to define the outline shape.
  /// The size is taken from `size`.
  ///
  /// This value is a string, not some weird Rust type, despite
  /// what you see here.
  #[serde(flatten)] pub outline: Box<dyn shapelib::OutlineDefn>,
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
/// This is not a key value list.  The first two fields are found by
/// splitting on whitespace, and the final field is the rest of the
/// line.
#[derive(Deserialize,Debug)]
pub struct FileData {
  /// The core of the item name.  See `GroupDefn`.
  pub item_spec: String,

  /// The core of the remote file name, for pieces which are scraped.
  ///
  /// Not used by Otter during runtime or when loading pieces.
  ///
  /// When [`scraper.method`](LibraryTomlFile::scraper) is `"none"`,
  /// this field is not used and is conventionally set to "`-`".
  #[cfg(doc)] pub r_file_spec: String,
  #[cfg(not(doc))] pub r_file_spec: (),

  /// HTML desscription.  (Shown hn the game log, for example.)
  pub desc: Html,
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

