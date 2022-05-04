// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

//! Otter game system; common infrastructure Rust crate.
//!
//! Otter, the Online Table Top Environment Renderer,
//! is an online game system.
//!
//! <https://www.chiark.greenend.org.uk/~ianmdlvl/otter/docs/README.html>
//!
//! This crate is intended for use only by other parts of Otter.
//!
//! The command line client for joining and managing games is
//! available via
//! [`cargo install otter-cli`](https://lib.rs/crates/otter-cli)
//!
//! To run an Otter server, you will need to read the
//! [build instructions](https://www.chiark.greenend.org.uk/~ianmdlvl/otter/docs/build.html).

pub mod imports;
pub mod prelude;

pub mod accounts;
pub mod asseturl;
pub mod authproofs;
pub mod bundles;
pub mod childio;
pub mod clock;
pub mod commands;
pub mod config;
pub mod currency;
pub mod deck;
pub mod dice;
pub mod digestrw;
pub mod debugmutex;
pub mod debugreader;
pub mod error;
pub mod fastsplit;
pub mod gamestate;
pub mod global;
pub mod hand;
pub mod hidden;
pub mod keydata;
pub mod mgmtchannel;
pub mod nwtemplates;
pub mod occultilks;
pub mod organise;
pub mod packetframe;
pub mod pcaliases;
pub mod pcrender;
pub mod pieces;
pub mod progress;
pub mod shapelib;
pub mod spec;
pub mod termprogress;
pub mod timedfd;
pub mod tz;
pub mod updates;
pub mod ui;
pub mod utils;

#[path = "fake-rng.rs"]           pub mod fake_rng;
#[path = "fake-time.rs"]          pub mod fake_time;
#[path = "materials-format.rs"]   pub mod materials_format;
#[path = "shapelib-toml.rs"]      pub mod shapelib_toml;
#[path = "slotmap-slot-idx.rs"]   pub mod slotmap_slot_idx;
#[path = "toml-de.rs"]            pub mod toml_de;
