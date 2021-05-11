// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![feature(min_type_alias_impl_trait)]
#![feature(io_into_inner_error_parts)]
#![allow(clippy::redundant_closure_call)]

pub mod imports;
pub mod prelude;

pub mod accounts;
pub mod asseturl;
pub mod authproofs;
pub mod bundles;
pub mod clock;
pub mod commands;
pub mod config;
pub mod deck;
pub mod debugreader;
pub mod error;
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
pub mod sse;
pub mod tz;
pub mod updates;
pub mod ui;
pub mod utils;

#[path = "shapelib-toml.rs"]      pub mod shapelib_toml;
#[path = "slotmap-slot-idx.rs"]   pub mod slotmap_slot_idx;
#[path = "toml-de.rs"]            pub mod toml_de;
#[path = "fake-rng.rs"]           pub mod fake_rng;
