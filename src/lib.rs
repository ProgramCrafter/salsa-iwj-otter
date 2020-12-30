// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![feature(proc_macro_hygiene, decl_macro)]
#![feature(slice_strip)]

#![allow(clippy::redundant_closure_call)]

pub mod accounts;
pub mod authproofs;
pub mod commands;
pub mod config;
pub mod debugreader;
pub mod error;
pub mod gamestate;
pub mod global;
pub mod imports;
pub mod keydata;
pub mod lens;
pub mod mgmtchannel;
pub mod nwtemplates;
pub mod pieces;
pub mod shapelib;
pub mod spec;
pub mod sse;
pub mod tz;
pub mod updates;
pub mod ui;
pub mod utils;

#[path = "slotmap-slot-idx.rs"]   pub mod slotmap_slot_idx;
#[path = "toml-de.rs"]            pub mod toml_de;
