// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![feature(proc_macro_hygiene, decl_macro)]
#![feature(slice_strip)]

#![allow(clippy::redundant_closure_call)]

pub mod imports;
pub mod global;
pub mod pieces;
pub mod gamestate;
pub mod keydata;
pub mod updates;
pub mod sse;
pub mod error;
pub mod session;
pub mod api;
pub mod spec;
pub mod cmdlistener;
pub mod commands;
pub mod utils;
pub mod mgmtchannel;
pub mod debugreader;
pub mod shapelib;
pub mod tz;
pub mod accounts;
pub mod config;
pub mod nwtemplates;
#[path="toml-de.rs"] pub mod toml_de;
#[path="slotmap-slot-idx.rs"] pub mod slotmap_slot_idx;
