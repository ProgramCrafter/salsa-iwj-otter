
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
pub mod http;
pub mod session;
pub mod api;
pub mod spec;
pub mod cmdlistener;
pub mod commands;
pub mod utils;
#[path="slotmap-slot-idx.rs"] pub mod slotmap_slot_idx;
