// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub mod crates;
pub mod imports;
pub mod prelude;

pub mod authproofs;
pub mod childio;
pub mod config;
pub mod debugmutex;
pub mod digestrw;
pub mod keydata;
pub mod packetframe;
pub mod progress;
pub mod support;
pub mod termprogress;
pub mod timedfd;
pub mod tz;

#[path = "fake-rng.rs"]           pub mod fake_rng;
#[path = "fake-time.rs"]          pub mod fake_time;
#[path = "slotmap-slot-idx.rs"]   pub mod slotmap_slot_idx;
#[path = "toml-de.rs"]            pub mod toml_de;
