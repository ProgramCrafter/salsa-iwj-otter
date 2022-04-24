// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use otter::prelude::*;

use ansi_term::Style;
use flexi_logger::{style, TS_DASHES_BLANK_COLONS_DOT_BLANK};
use flexi_logger::{AdaptiveFormat, DeferredNow, FormatFunction, Record};

#[throws(io::Error)]
fn generic_format(w: &mut dyn io::Write,
                  now: &mut DeferredNow, record: &Record,
                  style: Style) {
  write!(w, "[{}] {} [{}] {}:{}: {}",
         now.format(TS_DASHES_BLANK_COLONS_DOT_BLANK),
         style.paint(record.level().to_string()),
         record.module_path().unwrap_or("<unnamed>"),
         record.file().unwrap_or("<unnamed>"),
         record.line().unwrap_or(0),
         &record.args())?;
}

#[throws(io::Error)]
fn basic_format(w: &mut dyn std::io::Write,
                       now: &mut DeferredNow, record: &Record) {
  generic_format(w, now, record, default())?;
}

#[throws(io::Error)]
fn coloured_format(w: &mut dyn io::Write,
                   now: &mut DeferredNow, record: &Record) {
  generic_format(w, now, record, style(record.level()))?;
}

pub const BASIC_FORMAT: FormatFunction = basic_format;
pub const ADAPTIVE_FORMAT: AdaptiveFormat = AdaptiveFormat::Custom(
  basic_format, coloured_format
);

#[throws(StartupError)]
pub fn setup() {
  flexi_logger::Logger::with(log_config())
    .format(BASIC_FORMAT)
    .adaptive_format_for_stderr(ADAPTIVE_FORMAT)
    .adaptive_format_for_stdout(ADAPTIVE_FORMAT)
    .start()?;
}
