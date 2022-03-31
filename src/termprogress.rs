// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later There is NO WARRANTY.

use crate::prelude::*;

use unicode_width::UnicodeWidthChar;

const FORCE_VAR: &str = "OTTER_TERMPROGRESS_FORCE";

type Col = usize;

pub trait Reporter {
  fn report(&mut self, pi: &ProgressInfo<'_>);
  fn clear(&mut self);
}

pub struct Null;
impl Null {
  pub fn reporter() -> Box<dyn Reporter> { Box::new(Null) }
}

#[allow(unused_variables)]
impl Reporter for Null {
  fn report(&mut self, pi: &ProgressInfo<'_>) { }
  fn clear(&mut self) { }
}

pub fn reporter() -> Box<dyn Reporter> {
  let term = console::Term::buffered_stderr();

  let mut newlines = false;
  let mut recheck_width = true;
  let width = if let Ok(val) = env::var(FORCE_VAR) {
    let mut val = &val[..];
    if let Some(rhs) = val.strip_prefix("+") {
        val = rhs;
        newlines = true;
    }
    let width = val.parse()
      .expect(&format!("bad {} syntax", FORCE_VAR));
    recheck_width = false;
    Some(width)
  } else {
    if_chain!{
      if term.is_term();
      if let Some((_, width)) = term.size_checked();
      then { Some(width.into()) }
      else { None }
    }
  };

  if let Some(width) = width {
    Box::new(TermReporter {
      term, width, newlines, recheck_width,
      needs_clear: None,
      spinner: 0,
    })
  } else {
    Box::new(Null)
  }
}

pub struct TermReporter {
  term: console::Term,
  width: Col,
  needs_clear: Option<()>,
  spinner: usize,
  newlines: bool,
  recheck_width: bool,
}

const LHS_TARGET: Col = 25;
const LHS_FRAC: f32 = (LHS_TARGET as f32) / 78.0;
const SPINNER: &[char] = &['-', '\\', '/'];

impl Reporter for TermReporter {
  fn report(&mut self, pi: &ProgressInfo<'_>) {
    if self.recheck_width {
      if let Some((_, width)) = self.term.size_checked() {
        self.width = width.into()
      }
    }

    let mut out = String::new();
    let w = self.width;
    if let Some(w) = w.checked_sub(1) {
      out.push(SPINNER[self.spinner]);
      self.spinner += 1; self.spinner %= SPINNER.len();
      if let Some(w) = w.checked_sub(1) {
        let lhs = min(LHS_TARGET, ((w as f32) * LHS_FRAC) as Col);
        self.bar(&mut out, lhs,     &pi.phase);
        out.push('|');
        self.bar(&mut out, w - lhs, &pi.item);
      }
    }
    self.clear_line();
    if out.len() > 0 {
      if self.newlines {
        writeln!(&mut self.term, "{}", out).unwrap_or(());
      } else {
        self.needs_clear = Some(());
        self.term.write_str(&out).unwrap_or(());
      }
    }
    self.term.flush().unwrap_or(());
  }

  fn clear(&mut self) {
    self.clear_line();
    self.term.flush().unwrap_or(());
  }
}

impl TermReporter {
  fn clear_line(&mut self) {
    if let Some(()) = self.needs_clear.take() {
      self.term.clear_line().unwrap_or(());
    }
  }

  fn bar(&self, out: &mut String, fwidth: Col, info: &progress::Count) {
    let desc = console::strip_ansi_codes(&info.desc);
    let w_change = if info.n == 0 { 0 } else {
      min(
        ((info.i as f32) / (info.n as f32) * (fwidth as f32)) as Col,
        fwidth // just in case
      )
    };
    let mut desc = desc
      .chars()
      .chain(iter::repeat(' '))
      .peekable();
    let mut w_sofar = 0;

    let mut half = |stop_at|{
      let mut accumulate = String::new();
      loop {
        let &c = desc.peek().unwrap();
        if_let!{ Some(w) = c.width(); else continue; }
        let w_next = w_sofar + w;
        if w_next > stop_at { break }
        accumulate.push(c);
        w_sofar = w_next;
        let _ = desc.next().unwrap();
      }
      accumulate
    };

    let lhs = half(w_change);
    if lhs.len() > 0 {
      let style = console::Style::new().for_stderr().reverse();
      *out += &style.apply_to(lhs).to_string();
    }

    *out += &half(fwidth);
    out.extend( iter::repeat(' ').take( fwidth - w_sofar ));
  }
}

impl Drop for TermReporter {
  fn drop(&mut self) {
    self.clear();
  }
}

pub struct Nest {
  outer_n: usize,
  outer_i: usize,
  inner_last_phase: usize,
  actual_reporter: Box<dyn Reporter>,
}

impl Nest {
  /// Assumes that every inner phase is of the same length as the first
  pub fn new(outer_count: usize, actual_reporter: Box<dyn Reporter>)
             -> Self { Nest {
    actual_reporter,
    outer_n: outer_count,
    outer_i: 0,
    inner_last_phase: 0,
  } }
}

impl Reporter for Nest {
  fn report(&mut self, inner_pi: &ProgressInfo<'_>) {
    // Autodetect new outer phase item, when inner pahse rewinds
    if inner_pi.phase.i < self.inner_last_phase {
      self.outer_i += 1;
    }
    self.inner_last_phase = inner_pi.phase.i;

    let desc = if self.outer_n > 1 {
      format!("{}/{} {}", self.outer_i, self.outer_n,
              &inner_pi.phase.desc).into()
    } else  {
      inner_pi.phase.desc.clone()
    };

    let outer_phase = progress::Count {
      i: inner_pi.phase.i + inner_pi.phase.n * self.outer_i,
      n:                    inner_pi.phase.n * self.outer_n,
      desc,
    };

    let outer_pi = ProgressInfo {
      phase: outer_phase,
      item:  inner_pi.item.clone(),
    };

    self.actual_reporter.report(&outer_pi);
  }

  fn clear(&mut self) {
    self.actual_reporter.clear();
  }
}
