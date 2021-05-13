// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later There is NO WARRANTY.

use crate::prelude::*;

use unicode_width::UnicodeWidthChar;

type Col = usize;

pub trait Reporter {
  fn report(&mut self, pi: &ProgressInfo<'_>);
}

pub struct NullReporter;

#[allow(unused_variables)]
impl Reporter for NullReporter {
  fn report(&mut self, pi: &ProgressInfo<'_>) { }
}

pub fn new() -> Box<dyn Reporter> {
  let term = console::Term::buffered_stderr();
  if_chain!{
    if term.is_term();
    if let Some((_, width)) = term.size_checked();
    then {
      Box::new(TermReporter {
        term,
        width: width.into(),
        needs_clear: None,
      })
    } else {
      Box::new(NullReporter)
    }
  }
}

pub struct TermReporter {
  term: console::Term,
  width: Col,
  needs_clear: Option<()>,
}

const LHS_TARGET: Col = 25;
const LHS_FRAC: f32 = (LHS_TARGET as f32)/80.0;

impl Reporter for TermReporter {
  fn report(&mut self, pi: &ProgressInfo<'_>) {
    if let Some((_, width)) = self.term.size_checked() {
      self.width = width.into()
    }

    let mid = min(LHS_TARGET, ((self.width as f32) * LHS_FRAC) as Col);
    let mut out = String::new();
    self.bar(&mut out, mid,              &pi.phase);
    self.bar(&mut out, self.width - mid, &pi.item);
    self.clear_line();
    self.needs_clear = Some(());
    self.term.write_str(&out).unwrap_or(());
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
        LHS_TARGET
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
    self.clear_line();
    self.term.flush().unwrap_or(());
  }
}
