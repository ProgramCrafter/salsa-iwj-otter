// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use parking_lot::Mutex;

static TERA : Mutex<Option<State>> = const_mutex(None);

pub fn template_render(name: &str) {
  let guard = TERA.lock();
  let tera = 
}

struct State {
  tera: tera::Tera;
}

impl Default for State {
  fn default() -> State { State {
    tera: tera::new(
  } }
}

type TeraWrapper = RwLock<Arc<State>>;

impl State {
  
}
