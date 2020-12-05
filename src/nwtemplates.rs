// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

use parking_lot::{const_rwlock, RwLock, MappedRwLockReadGuard};

static STATE : RwLock<Option<State>> = const_rwlock(None);

struct State {
  tera: tera::Tera,
}

#[throws(StartupError)]
pub fn init() {
  let guard = STATE.write();
  assert!(guard.is_none());
  let glob = format!("{}/*.tera", config().nwtemplates);
  *guard = State {
    tera: tera::Tera::new(&glob)?,
  };
}

#[throws(tera::Error)]
pub fn render<D: Serialize>(template_name: &str, data: &D) {
  fn get_st() -> MappedRwLockReadGuard<'static, State> {
    STATE.read().as_ref().unwrap()
  }
  get_st().render(template_name, data)
}
