// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use parking_lot::RwLock;

static STATE : RwLock<Option<State>> = const_mutex(None);

#[throws(StartupError)]
pub fn init() {
  let guard = STATE.write();
  assert!(guard.is_none());
  let glob = format!("{}/*.tera", config().nwtemplates);
  *guard = State {
    tera: tera::new(&glob)?,
  };
}

#[throws(tera::Error)]
pub fn template_render<D: Serialize>(template_name: &str, data: &D) {
  fn get_st() -> MappedRwLockReadGuard<State> {
    STATE.read().as_ref().unwrap()
  }
  get_st().render(template_name, data)
}
