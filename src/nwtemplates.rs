// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

use parking_lot::MappedRwLockReadGuard;
use parking_lot::{const_rwlock, RwLock, RwLockReadGuard};

static STATE: RwLock<Option<State>> = const_rwlock(None);

struct State {
  tera: tera::Tera,
}

#[throws(StartupError)]
pub fn init() {
  let mut guard = STATE.write();
  assert!(guard.is_none());
  let config = config();
  let nwtemplate_dir = &config.nwtemplate_dir;
  let glob = format!("{}/*.tera", nwtemplate_dir);
  let tera = tera::Tera::new(&glob)
    .map_err(|e| anyhow!("{}", e))
    .context("load tamplates")
    .with_context(|| nwtemplate_dir.to_string())?;

  *guard = Some(State {
    tera,
  })
}

#[throws(anyhow::Error)]
pub fn render<D: Serialize>(template_name: &str, data: &D) -> String {
  fn get_tera() -> MappedRwLockReadGuard<'static, tera::Tera> {
    let g = STATE.read();
    RwLockReadGuard::map(g, |g| &g.as_ref().unwrap().tera)
  }
  get_tera().render(template_name, data)
    .map_err(|e| anyhow!(e.to_string()))
    ?
}
