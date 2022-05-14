// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

use parking_lot::MappedRwLockReadGuard;
use parking_lot::{const_rwlock, RwLock, RwLockReadGuard};

static STATE: RwLock<Option<State>> = const_rwlock(None);

struct State {
  tera: Tera,
}

#[throws(StartupError)]
pub fn init_from_config() {
  init_from_dir(&config().nwtemplate_dir)?;
}

#[throws(StartupError)]
pub fn init_from_dir(nwtemplate_dir: &str) {
  let mut guard = STATE.write();
  assert!(guard.is_none());
  let glob = format!("{}/*.tera", nwtemplate_dir);
  let tera = Tera::new(&glob)
    .map_err(|e| anyhow!("{}", e))
    .context("load tamplates")
    .with_context(|| nwtemplate_dir.to_string())?;

  if tera.get_template_names().next().is_none() {
    warn!("nwtemplates directory {:?} contains no templates!",
          nwtemplate_dir);
  }

  *guard = Some(State {
    tera,
  })
}

#[throws(anyhow::Error)]
pub fn render<D>(template_name: &str, data: &D) -> String
where D: Serialize + Debug
{
  fn get_tera() -> MappedRwLockReadGuard<'static, Tera> {
    let g = STATE.read();
    RwLockReadGuard::map(g, |g| &g.as_ref().unwrap().tera)
  }
  let context = tera::Context::from_serialize(data)
    .with_context(
      || format!("failed make context from serializable {:?}", data)
    )
    .map_err(IE::from)?;
  get_tera().render(template_name, &context).map_err(|e| {
    error!("template render error: {:?}", &e);
    anyhow!(e.to_string())
  })?
}
