// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(dead_code)] // todo

use crate::prelude::*;

pub use crate::prelude::Sha512Trunc256 as Digester;

#[derive(Debug,Copy,Clone,Hash,Eq,PartialEq,Serialize,Deserialize)]
pub struct Hash(pub [u8; 32]);

#[derive(Debug,Copy,Clone,Hash,Eq,PartialEq,Serialize,Deserialize)]
#[derive(EnumString,strum::Display,Ord,PartialOrd)]
pub enum Kind {
  #[strum(to_string="zip")]       Zip,
//  #[strum(to_string="game.toml")] GameSpec, // identification problems
}
impl Kind { pub fn only() -> Self { Kind::Zip } }

#[derive(Copy,Clone,Debug,Hash,PartialEq,Eq,Ord,PartialOrd)]
#[derive(Serialize,Deserialize)]
#[serde(transparent)]
pub struct Index(u16);
impl From<Index> for usize {
  fn from(i: Index) -> usize { i.0.into() }
}
impl TryFrom<usize> for Index {
  type Error = TryFromIntError;
  #[throws(Self::Error)]
  fn try_from(i: usize) -> Index { Index(i.try_into()?) }
}
impl Display for Index {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    write!(f, "{:05}", self.0)?;
  }
}
impl FromStr for Index {
  type Err = std::num::ParseIntError;
  #[throws(Self::Err)]
  fn from_str(s: &str) -> Index { Index(u16::from_str(s)?) }
}

const BUNDLES_MAX: Index = Index(64);

#[derive(Copy,Clone,Debug,Hash,PartialEq,Eq,Ord,PartialOrd)]
#[derive(Serialize,Deserialize)]
pub struct Id { index: Index, kind: Kind, }

#[derive(Debug,Clone)]
pub struct InstanceBundles {
  // todo: this vec is needed during loading only!
  bundles: Vec<Option<Note>>,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct Note {
  pub kind: Kind,
  pub state: State,
}

type BadBundle = String; // todo: make this a newtype

#[derive(Debug,Clone,Serialize,Deserialize)]
pub enum State {
  Uploading,
  BadBundle(BadBundle),
  Loaded(Loaded),
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct Loaded {
  meta: BundleMeta,
}

pub fn b_dir(instance: &InstanceName) -> String {
  savefilename(instance, "b-", "")
}
fn b_file<S>(instance: &InstanceName, index: Index, suffix: S) -> String
where S: Display + Debug
{
  format!("{}/{}.{}",
          savefilename(instance, "b-", ""),
          index, suffix)
}

impl Display for Id {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    write!(f, "{}.{}", self.index, self.kind)?
  }
}

impl Id {
  fn path_tmp(&self, instance: &InstanceName) -> String {
    b_file(instance, self.index, "tmp")
  }

  fn path(&self, instance: &InstanceName) -> String {
    b_file(instance, self.index, self.kind)
  }
}

#[derive(Error,Debug)]
enum IncorporateError {
  NotBundle(&'static str),
  IE(#[from] IE),
}
display_as_debug!{IncorporateError}
impl From<&'static str> for IncorporateError {
  fn from(why: &'static str) -> Self { Self::NotBundle(why) }
}

pub struct Uploading {
  instance: Arc<InstanceName>,
  id: Id,
  file: DigestWrite<Digester, BufWriter<fs::File>>,
}

#[throws(IE)]
fn load_bundle(ib: &mut InstanceBundles, ig: &mut Instance,
               id: Id, path: &str) {
  #[derive(Error,Debug)]
  enum LoadError {
    BadBundle(BadBundle),
    IE(#[from] IE),
  }
  display_as_debug!{LoadError}

  let iu: usize = id.index.into();

  match ib.bundles.get(iu) {
    None => ib.bundles.resize_with(iu+1, default),
    Some(None) => { },
    Some(Some(Note { kind:_, state: State::Uploading })) => { },
    Some(Some(ref note)) => throw!(IE::DuplicateBundle {
      index: id.index,
      kinds: [note.kind, id.kind],
    })
  };
  let slot = &mut ib.bundles[iu];

  #[throws(LoadError)]
  fn inner(_ig: &mut Instance,
           _id: Id, _path: &str) -> Loaded {
    Loaded { meta: BundleMeta { title: "title!".to_owned() } }
    // todo: find zipfile, read metdata toml
    // todo:: show in UI for download
    // todo: do actual things, eg libraries and specs
  }

  let state = match inner(ig,id,path) {
    Ok(loaded)                     => State::Loaded(loaded),
    Err(LoadError::BadBundle(bad)) => State::BadBundle(bad),
    Err(LoadError::IE(ie))         => throw!(ie),
  };

  *slot = Some(Note { kind: id.kind, state })
}

#[throws(IncorporateError)]
fn incorporate_bundle(ib: &mut InstanceBundles, ig: &mut Instance,
                      fpath: &str) {
  let fleaf = fpath.rsplitn(2, '/').next().unwrap();

  let [lhs, rhs] = fleaf.splitn(2, '.')
    .collect::<ArrayVec<[&str;2]>>()
    .into_inner().map_err(|_| "no dot")?;

  let index = lhs.parse().map_err(|_| "bad index")?;
  let kind = rhs.parse().map_err(|_| "bad extension")?;

  load_bundle(ib, ig, Id { index, kind }, fpath)?;
}

impl InstanceBundles {
  pub fn new() -> Self { InstanceBundles{ bundles: default() } }

  pub fn list(&self) -> MgmtBundleList {
    self.bundles.iter().enumerate().filter_map(|(index, slot)| {
      let note = slot.as_ref()?;
      let index = index.try_into().unwrap();
      Some((Id { index, kind: note.kind },
            note.state.clone()))
    }).collect()
  }

  #[throws(IE)]
  pub fn load_game_bundles(ig: &mut Instance) -> Self {
    let bd = b_dir(&ig.name);
    let mo = glob::MatchOptions {
      require_literal_leading_dot: true,
      ..default()
    };
    let mut ib = InstanceBundles::new();
    for fpath in
      glob::glob_with(&format!("{}/*", bd), mo)
      .context("pattern for bundle glob")?
    {
      let fpath = fpath.context("bundle glob")?;
      let fpath = fpath
        .to_str().ok_or_else(|| anyhow!("glob unicode conversion"))?;
      match incorporate_bundle(&mut ib, ig, fpath) {
        Ok(()) => { },
        Err(IncorporateError::NotBundle(why)) => {
          debug!("bundle file {:?} skippping {}", &fpath, why);
        }
        Err(IncorporateError::IE(ie)) => throw!(ie),
      }
    }
    debug!("loaded bundles {} {:?}", &ig.name, ib);
    ib
  }

  #[throws(MgmtError)]
  pub fn start_upload(&mut self, ig: &mut Instance, kind: Kind)
                      -> Uploading {
    let state = State::Uploading;
    let slot = Some(Note { kind, state });
    let index = self.bundles.len();
    if index >= usize::from(BUNDLES_MAX) { throw!(ME::TooManyBundles) }
    let index = index.try_into().unwrap();
    self.bundles.push(slot);
    let id = Id { kind, index };
    let tmp = id.path_tmp(&ig.name);

    let file = (|| Ok::<_,AE>({
      let mkf = || fs::File::create(&tmp);
      match mkf() {
        Ok(f) => f,
        Err(e) if e.kind() == ErrorKind::NotFound => {
          let d = b_dir(&ig.name);
          fs::create_dir(&d).context("create containing directory")?;
          mkf().context("crate (after creating containing directory)")?
        }
        e@ Err(_) => e.context("create")?,
      }
    }))()
      .with_context(|| tmp.to_owned()).context("upload file")
      .map_err(IE::from)?;
    
    let file = BufWriter::new(file);
    let file = DigestWrite::new(file);
    let instance = ig.name.clone();
    Uploading { file, instance, id }
  }
}

impl Uploading {
  #[throws(MgmtError)]
  pub fn bulk<R>(&mut self, data: &mut R)
  where R: Read
  {
    io::copy(data, &mut self.file)
      .with_context(|| self.id.path_tmp(&*self.instance))
      .context("copy").map_err(IE::from)?
  }
}

impl InstanceBundles {
  #[throws(MgmtError)]
  pub fn finish_upload(&mut self, ig: &mut Instance,
                       Uploading { instance:_, id, file }: Uploading,
                       expected: &Hash) {
    let (hash, mut file) = file.finish();
    let tmp = id.path_tmp(&ig.name);
    let install = id.path(&ig.name);
    file.flush()
      .with_context(|| tmp.clone()).context("flush").map_err(IE::from)?;
    if hash.as_slice() != &expected.0[..] { throw!(ME::UploadCorrupted) }
    load_bundle(self, ig, id, &tmp)?;
    match self.bundles.get(usize::from(id.index)) {
      Some(Some(Note { state: State::Loaded(..), .. })) => {
        fs::rename(&tmp, &install)
          .with_context(||install.clone())
          .context("install").map_err(IE::from)?;
      }
      Some(Some(Note { state: State::BadBundle(ref bad), .. })) => {
        throw!(ME::BadBundle(bad.clone()))
      }
      ref x => panic!("unexpected {:?}", x),
    };
  }
}
