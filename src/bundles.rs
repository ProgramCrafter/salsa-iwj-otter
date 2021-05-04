// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(dead_code)] // todo

use crate::prelude::*;

pub use crate::prelude::Sha512Trunc256 as Digester;
pub type DigestWrite<W> = crate::utils::DigestWrite<Digester, W>;

#[derive(Debug,Copy,Clone,Hash,Eq,PartialEq,Serialize,Deserialize)]
pub struct Hash(pub [u8; 32]);

#[derive(Debug,Copy,Clone,Hash,Eq,PartialEq,Serialize,Deserialize)]
#[derive(EnumString,strum::Display,Ord,PartialOrd)]
pub enum Kind {
  #[strum(to_string="zip")]       Zip,
//  #[strum(to_string="game.toml")] GameSpec, // identification problems
}
impl Kind { pub fn only() -> Self { Kind::Zip } }

#[derive(Copy,Clone,Default,Debug,Hash,PartialEq,Eq,Ord,PartialOrd)]
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
hformat_as_display!{Id}

const BUNDLES_MAX: Index = Index(64);

#[derive(Clone,Serialize,Deserialize)]
pub enum AssetUrlKey {
  Dummy,
  Y(AssetUrlKeyRaw),
}
type AssetUrlKeyRaw = [u8; 32];
impl Debug for AssetUrlKey {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    use AssetUrlKey::*;
    match self {
      Y(_) => write!(f, "AssetUrlKey::Y{{..}}")?,
      Dummy => write!(f, "AssetUrlKey::Dummy")?,
    }
  }
}
impl Default for AssetUrlKey { fn default() -> Self { Self::Dummy } }
impl AssetUrlKey {
  #[throws(IE)]
  pub fn new_random() -> AssetUrlKey {
    let mut buf: AssetUrlKeyRaw = default();
    let mut rng: rand::rngs::ThreadRng = thread_rng();
    rand::RngCore::try_fill_bytes(&mut rng, &mut buf)
      .context("generate new AssetUrlKey")?;
    AssetUrlKey::Y(buf)
  }
}
type AssetUrlTokenRaw = digest::Output<Digester>;
#[derive(Clone)]
pub struct AssetUrlToken(AssetUrlTokenRaw);
impl Debug for AssetUrlToken {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) { write!(f, "AssetUrlToken{{..}}")?; }
}
impl AssetUrlKey {
  pub fn token<V>(&self, what: &str, v: V) -> AssetUrlToken
  where V: Serialize {
    let k = match self {
      AssetUrlKey::Y(k) => k,
      _ => panic!("dummy AssetUrlKey being used!"),
    };
    let mut dw = DigestWrite::sink();
    write!(dw, "{}\0", what).unwrap();
    dw.write(&k[..]).unwrap();
    rmp_serde::encode::write(&mut dw, &v).expect("serialize failed!");
    AssetUrlToken(dw.finish().0)
  }

  #[throws(BadAssetUrlToken)]
  pub fn check<V>(&self, what: &str, v: &V, got: &AssetUrlToken)
                  -> Authorisation<V>
  where V: Serialize {
    let exp = self.token(what, v);
    if ! bool::from(ConstantTimeEq::ct_eq(
      &exp.0[..],
      &got.0[..],
    )) { throw!(BadAssetUrlToken) }
    else { Authorisation::authorised(v) }
  }
}
impl Display for AssetUrlToken {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    f.write_str(&base64::encode_config(&self.0, base64::URL_SAFE_NO_PAD))?
  }
}
impl FromStr for AssetUrlToken {
  type Err = BadAssetUrlToken;
  #[throws(BadAssetUrlToken)]
  fn from_str(s: &str) -> Self {
    let mut buf: AssetUrlTokenRaw = default();
    let l = base64::decode_config_slice(
      s.as_bytes(), base64::URL_SAFE_NO_PAD, &mut buf)
      .map_err(|_| BadAssetUrlToken)?;
    if l != buf.len() { throw!(BadAssetUrlToken) }
    AssetUrlToken(buf)
  }
}
hformat_as_display!{AssetUrlToken}

#[derive(Error,Debug,Copy,Clone,Serialize)]
pub struct BadAssetUrlToken;
display_as_debug!{BadAssetUrlToken}

#[derive(Copy,Clone,Debug,Hash,PartialEq,Eq,Ord,PartialOrd)]
#[derive(Serialize,Deserialize)]
pub struct Id { pub index: Index, pub kind: Kind, }

impl Authorisation<InstanceName> {
  pub fn bundles(self) -> Authorisation<Id> { self.therefore_ok() }
}

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

impl Display for State {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    match self {
      State::Loaded(Loaded{ meta }) => {
        let BundleMeta { title } = meta;
        write!(f, "Loaded {:?}", title)?;
      }
      other => write!(f, "{:?}", other)?,
    }
  }
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

#[derive(Error,Debug)]
pub enum LoadError {
  BadBundle(BadBundle),
  IE(#[from] IE),
}
display_as_debug!{LoadError}
use LoadError as LE;

impl From<ZipError> for LoadError {
  fn from(ze: ZipError) -> LoadError {
    LE::BadBundle(format!("bad zipfile: {}", ze))
  }
}

#[ext(pub)]
impl<R> ZipArchive<R> where R: Read + io::Seek {
  #[throws(LoadError)]
  fn by_name_caseless<'a>(&'a mut self, name: &str) -> ZipFile<'a>
  {
    fn search<'a,R>(za: &'a mut ZipArchive<R>, name: &str)
                    -> Result<usize, LoadError>
    where R: Read + io::Seek
    {
      for i in 0..za.len() {
        let m = za.by_index(i);
        if matches!(m, Err(ZipError::FileNotFound)) { continue }
        let m = m?;
        if m.name_raw().eq_ignore_ascii_case(name.as_bytes()) { return Ok(i) }
      }
      return Err(LE::BadBundle(format!("bundle missing {}", name)));
    }
    let i = search(self, name)?;
    self.by_index(i)?
  }
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

  fn path_(&self, instance: &InstanceName) -> String {
    b_file(instance, self.index, self.kind)
  }

  pub fn path(&self, instance: &Unauthorised<InstanceGuard<'_>, InstanceName>,
          auth: Authorisation<Id>) -> String {
    self.path_(&instance.by_ref(auth.therefore_ok()).name)
  }

  #[throws(IE)]
  pub fn open_by_name(&self, instance_name: &InstanceName,
                      _: Authorisation<Id>) -> Option<fs::File> {
    let path = self.path_(instance_name);
    match File::open(&path) {
      Ok(f) => Some(f),
      Err(e) if e.kind() == ErrorKind::NotFound => None,
      Err(e) => void::unreachable(
        Err::<Void,_>(e).context(path).context("open bundle")?
      ),
    }
  }

  #[throws(IE)]
  pub fn open(&self, instance: &Instance) -> Option<fs::File> {
    let name = &*instance.name;
    let auth = Authorisation::authorised(name).bundles();
    self.open_by_name(name, auth)?
  }

  pub fn token(&self, instance: &Instance) -> AssetUrlToken {
    instance.asset_url_key.token("bundle", &(&*instance.name, *self))
  }
}

#[derive(Debug,Copy,Clone,Error)]
#[error("{0}")]
#[repr(transparent)]
pub struct NotBundle(&'static str);
impl From<&'static str> for NotBundle {
  fn from(s: &'static str) -> NotBundle {
    unsafe { mem::transmute(s) }
  }
}

#[derive(Error,Debug)]
enum IncorporateError {
  #[error("NotBundle({0})")] NotBundle(#[from] NotBundle),
  #[error("{0}")] IE(#[from] IE),
}

pub struct Uploading {
  instance: Arc<InstanceName>,
  id: Id,
  file: DigestWrite<BufWriter<fs::File>>,
}

#[throws(IE)]
fn load_bundle(ib: &mut InstanceBundles, ig: &mut Instance,
               id: Id, path: &str) {
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
  fn inner(_ig: &mut Instance, id: Id, path: &str) -> Loaded {
    match id.kind { Kind::Zip => () }
    let za = File::open(path)
      .with_context(|| path.to_owned()).context("open zipfile")
      .map_err(IE::from)?;
    let mut za = ZipArchive::new(za)?;

    let meta = {
      let mut mf = za.by_name_caseless("otter.toml")?;
      let mut meta = String::new();
      mf.read_to_string(&mut meta).map_err(
        |e| LE::BadBundle(format!("access toml zip member: {}", e)))?;
      let meta = meta.parse().map_err(
        |e| LE::BadBundle(format!("parse zip member as toml: {}", e)))?;
      let meta = toml_de::from_value(&meta).map_err(
        |e| LE::BadBundle(format!("interpret zip member metadata: {}", e)))?;
      meta
    };

    Loaded { meta }
    // todo: find zipfile, read metdata toml
    // todo:: show in UI for download
    // todo: do actual things, eg libraries and specs
  }

  let state = match inner(ig,id,path) {
    Ok(loaded)                     => State::Loaded(loaded),
    Err(LoadError::BadBundle(bad)) => State::BadBundle(bad),
    Err(LoadError::IE(ie))         => throw!(ie),
  };

  *slot = Some(Note { kind: id.kind, state });
}

impl FromStr for Id {
  type Err = NotBundle;
  #[throws(NotBundle)]
  fn from_str(fleaf: &str) -> Id {
    let [lhs, rhs] = fleaf.splitn(2, '.')
      .collect::<ArrayVec<[&str;2]>>()
      .into_inner().map_err(|_| "no dot")?;
    let index = lhs.parse().map_err(|_| "bad index")?;
    let kind = rhs.parse().map_err(|_| "bad extension")?;
    Id { index, kind }
  }
}

#[throws(IncorporateError)]
fn incorporate_bundle(ib: &mut InstanceBundles, ig: &mut Instance,
                      fpath: &str) {
  let fleaf = fpath.rsplitn(2, '/').next().unwrap();
  let id = fleaf.parse()?;
  load_bundle(ib, ig, id, fpath)?;
}

impl InstanceBundles {
  pub fn new() -> Self { InstanceBundles{ bundles: default() } }

  fn iter(&self) -> impl Iterator<Item=(Id, &State)> {
    self.bundles.iter().enumerate().filter_map(|(index, slot)| {
      let Note { kind, ref state } = *slot.as_ref()?;
      let index = index.try_into().unwrap();
      Some((Id { index, kind }, state))
    })
  }

  fn updated(&self, ig: &mut Instance) {
    ig.bundle_list = self.iter().map(|(id, state)| {
      (id, state.clone())
    }).collect();

    let new_info_pane = ig.bundle_list.info_pane(ig).unwrap_or_else(|e|{
      let m = "error rendering bundle list";
      error!("{}: {}", m, e);
      Html::from_txt(m)
    });
    let new_info_pane = Arc::new(new_info_pane);
    let mut prepub = PrepareUpdatesBuffer::new(ig, Some(1));
    prepub.raw_updates(vec![
      PUE::UpdateBundles { new_info_pane }
    ]);
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
    ib.updated(ig);
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
    self.updated(ig);
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

#[ext(pub)]
impl MgmtBundleList {
  #[throws(IE)]
  fn info_pane(&self, ig: &Instance) -> Html {
    #[derive(Serialize,Debug)]
    struct RenderPane {
      bundles: Vec<RenderBundle>,
    }
    #[derive(Serialize,Debug)]
    struct RenderBundle {
      id: Html,
      url: Html,
      title: Html,
    }
    let bundles = self.iter().filter_map(|(&id, state)| {
      if_let!{ State::Loaded(Loaded { meta }) = state; else return None; }
      let BundleMeta { title } = meta;
      let title = Html::from_txt(title);
      let token = id.token(ig);
      let url = hformat!("/_/bundle/{}/{}?{}", &*ig.name, &id, &token);
      let id = hformat!("{}", id);
      Some(RenderBundle { id, url, title })
    }).collect();

    Html::from_html_string(
      nwtemplates::render("bundles-info-pane.tera", &RenderPane { bundles })?
    )
  }
}

impl InstanceBundles {
  #[throws(MgmtError)]
  pub fn finish_upload(&mut self, ig: &mut Instance,
                       Uploading { instance:_, id, file }: Uploading,
                       expected: &Hash) {
    let (hash, mut file) = file.finish();
    let tmp = id.path_tmp(&ig.name);
    let install = id.path_(&ig.name);
    file.flush()
      .with_context(|| tmp.clone()).context("flush").map_err(IE::from)?;
    if hash.as_slice() != &expected.0[..] { throw!(ME::UploadCorrupted) }
    load_bundle(self, ig, id, &tmp)?;
    self.updated(ig);
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

#[test]
fn id_file_parse() {
  let check_y = |s,index,kind| {
    let id = Id { index, kind };
    assert_eq!(Id::from_str(s).unwrap(), id);
    assert_eq!(id.to_string(), s);
  };
  let check_n = |s,m| {
    assert_eq!(Id::from_str(s).unwrap_err().to_string(), m)
  };
  check_y("00000.zip", Index(0), Kind::Zip);
  check_n("00000zip",  "no dot");
  check_n("womba.zip", "bad index");
  check_n("00000.xyz", "bad extension");
  check_n("65536.zip", "bad index");
}
