// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

//---------- public types ----------

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

#[derive(Copy,Clone,Debug,Hash,PartialEq,Eq,Ord,PartialOrd)]
#[derive(Serialize,Deserialize)]
pub struct Id { pub index: Index, pub kind: Kind, }

#[derive(Debug,Clone)]
pub struct InstanceBundles {
  // todo: this vec is needed during loading only!
  bundles: Vec<Option<Note>>,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub enum State {
  Uploading,
  Loaded(Loaded),
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct Loaded {
  pub meta: BundleMeta,
}

/// returned by start_upload
pub struct Uploading {
  id: Id,
  instance: Arc<InstanceName>,
  file: DigestWrite<BufWriter<fs::File>>,
}

/// returned by start_upload
pub struct Uploaded {
  id: Id,
  parsed: Parsed,
}

#[derive(Debug,Copy,Clone,Error)]
#[error("{0}")]
#[repr(transparent)]
pub struct NotBundle(&'static str);

#[derive(Error,Debug)]
pub enum LoadError {
  BadBundle(BadBundle),
  IE(#[from] IE),
}
display_as_debug!{LoadError}

//---------- private definitions ----------

#[derive(Debug,Clone,Serialize,Deserialize)]
struct Parsed {
  pub meta: BundleMeta,
}

const BUNDLES_MAX: Index = Index(64);

#[derive(Debug,Clone,Serialize,Deserialize)]
struct Note {
  pub kind: Kind,
  pub state: State,
}

pub type BadBundle = String;

use LoadError as LE;

//---------- straightformward impls ----------

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

impl From<&'static str> for NotBundle {
  fn from(s: &'static str) -> NotBundle {
    unsafe { mem::transmute(s) }
  }
}

impl From<LoadError> for MgmtError {
  fn from(le: LoadError) -> MgmtError { match le {
    LE::BadBundle(why) => ME::BadBundle(why),
    LE::IE(ie) => ME::from(ie),
  } }
}

//---------- pathname handling (including Id leafname) ----------

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

//---------- displaing/presenting/authorising ----------

impl Authorisation<InstanceName> {
  pub fn bundles(self) -> Authorisation<Id> { self.therefore_ok() }
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

//---------- loading ----------

impl From<ZipError> for LoadError {
  fn from(ze: ZipError) -> LoadError {
    match ze {
      ZipError::Io(ioe) => IE::from(
        AE::from(ioe).context("zipfile io error")
      ).into(),
      _ => LE::BadBundle(format!("bad zipfile: {}", ze))
    }
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

pub trait ReadSeek: Read + io::Seek { }
impl<T> ReadSeek for T where T: Read + io::Seek { }

trait BundleParseError: Sized {
  fn required<XE,T,F>(bpath: &str, f:F) -> Result<T,Self>
  where XE: Into<LoadError>,
        F: FnOnce() -> Result<T,XE>;

  fn besteffort<XE,T,F,G>(bpath: &str, f:F, g:G) -> Result<T,Self>
  where XE: Into<LoadError>,
        F: FnOnce() -> Result<T,XE>,
        G: FnOnce() -> T;
}

#[derive(Debug,Error)]
enum ReloadError {
  IE(IE),
  Unloadable(BadBundle),
}
display_as_debug!{ReloadError}

impl BundleParseError for ReloadError {
  fn required<XE,T,F>(bpath: &str, f:F) -> Result<T,ReloadError>
  where XE: Into<LoadError>,
        F: FnOnce() -> Result<T,XE>
  {
    use ReloadError as RLE;
    f().map_err(|xe| {
      let le: LE = xe.into();
      match le {
        LE::BadBundle(why) => RLE::Unloadable(
          format!("{}: {}", bpath, &why)
        ),
        LE::IE(IE::Anyhow(ae)) => RLE::IE(IE::Anyhow(
          ae.context(bpath.to_owned())
        )),
        LE::IE(ie) => RLE::IE(ie),
      }
    })
  }

  fn besteffort<XE,T,F,G>(bpath: &str, f:F, g:G) -> Result<T,ReloadError>
  where XE: Into<LoadError>,
        F: FnOnce() -> Result<T,XE>,
        G: FnOnce() -> T,
  {
    Ok(f().unwrap_or_else(|e| {
      match e.into() {
        LE::IE(ie) => {
          error!("reloading, error, partially skipping {}: {}", bpath, ie);
        },
        LE::BadBundle(why) => {
          warn!("reloading, partially skipping {}: {}", bpath, why);
        },
      }
      g()
    }))
  }
}

impl BundleParseError for LoadError { 
  fn required<XE,T,F>(_bpath: &str, f:F) -> Result<T,LoadError>
  where XE: Into<LoadError>,
        F: FnOnce() -> Result<T,XE>
  {
    f().map_err(Into::into)
  }

  fn besteffort<XE,T,F,G>(_bpath: &str, f:F, _:G) -> Result<T,LoadError>
  where XE: Into<LoadError>,
        F: FnOnce() -> Result<T,XE>,
        G: FnOnce() -> T,
  {
    f().map_err(Into::into)
  }
}

#[throws(EH)]
fn parse_bundle<EH>(id: Id, file: &mut dyn ReadSeek, bpath: &str) -> Parsed
  where EH: BundleParseError
{
  let file = BufReader::new(file);
  match id.kind { Kind::Zip => () }
  let mut za = EH::required(bpath, ||{
    ZipArchive::new(file)
  })?;

  let meta = EH::besteffort(bpath, ||{
    let mut mf = za.by_name_caseless("otter.toml")?;
    let mut meta = String::new();
    mf.read_to_string(&mut meta).map_err(
      |e| LE::BadBundle(format!("access toml zip member: {}", e)))?;
    let meta = meta.parse().map_err(
      |e| LE::BadBundle(format!("parse zip member as toml: {}", e)))?;
    let meta = toml_de::from_value(&meta).map_err(
      |e| LE::BadBundle(format!("interpret zip member metadata: {}", e)))?;
    Ok::<_,LE>(meta)
  }, ||{
    BundleMeta {
      title: "[bundle metadata could not be reloaded]".to_owned()
    }
  })?;

  // todo: do actual things, eg libraries and specs

  Parsed { meta }
}

//---------- scanning/incorporating/uploading ----------

#[throws(InternalError)]
fn incorporate_bundle(ib: &mut InstanceBundles, _ig: &mut Instance,
                 id: Id, parsed: Parsed) {
  let Parsed { meta } = parsed;

  let iu: usize = id.index.into();
  let slot = &mut ib.bundles[iu];
  match slot {
    None => { },
    Some(Note { kind:_, state: State::Uploading }) => { },
    Some(ref note) => throw!(IE::DuplicateBundle {
      index: id.index,
      kinds: [note.kind, id.kind],
    })
  };
  
  let state = State::Loaded(Loaded { meta });
  *slot = Some(Note { kind: id.kind, state });
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
  pub fn reload_game_bundles(ig: &mut Instance) -> Self {
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

      let fleaf = fpath.rsplitn(2, '/').next().unwrap();
      let id: Id = match fleaf.parse() {
        Ok(y) => y,
        Err(NotBundle(why)) => {
          debug!("bundle file {:?} skippping {}", &fpath, why);
          // xxx delete?
          continue;
        },
      };

      let mut file = File::open(fpath)
        .with_context(|| fpath.to_owned()).context("open zipfile")
        .map_err(IE::from)?;

      let iu: usize = id.index.into();
      if ib.bundles.get(iu).is_none() {
        ib.bundles.resize_with(iu+1, default);
      }

      let parsed = match parse_bundle(id, &mut file, fpath) {
        Ok(y) => y,
        Err(LE::BadBundle(why)) => {
          debug!("bundle file {:?} bad {}", &fpath, why);
          continue;
        }
        Err(LE::IE(ie)) => throw!(ie),
      };

      incorporate_bundle(&mut ib, ig, id, parsed)?;
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
      let mkf = || {
        fs::OpenOptions::new()
          .read(true).write(true).create_new(true)
          .open(&tmp)
      };
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
  pub fn bulk<R>(self, data: &mut R, expected: &Hash) -> Uploaded
  where R: Read
  {
    let Uploading { id, mut file, instance } = self;
    let tmp = id.path_tmp(&instance);

    io::copy(data, &mut file)
      .with_context(|| tmp.clone())
      .context("copy").map_err(IE::from)?;

    let (hash, file) = file.finish();

    let mut file = file.into_inner().map_err(|e| e.into_error())
      .with_context(|| tmp.clone()).context("flush").map_err(IE::from)?;
    if hash.as_slice() != &expected.0[..] { throw!(ME::UploadCorrupted) }

    file.rewind().context("rewind"). map_err(IE::from)?;
    let mut file = BufReader::new(file);

    let parsed = parse_bundle::<LoadError>(id, &mut file, &tmp)?;

    Uploaded { id, parsed }
  }
}

impl InstanceBundles {
  #[throws(MgmtError)]
  pub fn finish_upload(&mut self, ig: &mut Instance,
                       Uploaded { id, parsed }: Uploaded) {
    let tmp = id.path_tmp(&ig.name);
    let install = id.path_(&ig.name);

    incorporate_bundle(self, ig, id, parsed)?;

    self.updated(ig);
    match self.bundles.get(usize::from(id.index)) {
      Some(Some(Note { state: State::Loaded(..), .. })) => {
        fs::rename(&tmp, &install)
          .with_context(||install.clone())
          .context("install").map_err(IE::from)?;
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
