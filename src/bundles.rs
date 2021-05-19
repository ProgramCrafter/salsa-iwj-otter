// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

//---------- public types ----------

pub use crate::prelude::Sha512Trunc256 as Digester;
pub type DigestWrite<W> = crate::utils::DigestWrite<Digester, W>;

#[derive(Copy,Clone,Hash,Eq,PartialEq,Serialize,Deserialize)]
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
  #[error("bad bundle: {0}")]     BadBundle(BadBundle),
  #[error("internal error: {0}")] IE(#[from] IE),
}

// Bundle states:
//
//              GameState   Instance      Note       main file    .d
//              pieces &c   libs, specs
//
//  ABSENT        unused     absent       None        absent      absent
//  NEARLY-ABSENT unused     absent       Uploading   absent      absent
//  WRECKAGE      unused     absent       Uploading   maybe .tmp  wreckage
//  BROKEN        unused     absent       Loaded      .zip        populated
//  UNUSED        unused     available    Loaded      .zip        populated
//  USED          used       available    Loaded      .zip        populated

//---------- private definitions ----------

pub type ZipArchive = zipfile::read::ZipArchive<BufReader<File>>;

define_index_type!{ pub struct LibInBundleI = usize; }

#[derive(Debug)]
struct Parsed {
  meta: BundleMeta,
  libs: IndexVec<LibInBundleI, shapelib::Contents>,
}

#[derive(Debug)]
struct ForProcess {
  za: IndexedZip,
  newlibs: IndexVec<LibInBundleI, ForProcessLib>,
}

#[derive(Debug)]
struct ForProcessLib {
  dir_inzip: String,
  svg_dir: String,
  need_svgs: Vec<GoodItemName>,
}

const BUNDLES_MAX: Index = Index(64);

#[derive(Debug,Clone,Serialize,Deserialize)]
struct Note {
  pub kind: Kind,
  pub state: State,
}

pub type BadBundle = String;

use LoadError as LE;

#[derive(Debug,Copy,Clone)]
enum BundleSavefile {
  Bundle(Id),
  PreviousUploadFailed(Index),
}

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

impl LoadError {
  fn badlib(libname: &str, e: &dyn Display) -> LoadError {
    LE::BadBundle(format!("bad library: {}: {}", libname, e))
  }
}

impl BundleSavefile {
  pub fn index(&self) -> Index {
    use BundleSavefile::*;
    match self {
      Bundle(id) => id.index,
      &PreviousUploadFailed(index) => index,
    }
  }
}

impl Debug for Hash {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    for v in self.0 { write!(f, "{:02x}", v)?; }
  }
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

impl FromStr for BundleSavefile {
  type Err = NotBundle;
  #[throws(NotBundle)]
  fn from_str(fleaf: &str) -> BundleSavefile {
    let [lhs, rhs] = fleaf.splitn(2, '.')
      .collect::<ArrayVec<&str,2>>()
      .into_inner().map_err(|_| "no dot")?;
    let index = lhs.parse().map_err(|_| "bad index")?;
    if rhs == "tmp" { return BundleSavefile::PreviousUploadFailed(index) }
    let kind = rhs.parse().map_err(|_| "bad extension")?;
    BundleSavefile::Bundle(Id { index, kind })
  }
}
impl FromStr for Id {
  type Err = NotBundle;
  #[throws(NotBundle)]
  fn from_str(fleaf: &str) -> Id {
    match fleaf.parse()? {
      BundleSavefile::Bundle(id) => id,
      BundleSavefile::PreviousUploadFailed(_) => throw!(NotBundle("tmp")),
    }
  }
}

impl Id {
  fn path_tmp(&self, instance: &InstanceName) -> String {
    b_file(instance, self.index, "tmp")
  }

  fn path_(&self, instance: &InstanceName) -> String {
    b_file(instance, self.index, self.kind)
  }

  fn path_dir(&self, instance: &InstanceName) -> String {
    b_file(instance, self.index, "d")
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

trait ReadSeek: Read + io::Seek { }
impl<T> ReadSeek for T where T: Read + io::Seek { }

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

#[derive(Debug)]
pub struct IndexedZip {
  za: ZipArchive,
  members: BTreeMap<UniCase<String>, usize>,
}
deref_to_field_mut!{IndexedZip, ZipArchive, za }

#[derive(Debug,Copy,Clone,Hash,Eq,PartialEq,Ord,PartialOrd)]
pub struct ZipIndex(pub usize);
impl Display for ZipIndex {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) { Display::fmt(&self.0,f)? }
}

impl IndexedZip where {
  #[throws(LoadError)]
  pub fn new(file: File) -> Self {
    let file = BufReader::new(file);
    let mut za = ZipArchive::new(file)?;
    let mut members = BTreeMap::new();
    for i in 0..za.len() {
      let entry = za.by_index_raw(i)?;
      let sname = entry.name().to_owned();
      let uname = UniCase::new(sname.to_owned());
      if let Some(previously) = members.insert(uname, i) {
        let sname = sname.to_owned();
        drop(entry);
        let previously = za.by_index_raw(previously)?;
        let previously = previously.name();
        throw!(LE::BadBundle(format!(
          "duplicate files, differing only in case, {:?} vs {:?}",
          &previously, sname,
        )));
      }
    }
    IndexedZip { za, members }
  }

  #[throws(LoadError)]
  pub fn by_name_caseless<'a, S>(&'a mut self, name: S) -> Option<ZipFile<'a>>
  where S: Into<String>
  {
    if_let!{ Some(&i) = self.members.get(&UniCase::new(name.into()));
             else return Ok(None) }
    Some(self.za.by_index(i)?)
  }
}

#[ext(pub)]
impl ZipArchive {
  #[throws(LoadError)]
  fn i<'z>(&'z mut self, i: ZipIndex) -> ZipFile<'z> {
    self.by_index(i.0)?
  }
}

impl<'z> IntoIterator for &'z IndexedZip {
  type Item = (&'z UniCase<String>, ZipIndex);
  type IntoIter = impl Iterator<Item=Self::Item>;
  fn into_iter(self) -> Self::IntoIter {
    self.members.iter().map(|(name,&index)| (name, ZipIndex(index)))
  }
}

trait BundleParseErrorHandling: Copy {
  type Err;
  fn required<XE,T,F>(self, f:F) -> Result<T, Self::Err>
  where XE: Into<LoadError>,
        F: FnOnce() -> Result<T,XE>;

  fn besteffort<XE,T,F,G>(self, f:F, g:G) -> Result<T, Self::Err>
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

#[derive(Debug,Copy,Clone)]
struct BundleParseReload<'s>{ pub bpath: &'s str }
impl BundleParseErrorHandling for BundleParseReload<'_> {
  type Err = ReloadError;
  fn required<XE,T,F>(self, f:F) -> Result<T,ReloadError>
  where XE: Into<LoadError>,
        F: FnOnce() -> Result<T,XE>
  {
    use ReloadError as RLE;
    f().map_err(|xe| {
      let le: LE = xe.into();
      match le {
        LE::BadBundle(why) => RLE::Unloadable(
          format!("{}: {}", self.bpath, &why)
        ),
        LE::IE(IE::Anyhow(ae)) => RLE::IE(IE::Anyhow(
          ae.context(self.bpath.to_owned())
        )),
        LE::IE(ie) => RLE::IE(ie),
      }
    })
  }

  fn besteffort<XE,T,F,G>(self, f:F, g:G) -> Result<T,ReloadError>
  where XE: Into<LoadError>,
        F: FnOnce() -> Result<T,XE>,
        G: FnOnce() -> T,
  {
    Ok(f().unwrap_or_else(|e| {
      match e.into() {
        LE::IE(ie) => {
          error!("reloading, error, partially skipping {}: {}",
                 self.bpath, ie);
        },
        LE::BadBundle(why) => {
          warn!("reloading, partially skipping {}: {}",
                self.bpath, why);
        },
      }
      g()
    }))
  }
}

#[derive(Debug,Copy,Clone)]
struct BundleParseUpload;
impl BundleParseErrorHandling for BundleParseUpload {
  type Err = LoadError;
  fn required<XE,T,F>(self, f:F) -> Result<T,LoadError>
  where XE: Into<LoadError>,
        F: FnOnce() -> Result<T,XE>
  {
    f().map_err(Into::into)
  }

  fn besteffort<XE,T,F,G>(self, f:F, _:G) -> Result<T,LoadError>
  where XE: Into<LoadError>,
        F: FnOnce() -> Result<T,XE>,
        G: FnOnce() -> T,
  {
    f().map_err(Into::into)
  }
}

#[derive(Copy,Clone,Debug,EnumCount,EnumMessage,ToPrimitive)]
enum Phase {
  #[strum(message="transfer upload data")]   Upload,
  #[strum(message="scan")]                   Scan,
  #[strum(message="parse shape catalogues")] ParseLibs,
  #[strum(message="start processing")]       Reaquire,
  #[strum(message="process piece images")]   Pieces,
}
impl progress::Enum for Phase { }

#[derive(Copy,Clone,Debug,EnumCount,EnumMessage,ToPrimitive)]
enum ReaquireProgress {
  #[strum(message="reaquire game lock")]     Reaquire,
  #[strum(message="prepare")]                Prepare,
}
impl progress::Enum for ReaquireProgress { }

#[throws(EH::Err)]
fn parse_bundle<EH>(id: Id, instance: &InstanceName, file: File, eh: EH,
                    mut for_progress: &mut dyn progress::Originator)
                    -> (ForProcess, Parsed)
  where EH: BundleParseErrorHandling,
{
  match id.kind { Kind::Zip => () }
  let mut za = eh.required(||{
    IndexedZip::new(file)
  })?;

  #[derive(Copy,Clone,Debug,EnumCount,EnumMessage,ToPrimitive)]
  enum ToScan {
    #[strum(message="metadata")]   Meta,
    #[strum(message="shape libs")] Libs,
  }
  impl progress::Enum for ToScan { }
  for_progress.phase_item(Phase::Scan, ToScan::Meta);
  
  let meta = eh.besteffort(||{
    const META: &str = "otter.toml";
    let mut mf = za.by_name_caseless(META)?
      .ok_or_else(|| LE::BadBundle(format!("bundle missing {}", META)))?;
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

  for_progress.phase_item(Phase::Scan, ToScan::Libs);

  #[derive(Debug)]
  struct LibScanned {
    libname: String,
    dir_inzip: String,
    inzip: ZipIndex,
  }

  let mut libs = Vec::new();
  for (name,i) in &za {
    eh.besteffort(|| Ok::<_,LE>(if_chain!{
      let mut split = name.as_ref().split('/');
      if let Some(dir)  = split.next();
      if let Some(file) = split.next();
      if let None       = split.next();
      if unicase::eq(dir, "library");
      if let Some((base, ext)) = file.rsplit_once('.');
      if unicase::eq(ext, "toml");
      then {
        libs.push(LibScanned {
          dir_inzip: format!("{}/{}", &dir, &base),
          libname: base.to_lowercase(),
          inzip: i,
        });
      }
    }), ||())?;
  }

  for_progress.phase(Phase::ParseLibs, libs.len());

  let mut newlibs = Vec::new();

  #[derive(Debug,Clone)]
  struct LibraryInBundle<'l> {
    catalogue_data: String,
    svg_dir: &'l String,
    need_svgs: Vec<GoodItemName>,
    id: &'l Id,
  }

  impl shapelib::LibrarySource for LibraryInBundle<'_> {
    fn catalogue_data(&self) -> &str { &self.catalogue_data }
    fn svg_dir(&self) -> String { self.svg_dir.clone() }
    fn note_svg(&mut self, basename: &GoodItemName) {
      self.need_svgs.push(basename.clone())
    }
    fn bundle(&self) -> Option<bundles::Id> { Some(*self.id) }
  }

  for (progress_count, LibScanned { libname, dir_inzip, inzip })
    in libs.into_iter().enumerate()
  {
    for_progress.item(progress_count, &libname);

    eh.besteffort(|| Ok::<_,LE>({
      let svg_dir = format!("{}/lib{:06}", id.path_dir(&instance), &inzip);

      let mut zf = za.i(inzip)?;
      let mut catalogue_data = String::new();
      zf.read_to_string(&mut catalogue_data)
        .map_err(|e| LE::badlib(&libname, &e))?;
      let mut src = LibraryInBundle {
        catalogue_data: catalogue_data,
        svg_dir: &svg_dir,
        need_svgs: Vec::new(),
        id: &id,
      };
      let contents = shapelib::load_catalogue(&libname, &mut src)
        .map_err(|e| LE::badlib(&libname, &e))?;
      newlibs.push((
        contents,
        ForProcessLib {
          need_svgs: src.need_svgs,
          svg_dir, dir_inzip,
        }
      ));
    }), ||())?;
  }

  for_progress.phase_item(Phase::Reaquire, ReaquireProgress::Reaquire);

  let (libs, newlibs) = newlibs.into_iter().unzip();

  (ForProcess { za, newlibs },
   Parsed { meta, libs })
}

#[throws(LE)]
fn process_bundle(ForProcess { mut za, mut newlibs }: ForProcess,
                  id: Id, instance: &InstanceName,
                  mut for_progress: &mut dyn progress::Originator)
{
  for_progress.phase_item(Phase::Reaquire, ReaquireProgress::Prepare);

  let dir = id.path_dir(instance);
  fs::create_dir(&dir)
    .with_context(|| dir.clone()).context("mkdir").map_err(IE::from)?;
  
  let svg_count = newlibs.iter().map(|pl| pl.need_svgs.len()).sum();
  for_progress.phase(Phase::Pieces, svg_count);
  
  let mut svg_count = 0;
  for ForProcessLib { need_svgs, svg_dir, dir_inzip, .. } in &mut newlibs {

    fs::create_dir(&svg_dir)
      .with_context(|| svg_dir.clone()).context("mkdir").map_err(IE::from)?;
      
    for item in mem::take(need_svgs) {
      make_usvg(&mut za, &mut svg_count, for_progress,
                dir_inzip, svg_dir, &item)?;
    }
  }
}

//---------- piece image processing ----------

#[derive(Copy,Clone,Debug,Display,EnumIter)]
// In preference order
enum PictureFormat {
  Svg,
//  Png,   xxx implement this
}

#[throws(LE)]
fn make_usvg(za: &mut IndexedZip, progress_count: &mut usize,
             mut for_progress: &mut dyn progress::Originator,
             dir_inzip: &str, svg_dir: &str,
             item: &GoodItemName) {
  let (format, mut zf) = 'format: loop {
    for format in PictureFormat::iter() {
      let input_basename = format!("{}/{}.{}", dir_inzip, item, format);
      if let Some(zf) = za.by_name_caseless(input_basename)? {
        break 'format (format, zf);
      }
    }
    throw!(LE::BadBundle(format!(
      "missing image file, looked for one of {}/{}.{}", dir_inzip, item,
      PictureFormat::iter().join(" ."),
    )));
  };

  for_progress.item(*progress_count, zf.name());
  *progress_count += 1;

  let mut input = tempfile::tempfile_in(&svg_dir)
    .context("create").map_err(IE::from)?;
  io::copy(&mut zf, &mut input)
    .context("copy from zip").with_context(|| zf.name().to_owned())
    .map_err(|e| LE::BadBundle(e.to_string()))?;
  input.rewind()
    .context("rewind").map_err(IE::from)?;

  let usvg_path = format!("{}/{}.usvg", svg_dir, item);
  let output = File::create(&usvg_path)
    .with_context(|| usvg_path.clone()).context("create").map_err(IE::from)?;

  match format {
    PictureFormat::Svg => {
      let got = Command::new(&config().usvg_bin).args(&["-c","-"])
        .stdin(input).stdout(output)
        .output().context("run usvg").map_err(IE::from)?;
      if ! got.status.success() {
        throw!(LE::BadBundle(format!(
          "{}: usvg conversion failed: {}: {}",
          zf.name(), got.status, String::from_utf8_lossy(&got.stderr)
        )));
      }
    },
  }
}

//---------- specs ----------

#[throws(MgmtError)]
pub fn load_spec_to_read(ig: &Instance, spec_name: &str)
  -> (Box<dyn Read>, String)
{
  let spec_leaf = format!("{}.game.toml", spec_name);

  // todo: game specs from bundles

  if spec_name.chars().all(
    |c| c.is_ascii_alphanumeric() || c=='-' || c =='_'
  ) {
    let path = format!("{}/{}", config().specs_dir, &spec_leaf);
    debug!("{}: trying to loading builtin spec from {}",
           &ig.name, &path);
    match File::open(&path) {
      Ok(f) => return (Box::new(f) as _, path),
      Err(e) if e.kind() == ErrorKind::NotFound => { },
      Err(e) => throw!(IE::from(
        AE::from(e).context(path).context("try open game spec")
      )),
    }
  }

  Err(ME::GameSpecNotFound)?
}

//---------- scanning/incorporating/uploading ----------

#[throws(InternalError)]
fn incorporate_bundle(ib: &mut InstanceBundles, ig: &mut Instance,
                 id: Id, parsed: Parsed) {
  let Parsed { meta, libs } = parsed;

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

  for lib in libs {
    ig.local_libs.add(lib);
  }

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
  fn scan_game_bundles(instance: &InstanceName)
                       -> impl Iterator<Item=Result<
      (String, Result<BundleSavefile, NotBundle>),
      IE
      >>
  {
    let bd = b_dir(&instance);
    let mo = glob::MatchOptions {
      require_literal_leading_dot: true,
      ..default()
    };
    
    glob::glob_with(&format!("{}/*", bd), mo)
      .context("pattern for bundle glob")?
      .map(|fpath|{
        let fpath = fpath.context("bundle glob")?;
        let fpath = fpath
          .to_str().ok_or_else(|| anyhow!("glob unicode conversion"))?;
        let fleaf = fpath.rsplitn(2, '/').next().unwrap();
        let parsed: Result<BundleSavefile, NotBundle> = fleaf.parse();
        Ok::<_,IE>((fpath.to_owned(), parsed))
      })
  }

  #[throws(IE)]
  pub fn reload_game_bundles(ig: &mut Instance) -> Self {
    let mut ib = InstanceBundles::new();

    for entry in InstanceBundles::scan_game_bundles(&ig.name)? {
      let (fpath, parsed) = entry?;
      let parsed: BundleSavefile = match parsed {
        Ok(y) => y,
        Err(NotBundle(why)) => {
          debug!("bundle file {:?} skippping {}", &fpath, why);
          continue;
        },
      };

      let iu: usize = parsed.index().into();
      if ib.bundles.get(iu).is_none() {
        ib.bundles.resize_with(iu+1, default);
      }

      if_let!{ BundleSavefile::Bundle(id) = parsed;
               else continue; }

      let file = File::open(&fpath)
        .with_context(|| fpath.clone()).context("open zipfile")
        .map_err(IE::from)?;

      let eh = BundleParseReload { bpath: &fpath };
      let (_za, parsed) = match parse_bundle(id, &ig.name, file, eh, &mut ()) {
        Ok(y) => y,
        Err(e) => {
          debug!("bundle file {:?} reload failed {}", &fpath, e);
          continue;
        }
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
    // todo: if bundle hash is here already, simply promote it
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
  pub fn bulk<R,PW>(self, data: R, size: usize, expected: &Hash,
                    progress_mode: ProgressUpdateMode,
                    progress_stream: &mut ResponseWriter<PW>) -> Uploaded
  where R: Read, PW: Write
  {
    let mut for_progress_buf;
    let mut null_progress = ();
    let for_progress: &mut dyn progress::Originator =
      if progress_mode >= PUM::Simplex {
        for_progress_buf = progress::ResponseOriginator::new(progress_stream);
        &mut for_progress_buf
      } else {
        &mut null_progress
      };

    let Uploading { id, mut file, instance } = self;
    let tmp = id.path_tmp(&instance);

    let mut null_progress = ();
    let for_progress_upload: &mut dyn progress::Originator =
      if progress_mode >= PUM::Duplex
      { for_progress } else { &mut null_progress };
    
    let mut data_reporter = progress::ReadOriginator::new(
      for_progress_upload, Phase::Upload, size, data);

    let copied_size = match io::copy(&mut data_reporter, &mut file) {
      Err(e) if e.kind() == ErrorKind::TimedOut => throw!(ME::UploadTimeout),
      x => x,
    }
      .with_context(|| tmp.clone())
      .context("copy").map_err(IE::from)?;

    let (hash, file) = file.finish();

    if copied_size != size as u64 { throw!(ME::UploadTruncated) }

    let mut file = file.into_inner().map_err(|e| e.into_error())
      .with_context(|| tmp.clone()).context("flush").map_err(IE::from)?;
    if hash.as_slice() != &expected.0[..] { throw!(ME::UploadCorrupted) }

    file.rewind().context("rewind"). map_err(IE::from)?;

    let (za, parsed) = parse_bundle(id, &instance, file, BundleParseUpload,
                                    for_progress)?;

    process_bundle(za, id, &*instance, for_progress)?;

    Uploaded { id, parsed }
  }
}

impl InstanceBundles {
  #[throws(MgmtError)]
  pub fn finish_upload(&mut self, ig: &mut Instance,
                       Uploaded { id, parsed }: Uploaded) -> Id {
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
    id
  }
}

//---------- clearing ----------

impl InstanceBundles {
  pub fn truncate_all_besteffort(instance: &InstanceName) {
    if_let!{
      Ok(bundles) = InstanceBundles::scan_game_bundles(instance);
      Err(e) => {
        error!("failed to scan game bundles for {}: {}", instance, e);
        return;
      }
    };
    for entry in bundles {
      if_let!{
        Ok((fpath,_what)) = entry;
        Err(e) => {
          error!("failed to make sense of a pathname for {}: {}", instance, e);
          continue;
        }
      };
      if_let!{
        Ok(_) = File::create(&fpath);
        Err(e) => {
          warn!("failed to truncate a bundle for {}: {}: {}",
                instance, fpath, e);
          continue;
        }
      }
    }
  }

  #[throws(MgmtError)]
  pub fn clear(&mut self, ig: &mut Instance) {

    // Check we are not in bundle state USED
    let checks: &[(_,_, Option<&dyn Debug>)] = &[
      ( "pieces",            ig.gs.pieces.is_empty(),  None  ),
      ( "piece aliases",     ig.pcaliases.is_empty(),  None  ),
      ( "pieces - occults",  ig.gs.occults.is_empty(), Some(&ig.gs.occults) ),
      ( "pieces - ipieces",  ig.ipieces.is_empty(),    Some(&ig.ipieces   ) ),
      ( "pieces - ioccults", ig.ioccults.is_empty(),   Some(&ig.ioccults  ) ),
    ];
    for (m, ok, pr) in checks {
      if ! ok {
        if let Some(pr) = pr {
          error!("{}: failed to clear because leftover {}: {:?}",
                 &ig.name, m, pr);
        }
        throw!(ME::BundlesInUse(m.to_string()))
      }
    }

    // If we are in UNUSED, become BROKEN
    ig.local_libs.clear();

    (||{
      // If we are in BROKEN, become WRECKAGE
      let mut to_clean
        : Vec<(&dyn Fn(&str) -> io::Result<()>, String)>
        = vec![];
      for (index, slot) in self.bundles.iter_mut().enumerate() {
        if_let!{ Some(note) = slot; else continue }
        let id = Id {
          index: index.try_into()
            .map_err(|_e| internal_error_bydebug(&(index, &note)))?,
          kind: note.kind,
        };
        let tmp = id.path_tmp(&ig.name);
        let install = id.path_(&ig.name);
        match fs::rename(&install, &tmp) {
          Err(e) if e.kind() == ErrorKind::NotFound => { }
          x => x.with_context(|| tmp.clone())
            .with_context(|| install.clone())
            .context("rename away")?
        }
        note.state = State::Uploading;
        // These will happen in order, turning each bundle from
        // WRECKAGE into NEARLY-ABSENT
        to_clean.push((&|p| fs::remove_dir_all(p), id.path_dir(&ig.name) ));
        to_clean.push((&|p| fs::remove_file   (p), tmp                   ));
      }

      InstanceBundles::truncate_all_besteffort(&ig.name);

      // Actually try to clean up WRECKAGE into NEARLY-ABSENT
      for (f,p) in to_clean {
        match f(&p) {
          Err(e) if e.kind() == ErrorKind::NotFound => { }
          x => x.with_context(|| p.clone()).context("clean up")?
        }
      }

      let new_asset_key = AssetUrlKey::new_random()?;

      // Right, everything is at most NEARLY-ASENT, make them ABSENT
      self.bundles.clear();

      // Prevent old, removed, players from accessing any new bundles.
      ig.asset_url_key = new_asset_key;

      Ok::<_,IE>(())
    })()?;
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
