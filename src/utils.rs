// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

pub use otter_base::crates::extend::ext;

//========== miscellany ==========
// (roughly in order of implementation length)

pub fn is_default<T: ConstDefault + Eq>(t: &T) -> bool { t == &T::DEFAULT }

#[ext(pub, name=SeekExt)]
impl<T: io::Seek> T {
  #[throws(io::Error)]
  fn rewind(&mut self) { self.seek(io::SeekFrom::Start(0))? }
}

#[ext(pub, name=OrdExt)]
impl<T: Ord + Sized + Clone> T {
  fn update_max(&mut self, new: &Self) {
    if *new > *self { *self = new.clone() }
  }
}

#[ext(pub, name=SplitAtDelim)]
impl str {
  fn split_at_delim(&self, delim: char) -> (&Self, &Self) {
    match self.find(delim) {
      Some(index) => self.split_at(index),
      None => (self, ""),
    }
  }
}

#[derive(Debug,Clone)]
pub struct JsonString<T:Serialize>(pub T);
impl<T> Serialize for JsonString<T> where T:Serialize {
  #[throws(S::Error)]
  fn serialize<S>(&self, s: S) -> S::Ok where S:Serializer {
    let json = serde_json::to_string(&self.0)
      .map_err(|e| <S::Error as serde::ser::Error>::custom(e))?;
    Serialize::serialize(&json, s)?
  }
}

#[throws(Either<io::Error, io::Error>)]
pub fn io_copy_interactive<R,W>(read: &mut BufReader<R>, write: &mut W)
where R: Read, W: Write {
  loop {
    let buf = read.fill_buf().map_err(Either::Left)?;
    if buf.len() == 0 { break }

    let did = (||{
      let did = write.write(buf)?;
      if did == 0 { throw!(ErrorKind::WriteZero) }
      Ok::<_,io::Error>(did)
    })().map_err(Either::Right)?;
        
    read.consume(did);
    write.flush().map_err(Either::Right)?;
  }
}

/// Allows the use of serde for a compat struct
///
/// Ideally we would have
/// ```rust ignore
/// #[derive(Deserialize)]
/// #[serde(try_from=Compat)]
/// struct Main { /* new definition */ }
///
/// #[derive(Deserialize)]
/// #[serde(untagged)]
/// enum Compat { V1(Main), V2(Old) }
///
/// #[derive(Deserialize)]
/// struct Old { /* old version we still want to read */ }
///
/// impl TryFrom<Compat> for Main { /* ... */ }
/// ```
///
/// But the impl for `Compat` ends up honouring the `try_from` on `Main`
/// so is recursive.  We solve that abusing serde's remote feature.
///
/// For an example, see `IOccultIlk`.
///
/// The name of the main structure must be passed twice, once as an
/// identifier and once as a literal, because `stringify!` doesn't work
/// in the serde attribute.
#[macro_export]
macro_rules! serde_with_compat { {
  [ $( #[ $($attrs:meta)* ] )* ] [ $vis:vis ] [ $($intro:tt)* ]
    $main:ident=$main_s:literal $new:ident $compat_s:literal
  [ $($body:tt)* ]
} => {
  $(#[ $($attrs)* ])* 
  #[serde(try_from=$compat_s)]
  $vis $($intro)* $main $($body)*

  #[allow(non_camel_case_types)]
  $(#[ $($attrs)* ])* 
  #[serde(remote=$main_s)]
  $($intro)* $new $($body)*
} }

//---------- Timespec (for serde) ----------

pub mod timespec_serde {
  use super::*;

  #[derive(Serialize, Deserialize)]
  struct Timespec(i64, u32);

  #[throws(S::Error)]
  pub fn serialize<S:Serializer>(v: &TimeSpec, s: S) -> S::Ok {
    let v = Timespec(v.tv_sec(), v.tv_nsec().try_into().unwrap());
    Serialize::serialize(&v, s)?
  }
  #[throws(D::Error)]
  pub fn deserialize<'de, D:Deserializer<'de>>(d: D) -> TimeSpec {
    let Timespec(sec, nsec) = Deserialize::deserialize(d)?;
    libc::timespec { tv_sec: sec, tv_nsec: nsec.into() }.into()
  }
}

//---------- get_or_extend_with ----------


#[ext(pub)]
impl<T> Vec<T> {
  fn get_or_extend_with<F>(&mut self, i: usize, f: F) -> &mut T
  where F: FnMut() -> T {
    if self.get(i).is_none() {
      self.resize_with(i+1, f);
    }
    &mut self[i]
  }
}

#[ext(pub)]
impl<I,T> IndexVec<I,T> where I: index_vec::Idx {
  fn get_or_extend_with<F>(&mut self, i: I, f: F) -> &mut T
  where F: FnMut() -> T {
    self.raw.get_or_extend_with(i.index(), f)
  }
}

//========== OldNew ==========

#[derive(Copy,Clone,Debug,From,Into)]
#[derive(Hash,Eq,PartialEq,Serialize,Deserialize)]
#[serde(transparent)]
pub struct OldNew<T>([T; 2]);

#[derive(Copy,Clone,Debug)]
#[derive(Hash,Eq,PartialEq)]
pub enum OldNewIndex { Old, New }

#[allow(clippy::new_ret_no_self)]
impl<T> OldNew<T> {
  pub fn old(&self) -> &T { &self.0[0] }
  pub fn new(&self) -> &T { &self.0[1] }

  pub fn map<U, F: FnMut(&T) -> U>(&self, f: F) -> OldNew<U> {
    OldNew(
      self.iter().map(f)
        .collect::<ArrayVec<U,2>>()
        .into_inner()
        .unwrap_or_else(|_|panic!())
    )
  }

  pub fn as_refs(&self) -> OldNew<&T> {
    OldNew(
      self.iter()
        .collect::<ArrayVec<&T,2>>()
        .into_inner()
        .unwrap_or_else(|_|panic!())
    )
  }

  pub fn iter(&self) -> impl Iterator<Item=&T> {
    self.0.iter()
  }

  #[allow(clippy::should_implement_trait)] // yes, but, TAIT
  pub fn into_iter(self) -> impl Iterator<Item=T> {
    IntoIterator::into_iter(self.0)
  }
}

impl<T> Index<OldNewIndex> for OldNew<T> {
  type Output = T;
  fn index(&self, i: OldNewIndex) -> &T { &self.0[i as usize] }
}

//========== obtaining size from xml ==========

#[derive(Error,Clone,Serialize,Deserialize,Debug)]
pub enum SVGSizeError {
  #[error("parse error: {0}")]          ParseError(String),
  #[error("attribute {0} repeated")]    AttributeRepeated(SVGWidthOrHeight),
  #[error("attribute {0} unparseable")] AttributeUnparseable(SVGWidthOrHeight),
  #[error("missing attribute {0}")]     MissingAttribute(SVGWidthOrHeight),
  #[error("first element is not <svg>")] WrongFirstElement,
  #[error("encountered EOF before SVG element")] UnexpectedEOF,
}

#[derive(Clone,Copy,Serialize,Deserialize,Debug,AsRefStr,Display,EnumIter)]
#[allow(non_camel_case_types)]
pub enum SVGWidthOrHeight {
  width,
  height,
}

#[throws(SVGSizeError)]
pub fn svg_parse_size(xml: &HtmlStr) -> PosC<f64> {
  let mut tokens = xmlparser::Tokenizer::from(xml.as_html_str())
    .map(|t| t.map_err(|e| SvSE::ParseError(e.to_string())))
    .chain(iter::repeat_with(|| Err(SvSE::UnexpectedEOF)));

  use xmlparser::Token as Tk;

  for token in &mut tokens {
    match token? {
      Tk::ElementStart{ local, .. } => {
        if local.as_str() == "svg" { break }
        else { throw!(SvSE::WrongFirstElement) }
      }
      _ => { },
    }
  }

  let mut wh = [None; 2];

  for token in &mut tokens {
    match token? {
      Tk::ElementEnd{..} => {
        break
      },
      Tk::Attribute { local, value, .. } => {
        let local = local.as_str();
        if_let!{
          Some(f) = SVGWidthOrHeight::iter().find(
            |n| local == n.as_ref()
          );
          else continue;
        }
        let i = f as usize;
        if wh[i].is_some() {
          throw!(SvSE::AttributeRepeated(f))
        }
        let v: f64 = value.parse().map_err(
          |_| SvSE::AttributeUnparseable(f)
        )?;
        wh[i] = Some(v);
      },
      _ => { },
    }
  }
  PosC::try_from_iter_2(
    izip!(SVGWidthOrHeight::iter(), wh).map(
      |(f,v)| v.ok_or_else(|| SvSE::MissingAttribute(f))
    )
  )?
}

//========== Thunk ==========

// todo #[derive(Clone)]
pub struct Thunk<U: Sync, F: Sync + FnOnce() -> U> (
  lazy_init::LazyTransform<F, U>
);

impl<T: Sync, F: Sync + FnOnce() -> T> Debug for Thunk<T, F> where T: Debug{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self.0.get() {
      Some(inner) => write!(f, "Thunk({:?})", inner),
      None        => write!(f, "Thunk(...)"),
    }
  }
}

impl<T: Sync, F: Sync + FnOnce() -> T> Thunk<T, F> {
  pub fn new(f: F) -> Self {
    Thunk(
      lazy_init::LazyTransform::new(f)
    )
  }
  pub fn force_eval(thunk: &Self) {
    thunk.0.get_or_create(|f| f());
  }
  pub fn into_inner(thunk: Self) -> T {
    Thunk::force_eval(&thunk);
    thunk.0.into_inner().unwrap_or_else(|_|panic!())
  }
}

impl<T: Sync, F: Sync + FnOnce() -> T> Deref for Thunk<T, F> {
  type Target = T;
  fn deref(&self) -> &T {
    Thunk::force_eval(self);
    self.0.get().unwrap()
  }
}

impl<Y: Sync, E: Sync, F: Sync + FnOnce() -> Result<Y,E>>
  From<Thunk<Result<Y,E>, F>> for Result<Y,E>
{
  fn from(thunk: Thunk<Result<Y,E>, F>) -> Result<Y,E> {
    Thunk::into_inner(thunk)
  }
}

// todo: DerefMut

//========== .insert() and .remove() on various Entry ==========

macro_rules! entry_define_insert_remove {
  { $name:ident, $name_mod:ident, $entry:path, $into_key:ident } =>
  {
    #[allow(non_snake_case)]
    mod $name_mod {
      use $crate::prelude::extend::ext;
      use $crate::prelude::slotmap;
      use $entry as Entry;
      use Entry::{Occupied, Vacant};
      #[ext(pub, name=EntryExt)]
      impl<'e,K,V> Entry<'e,K,V> where K: slotmap::Key {
        fn insert(self, v: V) {
          match self {
            Vacant(ve)   => { ve.insert(v); }
            Occupied(mut oe) => { oe.insert(v); }
          }
        }
        fn remove(self) -> (K, Option<V>) {
          match self {
            Vacant(ve)   => { let k = ve.$into_key();        (k, None)    }
            Occupied(oe) => { let (k,v) = oe.remove_entry(); (k, Some(v)) }
          }
        }
      }
    }
    pub use $name_mod::EntryExt as $name;
  }
}

entry_define_insert_remove!{
  SlotmapSparseSecondaryEntryExt,
  SlotmapSparseSecondaryEntryExt_mod,
  slotmap::sparse_secondary::Entry,
  key
}

//========== FutureInstant ==========

#[derive(Debug,Copy,Clone,Eq,PartialEq,Ord,PartialOrd)]
#[derive(From,Into)]
#[derive(Serialize, Deserialize)]
#[serde(into="Duration", try_from="Duration")]
pub struct FutureInstant(pub Instant);

impl Into<Duration> for FutureInstant {
  fn into(self) -> Duration {
    let now = config().global_clock.now();
    Instant::from(self).checked_duration_since(now).unwrap_or_default()
  }
}

#[derive(Error,Debug)]
#[error("Duration (eg during load) implies out-of-range FutureInstant")]
pub struct FutureInstantOutOfRange;

impl TryFrom<Duration> for FutureInstant {
  type Error = FutureInstantOutOfRange;
  #[throws(FutureInstantOutOfRange)]
  fn try_from(duration: Duration) -> FutureInstant {
    let now = config().global_clock.now();
    now.checked_add(duration).ok_or(FutureInstantOutOfRange)?.into()
  }
}      

//========== Error handling ==========

#[derive(Debug)]
pub struct AnyhowDisplay<'a>(pub &'a anyhow::Error);
impl Display for AnyhowDisplay<'_> {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    let mut delim = "";
    self.0.for_each(&mut |s|{
      write!(f, "{}{}", delim, s)?;
      delim = ": ";
      Ok(())
    })?;
  }
}

#[ext(pub)]
impl anyhow::Error {
  fn for_each(&self, f: &mut dyn FnMut(&str) -> fmt::Result) -> fmt::Result {
    let mut done = String::new();
    for e in self.chain() {
      let s = e.to_string();
      if done.contains(&s) { continue }
      f(&s)?;
      done = s;
    }
    Ok(())
  }

  fn d(&self) -> AnyhowDisplay<'_> { AnyhowDisplay(self) }

  fn end_process(self, estatus: u8) -> ! {
    #[derive(Default,Debug)] struct Sol { any: bool, progname: String }
    impl Sol {
      fn nl(&mut self) {
        if self.any { eprintln!("") };
        self.any = false;
      }
      fn head(&mut self) {
        if ! self.any { eprint!("{}: error", &self.progname); }
        self.any = true
      }
    }
    let mut sol: Sol = Sol { any: false, progname: program_name() };
    self.for_each(&mut |s|{
      let long = s.len() > 80;
      if long && sol.any { sol.nl() }
      sol.head();
      eprint!(": {}", &s);
      if long { sol.nl() }
      Ok::<_,fmt::Error>(())
    }).unwrap();
    sol.nl();
    assert!(estatus > 0);
    exit(estatus.into());
  }
}

//========== IO - File::close ==========

// https://github.com/rust-lang/rust/issues/32255 :-(

#[ext(pub, name=LocalFileExt, supertraits=Sized)]
impl fs::File {
  #[throws(io::Error)]
  fn close(self) {
    let r = unsafe {
      let fd = self.into_raw_fd();
      libc::close(fd)
    };
    if r == 0 {
      ()
    } else if r == -1 {
      throw!(io::Error::last_os_error())
    } else {
      panic!("close(2) returned {}", r)
    }
  }
}


//========== IO - SigPipeWriter and RawStdout/CookedStdout ==========

pub struct SigPipeWriter<W>(pub W);

impl<W:Write> SigPipeWriter<W> {
  fn handle_err(e: io::Error) -> io::Error {
    if e.kind() != ErrorKind::BrokenPipe { return e }

    match (||{
      use nix::sys::signal::*;
      use Signal::SIGPIPE;
      unsafe {
        sigaction(SIGPIPE, &SigAction::new(
          SigHandler::SigDfl, SaFlags::empty(), SigSet::empty()))
          .context("sigaction")?;
      };
      raise(SIGPIPE).context("raise")?;
      Err::<Void,_>(anyhow!("continued after raise"))
    })() {
      Err(ae) => ae
        .context("attempt to die with SIGPIPE failed")
        .end_process(127),
      Ok(v) => match v { },
    }
  }
}

impl<W:Write> Write for SigPipeWriter<W> {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    self.0.write(buf).map_err(Self::handle_err)
  }
  fn flush(&mut self)             -> io::Result<()>    {
    self.0.flush()   .map_err(Self::handle_err)
  }
}

pub type RawStdout = SigPipeWriter<io::Stdout>;
#[allow(clippy::new_without_default)] // Don't want these made willy-nilly
impl RawStdout { pub fn new() -> Self { SigPipeWriter(io::stdout()) } }

pub struct CookedStdout(pub BufWriter<SigPipeWriter<io::Stdout>>);
#[allow(clippy::new_without_default)] // Don't want these made willy-nilly
impl CookedStdout {
  pub fn new() -> Self { Self(BufWriter::new(RawStdout::new())) }
  fn handle_err(e: io::Error) -> ! {
    AE::from(e).context("write stdout").end_process(EXIT_DISASTER as _);
  }
  fn must_flush(&mut self) {
    self.0.flush().unwrap_or_else(|e| Self::handle_err(e))
  }
}
impl Write for CookedStdout {
  #[throws(io::Error)]
  fn write(&mut self, buf: &[u8]) -> usize {
    let r = self.0.write(buf).unwrap_or_else(|e| Self::handle_err(e));
    if buf.contains(&b'\n') { self.flush()? }
    r
  }
  #[throws(io::Error)]
  fn flush(&mut self) { self.must_flush() }
}
impl Drop for CookedStdout {
  fn drop(&mut self) { self.must_flush() }
}

//========== hex ==========

#[throws(fmt::Error)]
pub fn fmt_hex(f: &mut Formatter, buf: &[u8]) {
  for v in buf { write!(f, "{:02x}", v)?; }
}

#[throws(as Option)]
#[must_use]
pub fn parse_slice_hex(s: &str, buf: &mut [u8]) -> usize {
  let l = s.len();
  if l % 2 != 0 { throw!() }
  let l = l/2;
  if l > buf.len() { throw!() }

  for (h, o) in izip!(
    s.as_bytes().chunks(2),
    buf.iter_mut(),
  ) {
    let h = str::from_utf8(h).ok()?;
    *o = u8::from_str_radix(h,16).ok()?;
  }

  l
}

#[throws(as Option)]
pub fn parse_fixed_hex<const N: usize>(s: &str) -> [u8; N] {
  let mut buf = [0u8; N];
  let l = parse_slice_hex(s, &mut buf)?;
  if l != N { throw!() }
  buf
}

#[macro_export]
macro_rules! format_by_fmt_hex {
  ($trait:ty, for $self:ty, . $($memb:tt)+) => {
    impl $trait for $self {
      #[throws(fmt::Error)]
      fn fmt(&self, f: &mut Formatter) {
        fmt_hex(f, &self . $($memb)+)?
      }
    }
  }
}

#[test]
fn test_parse_hex(){
  assert_eq!( parse_fixed_hex(""),     Some([          ]) );
  assert_eq!( parse_fixed_hex("41"  ), Some([b'A'      ]) );
  assert_eq!( parse_fixed_hex("4165"), Some([b'A', b'e']) );
  assert_eq!( parse_fixed_hex("4165"), Some([b'A', b'e']) );
  assert_eq!( parse_fixed_hex("41"  ), None::<[_;0]>      );
  assert_eq!( parse_fixed_hex("41"  ), None::<[_;2]>      );
  assert_eq!( parse_fixed_hex("1"   ), None::<[_;1]>      );
  assert_eq!( parse_fixed_hex("xy"  ), None::<[_;1]>      );
}

//========== want* macros ==========

#[macro_export]
macro_rules! want_failed_internal {
  { $variant:ident($binding:pat) = $input:expr, $x:expr, $($d:expr),* } => {
    InternalLogicError::new({
      #[allow(unused_mut)]
      let mut s = format!("wanted {}({}) = {}, but got {:?}",
	                  stringify!($variant), stringify!($binding),
                          stringify!($input), $x);
      $(
        write!(&mut s, " {}={:?}", stringify!($d), &$d).unwrap();
      )*
      s
    }).tolerate()
  }
}

#[macro_export]
macro_rules! want {
  { $variant:ident = $input:expr,
    ? $($d:expr),*
  } => (
    match $input {
      $variant(y) => Some(y),
      x => {
        want_failed_internal!{ $variant(_)=$input, x, $($d),* }
        None
      },
    }
  );
  { $variant:ident = $input:expr } => {
    want!( $variant = $input,
           ? )
  };
}

#[macro_export]
macro_rules! wants {
  { $($d:tt)* } => { want!(Some = $($d)*) }
}
#[macro_export]
macro_rules! wantok {
  { $($d:tt)* } => { want!(Ok = $($d)*) }
}

#[macro_export]
macro_rules! want_let {
  { $($variant:ident)::+($binding:pat) = $input:expr;
    else ? $($d:expr),*; $($otherwise:tt)*
  } => {
    let $binding = match $input {
      $($variant(y))::+ => y,
      x => {
        want_failed_internal!{
          $($variant)::+($binding)=$input, x, $($d),*
        }
        $($otherwise)*
      },
    };
  };
  { $($variant:ident)::+($binding:pat) = $input:expr;
    else $($otherwise:tt)*
  } => {
    want_let!{ $($variant($binding))::+ = $input; else ?; $($otherwise)* }
  };
}

//========== miscellaneous macros ==========

#[macro_export]
macro_rules! impl_via_ambassador{
  { 
    $(
      $( #[ $attr:meta ] )*
      impl $( [ $($generics:tt)* ] )? $Trait:ident for $Type:ty
      $( where [ $($where:tt)* ] )?
      { $($how_immut:tt)* }
    )*
  } => { $( paste!{
    $( #[ $attr ] )*
    impl $( < $($generics)* > )? $Trait for $Type
    $( where $($where)* )?
    {
      [< ambassador_impl_ $Trait >]!{ body_struct( <>, dyn $Trait,
          (),
          ($($how_immut)*),
          ()
      ) }
    }
  } )* }
}

#[macro_export]
macro_rules! ensure_eq {
  ($v1:expr, $v2:expr) => {
    ({
      let v1 = &$v1;
      let v2 = &$v2;
      if v1 != v2 {
        Err(anyhow!("ensure_eq failed: {} != {}: {:?} != {:?}",
                    stringify!($v1), stringify!($v2),
                    v1, v2))
      } else {
        Ok(())
      }
    }?)
  }
}

#[macro_export]
macro_rules! deref_to_field {
  {$({ $($gen:tt)* })? $outer:ty, $inner:ty, $($field:tt)*} => {
    impl $(< $($gen)* >)? Deref for $outer {
      type Target = $inner;
      fn deref(&self) -> &$inner { &self.$($field)* }
    }
  }
}
#[macro_export]
macro_rules! deref_to_field_mut {
  {$({ $($gen:tt)* })? $outer:ty, $inner:ty, $($field:tt)*} => {
    deref_to_field!{ $({ $($gen)* })? $outer, $inner, $($field)*}
    impl $(< $($gen)* >)? DerefMut for $outer {
      fn deref_mut(&mut self) -> &mut $inner { &mut self.$($field)* }
    }
  }
}
