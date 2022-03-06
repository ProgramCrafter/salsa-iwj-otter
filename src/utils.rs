// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;
use crate::prelude::*;

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

#[derive(Copy,Clone,Debug,From,Into)]
#[derive(Hash,Eq,PartialEq,Serialize,Deserialize)]
#[serde(transparent)]
pub struct OldNew<T>([T; 2]);

#[derive(Copy,Clone,Debug)]
#[derive(Hash,Eq,PartialEq)]
pub enum OldNewIndex { Old, New }

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

  pub fn into_iter(self) -> impl Iterator<Item=T> {
    IntoIterator::into_iter(self.0)
  }
}

impl<T> Index<OldNewIndex> for OldNew<T> {
  type Output = T;
  fn index(&self, i: OldNewIndex) -> &T { &self.0[i as usize] }
}

/*
put trait OptionExt {
  type Output;
  fn get_or_try_insert_with<
      E: Error,
      F: FnOnce() -> Result<Output,E>,
    >(&mut self, f: F) -> Result<&mut Output, E>;
}

impl<T> OptionExt for Option<T> {
  type Output = T;
  fn get_or_try_insert_with<E,F>
    (&mut self, f: F) -> Result<&mut Output, E>
    where E: Error, F: FnOnce() -> Result<Output,E>,
  {
    if self.is_none() {
      *self = Some(f()?);
    }
    Ok(self.as_mut().unwrap())
  }
}
*/

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

#[derive(Error,Clone,Copy,Debug,Eq,PartialEq,Serialize,Deserialize)]
#[error("error parsing Z coordinate")]
pub struct FooParseError;

pub mod timespec_serde {
  use super::*;

  #[derive(Serialize, Deserialize)]
  struct Timespec(i64, u32);

  #[throws(S::Error)]
  pub fn serialize<S:Serializer>(v: &TimeSpec, s: S) -> S::Ok {
    let v = Timespec(v.tv_sec().into(), v.tv_nsec().try_into().unwrap());
    Serialize::serialize(&v, s)?
  }
  #[throws(D::Error)]
  pub fn deserialize<'de, D:Deserializer<'de>>(d: D) -> TimeSpec {
    let Timespec(sec, nsec) = Deserialize::deserialize(d)?;
    libc::timespec { tv_sec: sec.into(), tv_nsec: nsec.into() }.into()
  }
}

pub fn toml_merge<'u,
                  S: 'u + AsRef<str>,
                  KV: IntoIterator<Item=(&'u S, &'u toml::Value)>
                  >(
  table: &mut toml::value::Table,
  updates: KV,
) {
  use toml::value::{Table, Value};
  type TME<'e> = toml::map::Entry<'e>;

  let mut kv = updates.into_iter().map(|(k, v)| (k.as_ref(), v));
  inner(table, &mut kv);

  fn inner<'u>(
    table: &mut Table,
    updates: &'u mut dyn Iterator<Item=(&'u str, &'u Value)>
  ) {
    for (k, v) in updates {
      let e = table.entry(k);
      match e {
        TME::Vacant(ve) => {
          ve.insert(v.clone());
        }
        TME::Occupied(mut oe) => match (oe.get_mut(), v) {
          (Value::Table(old), Value::Table(new)) => {
            toml_merge(old, new);
          }
          (Value::Array(old), Value::Array(new)) => {
            old.extend(new.iter().cloned());
          }
          (old, new) => {
            *old = new.clone();
          }
        }
      }
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

#[derive(Debug)]
pub enum Loop<E> {
  Continue,
  Break,
  Error(E),
}
impl<E> From<E> for Loop<E> {
  fn from(e: E) -> Loop<E> { Loop::Error(e) }
}
impl Loop<Infallible> {
  pub fn ok<T>(t: T) -> Result<T,Loop<Infallible>> { Ok(t) }
}

pub trait IteratorExt<U,E,F>: Iterator
  where F: FnMut(Self::Item) -> Result<U,Loop<E>>,
{
  type Return: Iterator<Item=U>;
  fn map_loop(self, f: F) -> Self::Return where E: EmptyType;

  type TryReturn: Iterator<Item=Result<U,E>>;
  fn try_map_loop(self, f: F) -> Self::TryReturn;
}

pub trait EmptyType { fn diverge<T>(self) -> T; }

impl EmptyType for Infallible {
  fn diverge<T>(self) -> T { match self { } }
}

impl<T,U,E,F> IteratorExt<U,E,F> for T where
  T: Iterator,
  F: FnMut(Self::Item) -> Result<U,Loop<E>>,
{
  type Return = impl Iterator<Item=U>;
  fn map_loop(self, f: F) -> Self::Return where E: EmptyType {
    self
      .map(f)
      .filter(|i| !matches!(i, Err(Loop::Continue)))
      .take_while(|i| !matches!(i, Err(Loop::Break)))
      .map(|i| i.ok().unwrap())
  }

  type TryReturn = impl Iterator<Item=Result<U,E>>;
  fn try_map_loop(self, f: F) -> Self::TryReturn {
    self
      .map(f)
      .filter(|i| matches!(i, Err(Loop::Continue)))
      .take_while(|i| matches!(i, Err(Loop::Break)))
      .map(|i| match i {
        Ok(y) => Ok(y),
        Err(Loop::Error(e)) => Err(e),
        _ => panic!(),
      })
  }
}

#[macro_export] // <- otherwise bogus warning `unused_macros`
macro_rules! matches_doesnot_yn2bool {
  (=) => (true);
  (!) => (false);
}

#[macro_export]
macro_rules! matches_doesnot {
  ($v:expr,
   $(
     $yn:tt $($p:pat)|*
   ),* $(,)?
  ) => {
    match $v {
      $(
        $($p)|* => $crate::matches_doesnot_yn2bool!($yn),
      )*
    }
  }
}

#[test]
fn matches_doesnot_test() {
  assert!(
    matches_doesnot!(
      Some(42),
      = Some(_),
      ! None
    )
  );
  assert!(
    matches_doesnot!(
      Some(42),
      ! None,
      ! Some(3),
      = Some(_),
    )
  );
  assert!(
    matches_doesnot!(
      Some(1),
      = Some(1) | Some(2),
      ! Some(_) | None
    )
  );
  assert!(
    ! matches_doesnot!(
      Some(1),
      ! Some(1) | Some(2),
      = Some(_) | None
    )
  );
}

#[macro_export]
macro_rules! trace_dbg {
  ($msg:expr $(,$val:expr)*) => {
    use log::*;
    use Level::*;
    if log_enabled!(Trace) {
      let mut buf = format!("{}", &$msg);
      $( write!(&mut buf, " {}={:?}", stringify!($val), &$val).unwrap(); )*
      trace!("{}", buf);
    }
  }

}

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

macro_rules! entry_define_insert_remove {
  { $name:ident, $name_mod:ident, $entry:path, $into_key:ident } =>
  {
    #[allow(non_snake_case)]
    mod $name_mod {
      use $crate::imports::extend::ext;
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

#[derive(Debug,Copy,Clone)]
pub struct DigestRead<D: Digest, R: Read> {
  d: D,
  r: R,
}

impl<D: Digest, R: Read> DigestRead<D, R> {
  pub fn new(r: R) -> Self { DigestRead { r, d: D::new() } }
  pub fn into_inner(self) -> (D, R) { (self.d, self.r) }
  pub fn finish(self) -> digest::Output<D> {
    self.d.finalize()
  }
}

impl<D: Digest, R: Read> Read for DigestRead<D, R> {
  #[throws(io::Error)]
  fn read(&mut self, buf: &mut [u8]) -> usize {
    let count = self.r.read(buf)?;
    self.d.update(&buf[0..count]);
    count
  }
}

#[test]
fn test_digest_read() {
  let ibuffer = b"abc";
  let exp = Sha512_256::digest(&ibuffer[..]);
  let inner = &ibuffer[..];
  let mut dr = DigestRead::<Sha512_256,_>::new(inner);
  let mut obuffer = [0;4];
  assert_eq!( dr.read(&mut obuffer).unwrap(), 3 );
  assert_eq!( &obuffer, b"abc\0" );
  let got = dr.finish();
  assert_eq!( got, exp );
}

#[derive(Debug,Copy,Clone)]
pub struct DigestWrite<D: Digest, W: Write> {
  d: D,
  w: W,
}

impl<D: Digest, W: Write> DigestWrite<D, W> {
  pub fn new(w: W) -> Self { DigestWrite { w, d: D::new() } }
  pub fn into_inner(self) -> (D, W) { (self.d, self.w) }
  pub fn finish(self) -> (digest::Output<D>, W) {
    (self.d.finalize(), self.w)
  }
}
impl<D: Digest> DigestWrite<D, io::Sink> {
  pub fn sink() -> Self { DigestWrite::new(io::sink()) }

  #[throws(io::Error)]
  pub fn of<R>(r: &mut R) -> digest::Output<D> where R: Read {
    let mut dw = DigestWrite::<D,_>::sink();
    io::copy(r, &mut dw)?;
    dw.finish().0
  }
}

impl<D: Digest, W: Write> Write for DigestWrite<D, W> {
  #[throws(io::Error)]
  fn write(&mut self, buf: &[u8]) -> usize {
    let count = self.w.write(buf)?;
    self.d.update(&buf[0..count]);
    count
  }
  #[throws(io::Error)]
  fn flush(&mut self) { self.w.flush()? }
}

#[test]
fn test_digest_write() {
  let ibuffer = b"xyz";
  let exp = Sha512_256::digest(&ibuffer[..]);
  let mut obuffer = [0;4];
  let inner = &mut obuffer[..];
  let mut dw = bundles::DigestWrite::new(inner);
  assert_eq!( dw.write(&ibuffer[..]).unwrap(), 3);
  let (got, recov) = dw.finish();
  assert_eq!( recov, b"\0" );
  assert_eq!( got, exp );
  assert_eq!( &obuffer, b"xyz\0" );
}

#[ext(pub, name=SeekExt)]
impl<T: io::Seek> T {
  #[throws(io::Error)]
  fn rewind(&mut self) { self.seek(io::SeekFrom::Start(0))? }
}

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
impl RawStdout { pub fn new() -> Self { SigPipeWriter(io::stdout()) } }

pub struct CookedStdout(pub BufWriter<SigPipeWriter<io::Stdout>>);
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

#[throws(fmt::Error)]
pub fn fmt_hex(f: &mut Formatter, buf: &[u8]) {
  for v in buf { write!(f, "{:02x}", v)?; }
}

#[throws(as Option)]
#[must_use]
pub fn parse_slice_hex(s: &str, buf: &mut [u8]) -> usize {
  let l = s.len();
  if l % 1 != 0 { throw!() }
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
