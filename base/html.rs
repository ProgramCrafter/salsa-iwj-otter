// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

#[derive(Clone,Serialize,Default,Deserialize,Hash,Eq,Ord,PartialEq,PartialOrd)]
#[serde(transparent)]
pub struct Html(String);

impl Debug for HtmlStr {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    const MAX: usize = 23;
    if self.len() < MAX {
      write!(f, "<{}>", &self.0)
    } else {
      write!(f, "<{}>...", &self.0[0..MAX-3])
    }
  }
}

impl AsRef<HtmlStr> for Html {
  fn as_ref(&self) -> &HtmlStr { HtmlStr::from_html_str(&self.0) }
}
impl AsRef<HtmlStr> for HtmlStr {
  fn as_ref(&self) -> &HtmlStr { self }
}
impl AsRef<HtmlStr> for HtmlLit {
  fn as_ref(&self) -> &HtmlStr { HtmlStr::from_html_str(self.0) }
}
impl Deref for Html {
  type Target = HtmlStr;
  fn deref(&self) -> &HtmlStr { HtmlStr::from_html_str(&self.0) }
}
impl Deref for HtmlLit {
  type Target = HtmlStr;
  fn deref(&self) -> &HtmlStr { HtmlStr::from_html_str(self.0) }
}

impl Debug for Html {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    Debug::fmt(self.as_ref(), f)
  }
}

#[derive(Hash,Eq,Ord,PartialEq,PartialOrd)]
#[repr(transparent)]
pub struct HtmlStr(str);

#[derive(Hash,Eq,Ord,PartialEq,PartialOrd)]
#[derive(Serialize,Deserialize)]
#[serde(transparent)]
pub struct HtmlLit(&'static str);

impl From<HtmlLit> for &'static HtmlStr {
  fn from(l: HtmlLit) -> &'static HtmlStr { HtmlStr::from_html_str(l.0) }
}

pub trait HtmlFormat<'e> {
  type Encoded: Display;
  fn html_format<'f: 'e>(&'f self) -> Self::Encoded;
}

impl From<HtmlLit> for Html {
  fn from(l: HtmlLit) -> Html { Html(l.0.into()) }
}
impl From<&HtmlStr> for Html {
  fn from(l: &HtmlStr) -> Html { Html(l.0.into()) }
}

impl<'e, T> HtmlFormat<'e> for &'e T where T: HtmlFormat<'e> + ?Sized {
  type Encoded = T::Encoded;
  fn html_format<'f: 'e>(&'f self) -> Self::Encoded {
    <T as HtmlFormat>::html_format(self)
  }
}

#[derive(Debug,Copy,Clone,Ord,PartialOrd,Eq,PartialEq,Hash)]
pub struct IsHtmlFormatted<T:Display>(pub T);
impl<'e, T:Display+'e> HtmlFormat<'e> for IsHtmlFormatted<T> {
  type Encoded = &'e T;
  fn html_format<'f: 'e>(&'f self) -> Self::Encoded { &self.0 }
}

impl Html {
  pub fn new() -> Self { default() }
  pub fn from_txt(s: &str) -> Self {
    Html(htmlescape::encode_minimal(s))
  }

  pub fn from_html_string(s: String) -> Self { Html(s) }
  pub fn as_html_string_mut(&mut self) -> &mut String { &mut self.0 }
  pub fn into_html_string(self) -> String { self.0 }

  pub const fn lit(s: &'static str) -> HtmlLit { HtmlLit(s) }
}

impl HtmlStr {
  pub fn from_html_str<'s>(s: &'s str) -> &'s Self {
    let s = unsafe { mem::transmute::<&'s str, &'s HtmlStr>(s) };
    s
  }
  #[allow(clippy::len_without_is_empty)]
  pub fn len(&self) -> usize { self.0.len() }
  pub fn as_html_str(&self) -> &str { &self.0 }
}

#[ext(pub, name=HtmlIteratorExt, supertraits=Iterator)]
impl<T:Iterator> T {
  fn hjoin<'i,'j, I,J>(self, j: &'j J) -> Html
  where
    Self: Iterator<Item=&'i I>,
    I: AsRef<HtmlStr> + 'i,
    J: AsRef<HtmlStr>,
  {
    let j: &HtmlStr = j.as_ref();
    Html::from_html_string(
      izip!(
        iter::once("").chain(iter::repeat(j.as_html_str())),
        self.map(|h| h.as_ref().as_html_str()),
      )
        .map(|(a,b)| iter::once(a).chain(iter::once(b)))
        .flatten()
        .collect::<String>()
    )
  }
}
  
impl Borrow<HtmlStr> for Html {
  fn borrow<'b>(&'b self) -> &'b HtmlStr {
    HtmlStr::from_html_str(&self.0)
  }
}
impl Borrow<HtmlStr> for HtmlLit {
  fn borrow<'b>(&'b self) -> &'static HtmlStr {
    HtmlStr::from_html_str(self.0)
  }
}

impl ToOwned for HtmlStr {
  type Owned = Html;
  fn to_owned(&self) -> Html { Html(self.0.to_owned()) }
}

#[ext(pub, name=HtmlFormatRef)]
impl<'e, T: HtmlFormat<'e>> T {
  fn to_html(&'e self) -> Html { hformat!("{}", *self) }
}

impl<'e> HtmlFormat<'e> for Html {
  type Encoded = &'e str;
  fn html_format<'f: 'e>(&'f self) -> Self::Encoded { &self.0 }
}
impl<'e> HtmlFormat<'e> for HtmlStr {
  type Encoded = &'e str;
  fn html_format<'f: 'e>(&'f self) -> Self::Encoded { &self.0 }
}
impl<'e> HtmlFormat<'e> for HtmlLit {
  type Encoded = &'static str;
  fn html_format<'f: 'e>(&'f self) -> Self::Encoded { self.0 }
}
impl<'e> HtmlFormat<'e> for str {
  type Encoded = String;
  fn html_format<'f: 'e>(&'f self) -> Self::Encoded {
    htmlescape::encode_minimal(self)
  }
}
impl<'e> HtmlFormat<'e> for String {
  type Encoded = String;
  fn html_format<'f: 'e>(&'f self) -> Self::Encoded {
    htmlescape::encode_minimal(self)
  }
}

hformat_as_display!{ usize u32 u64
                     isize i32 i64
                     f32 f64 }

#[macro_export]
macro_rules! hformat_as_display {
  ( $( $t:ty )* ) => {
    $(
      impl<'e> $crate::html::HtmlFormat<'e> for $t {
        type Encoded = &'e $t;
        fn html_format<'f: 'e>(&'f self) -> Self::Encoded { self }
      }
    )*
  }
}

#[macro_export]
macro_rules! hformat {
  ( $f:tt $(,$( $v:expr )?)* ) => {
    Html::from_html_string(
      format!(
        $f  $(,$(
          $crate::html::HtmlFormat::html_format(&$v)
        )?)*
      )
    )
  }
}

#[macro_export]
macro_rules! hwrite {
  ( $o:expr, $f:tt $(,$( $v:expr )?)* ) => {
    write!(
      $crate::html::Html::as_html_string_mut($o),
      $f $(,$(
        $crate::html::HtmlFormat::html_format(&$v)
      )?)*
    )
  }
}
