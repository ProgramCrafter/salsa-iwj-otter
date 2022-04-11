// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// This is in this crate for convenience, not because it's to do with
// Z coordinates.

use crate::prelude::*;

pub const SVG_SCALE: f64 = 6.;

pub fn timestring_abbreviate<'x>(base: &str, this: &'x str)
                                 -> (&'x str, bool)
{
  fn split(s: &str) -> ArrayVec<&str, 3> {
    s.splitn(3, ' ').collect()
  }
  let base3 = split(base);
  let this3 = split(this);
  let matches = |i| base3.get(i) == this3.get(i);
  if_chain! {
    if matches(0) && matches(2);
    if let Some(abbrev) = this3.get(1);
    then { (abbrev, true) }
    else { (this, false) }
  }
}

pub fn raw_angle_transform(compass: u8) -> String {
  assert!(compass < 8);
  if compass == 0 { default() }
  else { format!("rotate({})", -45 * (compass as i16)) }
}

#[throws(fmt::Error)]
pub fn die_cooldown_path<W: fmt::Write>(mut w: W, r: f64, remaining: f64) {
  write!(w, "M 0,-{r} A")?;

  let mut arcto = move |proportion: f64| {
    let angle = proportion * TAU;
    let x = r * angle.sin();
    let y = -r * angle.cos();
    write!(w, " {r},{r} 0 0 1 {x},{y}")
    //                  | | `sweep-flag (1 = clockwise)
    //                  | `large-arc-flag (see below)
    //                  `"angle" (ellipse skew angle)
  };
  
  // This avoids ever trying to draw an arc segment that is around 180 degrees.
  // If we did so there could be rounding errors that would mean we might
  // disagree with the SVG renderer about whether the angle is <=> 180.
  for split in [0.49, 0.98] {
    if split >= remaining { break }
    arcto(split)?;
  }
  arcto(remaining)?;
}

#[test]
fn die_cooldown_path_test() {
  let t80 = |remaining, exp: &str| {
    let mut got = String::new();
    die_cooldown_path(&mut got, 80., remaining).unwrap();
    assert_eq!(&got, exp, "\nfor {remaining} {exp}");
  };
  t80(1./3., "M 0,-80 A 80,80 0 0 1 69.2820323027551,39.999999999999986");
  t80(1.   , "M 0,-80 A 80,80 0 0 1 5.023241562345087,79.84213827426173 80,80 0 0 1 -10.026658685144373,-79.36917610515822 80,80 0 0 1 -0.000000000000019594348786357652,-80");
}

pub fn default<T:Default>() -> T { Default::default() }

#[macro_export]
macro_rules! display_as_debug {
  {$x:ty $( , $($gen_tt:tt)* )?} => {
    impl $( $($gen_tt)* )? std::fmt::Display for $x {
      #[throws(std::fmt::Error)]
      fn fmt(&self, f: &mut std::fmt::Formatter) {
        <Self as Debug>::fmt(self, f)?
      }
    }
  }
}
pub use crate::display_as_debug;

pub type SvgAttrs = Vec<(HtmlLit,Html)>;

pub fn space_table_attrs(table_size: PosC<f64>) -> SvgAttrs {
  let PosC { coords: [x, y] } = table_size;
  vec![
    (Html::lit("viewBox"), hformat!("0 0 {} {}", x, y) ),
    (Html::lit("width"  ), (SVG_SCALE * x).to_html()  ),
    (Html::lit("height" ), (SVG_SCALE * y).to_html()  ),
  ]
}

pub fn space_rect_attrs(table_size: PosC<f64>) -> SvgAttrs {
  vec![
    (Html::lit("width" ), table_size.x().to_html()  ),
    (Html::lit("height"), table_size.y().to_html()  ),
  ]
}

#[macro_export]
macro_rules! if_let {
  { $($variant:ident)::+ ($binding:pat) = $input:expr;
    else $($otherwise:tt)*
  } => {
    let $binding = match $input {
      $($variant)::+ (y) => y,
      _ => { $($otherwise)* },
    };
  };
  { $($variant:ident)::+ ($binding:pat) = $input:expr;
    $($otherwise:tt)*
  } => {
    let $binding = match $input {
      $($variant)::+  (y) => y,
      $($otherwise)*,
    };
  };
  { $($variant:ident)::+ {$binding:ident} = $input:expr;
    else $($otherwise:tt)*
  } => {
    let $binding = match $input {
      $($variant)::+ { $binding } => $binding,
      _ => { $($otherwise)* },
    };
  };
  { $($variant:ident)::+ {$binding:ident} = $input:expr;
    $($otherwise:tt)*
  } => {
    let $binding = match $input {
      $($variant)::+ { $binding } => $binding,
      $($otherwise)*,
    };
  };
}

#[ext(pub, name=DebugExt)]
impl<T:Debug> T {
  fn to_debug(&self) -> String { format!("{:?}", self) }
}

pub fn dbgc_helper(file: &'static str, line: u32,
                   values: &[(&'static str, &dyn Debug)]) {
  let buf = (||{
    let mut buf = String::new();
    write!(buf, "[{}:{}]", file, line)?;
    for (s, v) in values.iter() {
      write!(buf, " {}={:?}", s, v)?;
    }
    write!(buf, "\n")?;
    Ok::<_,fmt::Error>(buf)
  })();
  let buf = buf.unwrap_or_else(
    |e| format!("error formatting for dbgc! {}\n", e));
  eprint!("{}", buf);
}

#[macro_export]
macro_rules! dbgc {
    // NOTE: We cannot use `concat!` to make a static string as a format argument
    // of `eprintln!` because `file!` could contain a `{` or
    // `$val` expression could be a block (`{ .. }`), in which case the `eprintln!`
    // will be malformed.
    () => {
      $crate::misc::dbgc_helper(std::file!(), std::line!(), &[])
    };
    ($val:expr $(,)?) => {
        // Use of `match` here is intentional because it affects the lifetimes
        // of temporaries - https://stackoverflow.com/a/48732525/1063961
        match $val {
            tmp => {
                $crate::misc::dbgc_helper(std::file!(), std::line!(),
                                          &[(std::stringify!($val), &tmp)]);
                tmp
            }
        }
    };
    ($($val:expr),+ $(,)?) => {
      $crate::misc::dbgc_helper(std::file!(), std::line!(),
                                &[$((std::stringify!($val), &$val)),+])
    };
}
