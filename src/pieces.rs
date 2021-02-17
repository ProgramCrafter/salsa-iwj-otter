// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// pieces

use crate::prelude::*;

use piece_specs::SimpleCommon;

type ColourMap = IndexVec<FaceId, Colour>;

#[derive(Debug,Serialize,Deserialize)]
// todo: this serialisation is rather large
pub struct SimpleShape {
  pub desc: Html,
  pub path: Html,
  colours: ColourMap,
  #[serde(default)] pub edges: ColourMap,
  #[serde(default="default_edge_width")] pub edge_width: f64,
  pub itemname: String,
  pub outline: Box<dyn Outline>,
}

pub const SELECT_SCALE: f64 = 1.1;

pub const DEFAULT_EDGE_WIDTH: f64 = 0.2;
pub const INVISIBLE_EDGE_SENSITIVE: f64 = 2.;

fn default_edge_width() -> f64 { DEFAULT_EDGE_WIDTH }

#[derive(Copy,Clone,Debug,Error,Serialize,Deserialize)]
pub enum SVGProcessingError {
  UnknownOperator,
  BadNumber,
  WriteFail,
  NegativeDragraise,
}

display_as_debug!{SVGProcessingError}
error_from_losedetails!{SVGProcessingError,WriteFail,fmt::Error}
error_from_losedetails!{SVGProcessingError,BadNumber,std::num::ParseFloatError}

impl From<SVGProcessingError> for MgmtError {
  fn from(se: SVGProcessingError) -> MgmtError { se.into() }
}

#[throws(SvgE)]
pub fn svg_rescale_path(input: &Html, scale: f64) -> Html {
  type BM = u64;
  type BI = u32;
  #[derive(Debug,Copy,Clone)]
  struct RotatingBitmap {
    bits: BM,
    len: BI,
    index: BI,
  }
  impl RotatingBitmap {
    const fn new(bits: BM, len: BI) -> Self { Self{ bits, len, index:0 } }
    fn reset(&mut self) { self.index= 0; }
    fn next(&mut self) -> bool {
      let r = (self.bits >> (self.len-1 - self.index)) & 1 != 0;
      self.index += 1;
      if self.index == self.len { self.index = 0; }
      r
    }
  }
  const ALWAYS_MAP: RotatingBitmap = RotatingBitmap::new(0x01, 1);

  let mut out = String::new();
  let mut map = ALWAYS_MAP;
  let mut first = iter::once(());

  for w in input.0.split_ascii_whitespace() {
    if first.next().is_none() { write!(&mut out, " ")?; }
    match w {
      "L" | "l" | "M" | "m" |
      "V" | "v" | "H" | "h" => map = ALWAYS_MAP,
      "A" | "a"             => map = RotatingBitmap::new(0x63, 7),
      "z"                   => map.reset(),
      v if v.starts_with(|s:char| s=='-' || s=='.' || s.is_ascii_digit()) => {
        if map.next() {
          let v: f64 = v.parse()?;
          write!(&mut out, "{}", v * scale)?;
          continue;
        }
      }
      _ => throw!(SvgE::UnknownOperator),
    };
    write!(&mut out, "{}", w)?;
  }

  trace!("rescaled by {}: {:?} as {:?}", scale, input, &out);
  Html(out)
}

#[throws(SvgE)]
pub fn svg_circle_path(diam: f64) -> Html {
  let unit_path = Html::lit(
    "M 0 1  a 1 1 0 1 0 0 -2 \
     a 1 1 0 1 0 0  2  z"
  );
  let scale = diam * 0.5;
  let path = svg_rescale_path(&unit_path, scale)?;
  path
}

#[throws(SvgE)]
pub fn svg_rectangle_path(PosC([x,y]): PosC<f64>) -> Html {
  Html(format!("M {} {} h {} v {} h {} z",
               -x*0.5, -y*0.5, x, y, -x))
}

#[typetag::serde]
impl Outline for SimpleShape {
  delegate! {
    to self.outline {
      fn surround_path(&self, _pri: &PieceRenderInstructions)
                       -> Result<Html,IE>;
      fn thresh_dragraise(&self, _pri: &PieceRenderInstructions)
                          -> Result<Option<Coord>,IE>;
      fn bbox_approx(&self) -> [Pos;2];
    }
  }
}
//    let edge_attrs = format!(r##"stroke-width="" stroke"##

#[typetag::serde]
impl Piece for SimpleShape {
  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, _gpc: &PieceState,
               pri: &PieceRenderInstructions) {
    self.svg_piece_raw(f, pri, &mut |_|Ok(()))?;
  }
  #[throws(IE)]
  fn describe_html(&self, face: Option<FaceId>, _gpc: &PieceState) -> Html {
    Html(if_chain! {
      if let Some(face) = face;
      if let Some(colour) = self.colours.get(face);
      then { format!("a {} {}", colour.0, self.desc.0) }
      else { format!("a {}", self.desc.0) }
    })
  }
  fn nfaces(&self) -> RawFaceId { self.count_faces().try_into().unwrap() }

  fn itemname(&self) -> &str { &self.itemname }
}

impl SimpleShape {
  fn count_faces(&self) -> usize { max(self.colours.len(), self.edges.len()) }

  #[throws(SpecError)]
  fn new(desc: Html, path: Html,
         outline: Box<dyn Outline>,
         def_itemname: &'_ str,
         common: &SimpleCommon)
         -> SimpleShape
  {
    let itemname = common.itemname.clone()
      .unwrap_or_else(|| def_itemname.to_string());

    let cmap = |spec: &FaceColourSpecs| Ok::<_,SpecError>(
      spec
        .iter()
        .map(|s| s.try_into())
        .collect::<Result<_,_>>()?
    );

    if common.edge_width.is_some() && common.edges.len() == 0 {
      throw!(SpecError::SpecifiedWidthOfNoEdges);
    }

    let shape = SimpleShape {
      desc, path, itemname, outline,
      colours: cmap(&common.faces)?,
      edges: cmap(&common.edges)?,
      edge_width: common.edge_width.unwrap_or(DEFAULT_EDGE_WIDTH),
    };

    let count = shape.count_faces();
    if count == 0 { throw!(SpecError::ZeroFaces) }

    let check = |colours: &ColourMap| {
      let x = colours.len();
      if x == 0 || x == count { Ok(()) }
      else { Err(SpecError::InconsistentFacesEdgecoloursCount) }
    };
    check(&shape.colours)?;
    check(&shape.edges)?;

    shape
  }

  #[throws(IE)]
  pub fn svg_piece_raw(
    &self, f: &mut Html, pri: &PieceRenderInstructions,
    stroke_attrs_hook: &mut dyn FnMut(&mut String) -> Result<(),IE>,
  ) {
    let f = &mut f.0;
    let ef = |f: &mut String, cmap: &ColourMap, attrname: &str, otherwise| {
      if let Some(colour) = cmap.get(pri.face) {
        write!(f, r##" {}="{}""##, attrname, colour.0)
      } else {
        write!(f, "{}", otherwise)
      }
    };
    if self.colours.len() == 0 {
      write!(f,
             r##"<path fill="none" \
                  stroke-width="{}" stroke="transparent" d="{}"/>"##,
             INVISIBLE_EDGE_SENSITIVE,
             &self.path.0)?;
    }
    write!(f, r##"<path"##)?;
    ef(f, &self.colours, "fill", r##" fill="none""##)?;
    if self.edges.len() != 0 {
      write!(f, r##" stroke-width="{}""##, &self.edge_width)?;
    }
    stroke_attrs_hook(f)?;
    ef(f, &self.edges, "stroke", "")?;
    write!(f, r##" d="{}"/>"##, &self.path.0)?;
  }
}

#[typetag::serde(tag="type")]
pub trait SimplePieceSpec: Debug {
  fn load_raw(&self) -> Result<(SimpleShape, &SimpleCommon), SpecError>;
  fn load(&self) -> Result<Box<dyn Piece>, SpecError> {
    Ok(Box::new(self.load_raw()?.0))
  }
}

#[typetag::serde]
impl SimplePieceSpec for piece_specs::Disc {
  #[throws(SpecError)]
  fn load_raw(&self) -> (SimpleShape, &SimpleCommon) {
    let outline = shapelib::Circle { diam: self.diam as f64 };
    (SimpleShape::new(
      Html::lit("disc"),
      svg_circle_path(self.diam as f64)?,
      Box::new(outline),
      "simple-disc",
      &self.common,
    )?, &self.common)
  }
}

#[typetag::serde]
impl PieceSpec for piece_specs::Disc {
  #[throws(SpecError)]
  fn load(&self, _: usize) -> Box<dyn Piece> { SimplePieceSpec::load(self)? }
}

impl piece_specs::Square {
  #[throws(SpecError)]
  fn xy(&self) -> Pos {
    match *self.size.as_slice() {
      [s,]  => PosC([s,s]),
      [x,y] => PosC([x,y]),
      _ => throw!(SpecError::ImproperSizeSpec),
    }
  }
}

#[typetag::serde]
impl SimplePieceSpec for piece_specs::Square {
  #[throws(SpecError)]
  fn load_raw(&self) -> (SimpleShape, &SimpleCommon) {
    let outline = shapelib::Square { xy: self.xy()?.map(|v| v as f64) };
    (SimpleShape::new(
      Html::lit("square"),
      svg_rectangle_path(self.xy()?.promote())?,
      Box::new(outline),
      "simple-square",
      &self.common,
    )?, &self.common)
  }
}

#[typetag::serde]
impl PieceSpec for piece_specs::Square {
  #[throws(SpecError)]
  fn load(&self, _: usize) -> Box<dyn Piece> { SimplePieceSpec::load(self)? }
}
