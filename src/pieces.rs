// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// pieces

use crate::prelude::*;

use piece_specs::SimpleCommon;

type ColourMap = IndexVec<FaceId, Colour>;

#[derive(Debug,Serialize,Deserialize)]
// todo: this serialisation is rather large
pub struct GenericSimpleShape<Desc, Outl> {
  pub desc: Desc,
  colours: ColourMap,
  #[serde(default)] pub edges: ColourMap,
  #[serde(default="default_edge_width")] pub edge_width: f64,
  pub itemname: String,
  pub outline: Outl,
}
pub type SimpleShape = GenericSimpleShape<Html, Outline>;

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
pub fn svg_rescale_path(input: &HtmlStr, scale: f64) -> Html {
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

  for w in input.as_html_str().split_ascii_whitespace() {
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
  Html::from_html_string(out)
}

#[throws(SvgE)]
pub fn svg_circle_path(diam: f64) -> Html {
  let unit_path = Html::lit(
    "M 0 1  a 1 1 0 1 0 0 -2 \
     a 1 1 0 1 0 0  2  z"
  );
  let scale = diam * 0.5;
  let path = svg_rescale_path(unit_path.into(), scale)?;
  path
}

#[throws(SvgE)]
pub fn svg_rectangle_path(PosC{coords: [x,y]}: PosC<f64>) -> Html {
  hformat!("M {} {} h {} v {} h {} z",
           -x*0.5, -y*0.5, x, y, -x)
}

#[dyn_upcast]
impl<Desc, Outl:'static> OutlineTrait for GenericSimpleShape<Desc, Outl>
    where Desc: Debug + Send + Sync + 'static,
          Outl: OutlineTrait,
{
  delegate! {
    to self.outline {
      fn outline_path(&self, scale: f64) -> Result<Html,IE>;
      fn thresh_dragraise(&self) -> Result<Option<Coord>,IE>;
      fn bbox_approx(&self) -> Result<Rect, IE>;
    }
  }
}
//    let edge_attrs = format!(r##"stroke-width="" stroke"##

impl PieceBaseTrait for SimpleShape {
  fn nfaces(&self) -> RawFaceId { self.count_faces() }

  fn itemname(&self) -> &str { self.itemname() }
}

#[typetag::serde]
impl PieceTrait for SimpleShape {
  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, gpc: &GPiece,
               _gs: &GameState, _vpid: VisiblePieceId) {
    self.svg_piece_raw(f, gpc.face, &mut |_|Ok(()))?;
  }
  #[throws(IE)]
  fn describe_html(&self, gpc: &GPiece, _goccults: &GameOccults) -> Html {
    let r = if_chain! {
      if let face = gpc.face;
      if let Some(colour) = self.colours.get(face);
      then { hformat!("a {} {}", colour, self.desc) }
      else { hformat!("a {}", self.desc) }
    };
    r
  }
}

#[typetag::serde]
impl InertPieceTrait for SimpleShape {
  #[throws(IE)]
  fn svg(&self, f: &mut Html, _vpid: VisiblePieceId, face: FaceId,
         _: &PieceXDataState) {
    self.svg_piece_raw(f, face, &mut |_|Ok(()))?; 
  }

  #[throws(IE)]
  fn describe_html(&self, _: FaceId) -> Html {
    hformat!("a {}", self.desc)
  }
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct PieceLabelLoaded {
  #[serde(default)] pub place: piece_specs::PieceLabelPlace,
  pub colour: Option<Colour>,
}

impl piece_specs::PieceLabel {
  #[throws(SpecError)]
  pub fn load(&self) -> PieceLabelLoaded {
    let Self { place, ref colour } = *self;
    let colour = colour.as_ref().map(|c| c.try_into()).transpose()?;
    PieceLabelLoaded { place, colour }
  }
}  

#[ext(pub)]
impl Option<piece_specs::PieceLabel> {
  #[throws(SpecError)]
  fn load(&self) -> Option<PieceLabelLoaded> {
    self.as_ref().map(|l| l.load()).transpose()?
  }
}

impl PieceLabelLoaded {
  #[throws(IE)]
  pub fn svg(&self, f: &mut Html,
             outline: &RectShape,
             def_colour: Option<&Colour>,
             text: &Html) {
    let colour = {
      if let Some(c) = &self.colour { c.borrow() }
      else if let Some(c) = def_colour { c.borrow() }
      else { Html::lit("black").into() }
    };
    let fontsz = 4.;
    let PosC{ coords: [x,y] } = {
      use piece_specs::PieceLabelPlace::*;
      let inout = match self.place {
        BottomLeft        | TopLeft        =>  1.,
        BottomLeftOutside | TopLeftOutside => -1.,
      };
      let eff_size = (outline.xy - PosC::new(2., inout * 2.))?;
      let mut pos = (eff_size * -0.5)?;
      let y = &mut pos.coords[1];
      *y += 0.5 * fontsz * inout;
      match self.place {
        BottomLeft | BottomLeftOutside => { *y *= -1. },
           TopLeft |    TopLeftOutside => {           },
      };
      *y += 0.5 * fontsz;
      pos
    };
    hwrite!(f,
            r##"<text x="{}" y="{}" font-size="{}" fill="{}">{}</text>"##,
            x, y, fontsz, colour, &text
    )?;
  }  
}

impl<Desc, Outl:'static> GenericSimpleShape<Desc, Outl>
    where Desc: Debug + Send + Sync + 'static,
          Outl: OutlineTrait,
{
  pub fn count_faces(&self) -> RawFaceId {
    max(self.colours.len(), self.edges.len()).try_into().unwrap()
  }
  pub fn itemname(&self) -> &str { &self.itemname }

  #[throws(SpecError)]
  pub fn new(desc: Desc, outline: Outl,
         def_itemname: &'_ str,
         common: &SimpleCommon)
         -> GenericSimpleShape<Desc, Outl>
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

    let shape = GenericSimpleShape {
      desc, itemname, outline,
      colours: cmap(&common.faces)?,
      edges: cmap(&common.edges)?,
      edge_width: common.edge_width.unwrap_or(DEFAULT_EDGE_WIDTH),
    };

    let count = shape.count_faces();
    if count == 0 { throw!(SpecError::ZeroFaces) }

    let check = |colours: &ColourMap| {
      let x = colours.len();
      if x == 0 || x == usize::from(count) { Ok(()) }
      else { Err(SpecError::InconsistentFacesEdgecoloursCount) }
    };
    check(&shape.colours)?;
    check(&shape.edges)?;

    shape
  }

  #[throws(IE)]
  pub fn svg_piece_raw(
    &self, f: &mut Html, face: FaceId,
    stroke_attrs_hook: &mut dyn FnMut(&mut Html) -> Result<(),IE>,
  ) {
    let ef = |f: &mut Html, cmap: &ColourMap, attrname: &str, otherwise| {
      if let Some(colour) = cmap.get(face) {
        hwrite!(f, r##" {}="{}""##, attrname, colour)
      } else {
        hwrite!(f, "{}", otherwise)
      }
    };
    let path = self.outline_path(1.0)?;

    if self.colours.len() == 0 {
      hwrite!(f,
             r##"<path fill="none" \
                  stroke-width="{}" stroke="transparent" d="{}"/>"##,
             INVISIBLE_EDGE_SENSITIVE,
             &path)?;
    }
    hwrite!(f, r##"<path"##)?;
    ef(f, &self.colours, "fill", r##" fill="none""##)?;
    if self.edges.len() != 0 {
      hwrite!(f, r##" stroke-width="{}""##, &self.edge_width)?;
    }
    stroke_attrs_hook(f)?;
    ef(f, &self.edges, "stroke", "")?;
    hwrite!(f, r##" d="{}"/>"##, &path)?;
  }
}

#[typetag::serde(tag="type")]
pub trait SimplePieceSpec: Debug {
  fn load_raw(&self) -> Result<(SimpleShape, &SimpleCommon), SpecError>;
  #[throws(SpecError)]
  fn load(&self) -> PieceSpecLoaded {
    PieceSpecLoaded {
      p: Box::new(self.load_raw()?.0),
      occultable: None,
    }
  }
}

#[typetag::serde]
impl SimplePieceSpec for piece_specs::Disc {
  #[throws(SpecError)]
  fn load_raw(&self) -> (SimpleShape, &SimpleCommon) {
    let outline = CircleShape { diam: self.diam as f64 };
    (SimpleShape::new(
      Html::lit("disc").into(),
      outline.into(),
      "simple-disc",
      &self.common,
    )?, &self.common)
  }
}

impl piece_specs::Rect {
  #[throws(SpecError)]
  fn xy(&self) -> Pos {
    match *self.size.as_slice() {
      [s,]  => PosC::new(s,s),
      [x,y] => PosC::new(x,y),
      _ => throw!(SpecError::ImproperSizeSpec),
    }
  }
}

#[typetag::serde]
impl SimplePieceSpec for piece_specs::Rect {
  #[throws(SpecError)]
  fn load_raw(&self) -> (SimpleShape, &SimpleCommon) {
    let outline = RectShape { xy: self.xy()?.map(|v| v as f64) };
    let desc = Html::lit(
      if outline.xy.x() == outline.xy.y()
      { "square" } else { "rectangle" }
    ).into();
    (SimpleShape::new(
      desc,
      outline.into(),
      "simple-rect",
      &self.common,
    )?, &self.common)
  }
}

macro_rules! impl_PieceSpec_for_SimplePieceSpec { { $ty:ty } => {
  #[typetag::serde]
  impl PieceSpec for $ty {
    #[throws(SpecError)]
    fn load(&self, _: usize, _: &mut GPiece, _ig: &Instance, _:SpecDepth)
            -> PieceSpecLoaded {
      SimplePieceSpec::load(self)?
    }
  }
} }

impl_PieceSpec_for_SimplePieceSpec!{piece_specs::Disc}
impl_PieceSpec_for_SimplePieceSpec!{piece_specs::Rect}
