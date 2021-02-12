// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// pieces

use crate::imports::*;

type ColourMap = IndexVec<FaceId, Colour>;

type SE = SVGProcessingError;

#[derive(Debug,Serialize,Deserialize)]
// todo: this serialisation is rather large
struct SimpleShape {
  desc: Html,
  path: Html,
  colours: ColourMap,
  itemname: String,
  outline: Box<dyn Outline>,
}

pub const SELECT_SCALE: f64 = 1.1;

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

#[throws(SE)]
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
      _ => throw!(SE::UnknownOperator),
    };
    write!(&mut out, "{}", w)?;
  }

  trace!("rescaled by {}: {:?} as {:?}", scale, input, &out);
  Html(out)
}

#[throws(SE)]
pub fn svg_circle_path(diam: f64) -> Html {
  let unit_path = Html::lit(
    "M 0 1  a 1 1 0 1 0 0 -2 \
     a 1 1 0 1 0 0  2  z"
  );
  let scale = diam * 0.5;
  let path = svg_rescale_path(&unit_path, scale)?;
  path
}

#[throws(SE)]
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

#[typetag::serde]
impl Piece for SimpleShape {
  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, pri: &PieceRenderInstructions) {
    write!(&mut f.0, r##"<path fill="{}" d="{}"/>"##,
           self.colours[pri.face].0,
           &self.path.0)?;
  }
  fn describe_html(&self, face: Option<FaceId>) -> Html {
    Html(if let Some(face) = face {
      format!("a {} {}", self.colours[face].0, self.desc.0)
    } else {
      format!("a {}", self.desc.0)
    })
  }
  fn nfaces(&self) -> RawFaceId { self.colours.len().try_into().unwrap() }

  fn itemname(&self) -> &str { &self.itemname }
}

impl SimpleShape {
  fn new_from_path(desc: Html, path: Html,
                   faces: &IndexVec<FaceId,ColourSpec>,
                   outline: Box<dyn Outline>,
                   itemname: String)
                   -> Result<Box<dyn Piece>,SpecError> {
    let colours = faces
      .iter()
      .map(|s| s.try_into())
      .collect::<Result<_,SpecError>>()?;
    Ok(Box::new(SimpleShape {
      desc, path, colours, itemname, outline,
    }))
  }
}

type FacesSpec = IndexVec<FaceId,ColourSpec>;

trait SimplePieceSpec {
  fn outline(&self) -> Result<Box<dyn Outline>, SpecError>;
  fn path(&self) -> Result<Html, SpecError>;
  fn faces(&self) -> Result<&FacesSpec, SpecError>;
  fn desc(&self) -> Result<Html, SpecError>;
  fn itemname(&self) -> Result<String, SpecError>;

  #[throws(SpecError)]
  fn load(&self) -> Box<dyn Piece> {
    SimpleShape::new_from_path(self.desc()?,
                               self.path()?,
                               self.faces()?,
                               self.outline()?,
                               self.itemname()?)?
  }
}

impl SimplePieceSpec for piece_specs::Disc {
  fn outline(&self) -> Result<Box<dyn Outline>, SpecError> { Ok(Box::new(
    shapelib::Circle { diam: self.diam as f64 }
  ))}
  #[throws(SpecError)] fn path(&self) -> Html {
    svg_circle_path(self.diam as f64)?
  }
  #[throws(SpecError)] fn faces(&self) -> &FacesSpec { &self.faces }
  #[throws(SpecError)] fn desc(&self) -> Html { Html::lit("disc") }
  #[throws(SpecError)] fn itemname(&self) -> String {
    self.itemname.clone()
      .unwrap_or_else(||"simple-disc".to_string())
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

impl SimplePieceSpec for piece_specs::Square {
  fn outline(&self) -> Result<Box<dyn Outline>, SpecError> { Ok(Box::new(
    shapelib::Square { xy: self.xy()?.map(|v| v as f64) }
  ))}
  #[throws(SpecError)] fn path(&self) -> Html {
    svg_rectangle_path(self.xy()?.promote())?
  }
  #[throws(SpecError)] fn faces(&self) -> &FacesSpec { &self.faces }
  #[throws(SpecError)] fn desc(&self) -> Html { Html::lit("square") }
  #[throws(SpecError)] fn itemname(&self) -> String {
    self.itemname.clone()
      .unwrap_or_else(||"simple-square".to_string())
  }
}

#[typetag::serde]
impl PieceSpec for piece_specs::Square {
  #[throws(SpecError)]
  fn load(&self, _: usize) -> Box<dyn Piece> { SimplePieceSpec::load(self)? }
}
