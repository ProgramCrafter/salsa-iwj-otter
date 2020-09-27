// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// pieces

use crate::imports::*;

type ColourMap = IndexVec<FaceId,Colour>;

#[derive(Debug,Serialize,Deserialize)]
// todo: this serialisation is rather large
struct SimpleShape {
  desc : Html,
  path : Html,
  colours : ColourMap,
  itemname: String,
  outline: Box<dyn Outline>,
}

pub const SELECT_SCALE : f64 = 1.1;

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

type IE = InternalError;
type SE = SVGProcessingError;

#[throws(SE)]
pub fn svg_rescale_path(input: &Html, scale: f64) -> Html {
  type BM = u64;
  type BI = u32;
  #[derive(Debug,Copy,Clone)]
  struct RotatingBitmap {
    bits: BM,
    len: BI,
    index: BI,
  };
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
  const ALWAYS_MAP : RotatingBitmap = RotatingBitmap::new(0x01, 1);
  
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
          let v : f64 = v.parse()?;
          write!(&mut out, "{}", v * scale)?;
          continue;
        }
      }
      _ => throw!(SE::UnknownOperator),
    };
    write!(&mut out, "{}", w)?;
  }

  trace!("rescaled by {}: {:?} as {:?}",scale,input,&out);
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
pub fn svg_rectangle_path([x, y] : [f64;2]) -> Html {
  Html(format!("M {} {} h {} v {} h {} z",
               -x*0.5, -y*0.5, x, y, -x))
}

#[typetag::serde]
impl Outline for SimpleShape {
  delegate! {
    to self.outline {
      fn surround_path(&self, _pri : &PieceRenderInstructions)
                       -> Result<Html,IE>;
      fn thresh_dragraise(&self, _pri : &PieceRenderInstructions)
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
  #[throws(IE)]
  fn svg_x_defs(&self, _f: &mut Html, _pri : &PieceRenderInstructions) {
  }
  fn describe_html(&self, face : Option<FaceId>) -> Html {
    Html(if let Some(face) = face {
      format!("a {} {}", self.colours[face].0, self.desc.0)
    } else {
      format!("a {}", self.desc.0)
    })
  }
  #[throws(SpecError)]
  fn resolve_spec_face(&self, face: Option<FaceId>) -> FaceId {
    let face = face.unwrap_or_default();
    self.colours.get(face).ok_or(SpecError::FaceNotFound)?;
    face
  }

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

#[typetag::serde]
impl PieceSpec for piece_specs::Disc {
  #[throws(SpecError)]
  fn load(&self) -> Box<dyn Piece> {
    let outline = Box::new(shapelib::Circle { diam: self.diam as f64 });
    let path = svg_circle_path(self.diam as f64)?;
    let itemname = self.itemname.clone()
      .unwrap_or_else(||"simple-disc".to_string());
    SimpleShape::new_from_path(Html::lit("circle"), path,
                               &self.faces, outline, itemname)?
  }
}

#[typetag::serde]
impl PieceSpec for piece_specs::Square {
  #[throws(SpecError)]
  fn load(&self) -> Box<dyn Piece> {
    let (x, y) = match *self.size.as_slice() {
      [s,] => (s,s),
      [x, y] => (x,y),
      _ => throw!(SpecError::ImproperSizeSpec),
    };
    let outline = Box::new(shapelib::Square { xy: [x as f64, y as f64] });
    let path = svg_rectangle_path([x as f64, y as f64])?;
    let itemname = self.itemname.clone()
      .unwrap_or_else(||"simple-square".to_string());
    SimpleShape::new_from_path(Html::lit("square"), path,
                               &self.faces, outline, itemname)?
  }
} 
