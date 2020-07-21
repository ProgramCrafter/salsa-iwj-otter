
use crate::imports::*;

type ColourMap = IndexVec<FaceId,Colour>;

#[derive(Debug,Serialize,Deserialize)]
struct SimpleShape {
  desc : String,
  path : String,
  scaled_path : String,
  approx_dia : Coord,
  colours : ColourMap,
}

const SELECT_SCALE : f64 = 1.1;

#[derive(Copy,Clone,Debug,Error)]
pub enum SVGProcessingError {
  UnknownOperator,
  BadNumber,
  WriteFail,
  NegativeDragraise,
}

display_as_debug!{SVGProcessingError}
error_from_losedetails!{SVGProcessingError,WriteFail,fmt::Error}
error_from_losedetails!{SVGProcessingError,BadNumber,std::num::ParseFloatError}

type SE = SVGProcessingError;

#[throws(SE)]
pub fn svg_rescale_path(input: &str, scale: f64) -> String {
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

  for w in input.split_ascii_whitespace() {
    if !first.next().is_some() { write!(&mut out, " ")?; }
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
      _ => Err(SE::UnknownOperator)?,
    };
    write!(&mut out, "{}", w)?;
  }

eprintln!("rescaled by {}: {} as {}",scale,&input,&out);
  out
}

#[typetag::serde]
impl Piece for SimpleShape {
  #[throws(SE)]
  fn svg_piece(&self, f: &mut String, pri: &PieceRenderInstructions) {
    write!(f, r##"<path fill="{}" d="{}"/>"##,
           self.colours[pri.face],
           &self.path)?;
  }
  #[throws(SE)]
  fn surround_path(&self, _pri : &PieceRenderInstructions) -> String {
    self.scaled_path.clone()
  }
  #[throws(SE)]
  fn thresh_dragraise(&self, _pri : &PieceRenderInstructions)
                      -> Option<Coord> {
    Some(self.approx_dia / 2)
  }
  #[throws(SE)]
  fn svg_x_defs(&self, _f: &mut String, _pri : &PieceRenderInstructions) {
  }
  #[throws(SE)]
  fn describe_html(&self, face : Option<FaceId>) -> String {
    if let Some(face) = face {
      format!("a {} {}", self.colours[face], self.desc)
    } else {
      format!("a {}", self.desc)
    }
  }
}

impl SimpleShape {
  fn new_from_path(desc: String, path: String, approx_dia: Coord,
                   colours: ColourMap) -> Result<Box<dyn Piece>,SE> {
    let scaled_path = svg_rescale_path(&path, SELECT_SCALE)?;
    Ok(Box::new(SimpleShape {
      scaled_path, desc, approx_dia, path, colours,
    }))
  }
  fn new_square(edgelen: Coord, colours: ColourMap) -> Result<Box<dyn Piece>,SE> {
    let unit_path =
      "M -1 -1 h 2 v 2 h -2 z";
    let scale = (edgelen as f64) * 0.5;
    let path = svg_rescale_path(&unit_path, scale)?;
    Ok(Self::new_from_path("square".to_owned(), path, edgelen, colours)?)
  }
}

#[derive(Deserialize)]
#[derive(Debug,Default)]
#[repr(transparent)]
struct ColourSpec(String);

impl TryFrom<ColourSpec> for Colour {
  type Error = SE;
  #[throws(SE)]
  fn try_from(spec: ColourSpec) -> Colour {
    // xxx check syntax
    spec.0
  }
}

#[derive(Debug,Deserialize)]
struct Disc {
  diam : Coord,
  faces : [ColourSpec; 2],
}

#[typetag::deserialize]
impl PieceSpec for Disc {
  #[throws(SE)]
  fn load(mut self) -> Box<dyn Piece> {
    let unit_path =
      "M 0 1  a 1 1 0 1 0 0 -2 \
              a 1 1 0 1 0 0  2  z";
    let scale = (self.diam as f64) * 0.5;
    let path = svg_rescale_path(&unit_path, scale)?;
    let colours = self.faces
      .iter_mut()
      .map(|s| mem::take(s).try_into())
      .collect::<Result<_,SE>>()?;
    SimpleShape::new_from_path("circle".to_owned(), path, self.diam,
                               colours)?
  }
}

pub fn xxx_make_pieces() -> Result<Vec<(Pos, Box<dyn Piece>)>,SE> {
  Ok(vec![
    ([ 90, 80 ],
     Disc {
       diam : 20,
       faces : [ ColourSpec("red".to_string()), ColourSpec("grey".to_string()) ]
     }.load()?),
    ([ 90, 60 ],
     SimpleShape::new_square(
       20,
       index_vec![ "blue".to_string(), "grey".to_string() ],
     )?),
  ])
}
