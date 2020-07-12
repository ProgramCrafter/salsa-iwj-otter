
use crate::imports::*;

define_index_type! {
  pub struct FaceId = u8;
}

type ColourMap = IndexVec<FaceId,Colour>;

#[derive(Debug)]
struct SimpleShape {
  desc : String,
  path : String,
  scaled_path : String,
  approx_dia : Coord,
  colours : ColourMap,
}

const SELECT_SCALE : f64 = 1.1;


#[derive(Copy,Clone,Debug,Error)]
pub enum SVGProcessError {
  UnknownOperator,
  BadNumber,
  WriteFail,
}
display_as_debug!{SVGProcessError}
error_from_losedetails!{SVGProcessError, WriteFail, fmt::Error}
error_from_losedetails!{SVGProcessError, BadNumber, std::num::ParseFloatError}

#[throws(SVGProcessError)]
pub fn svg_rescale_path(input: &str, scale: f64) -> String {
  use SVGProcessError::*;

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
      _ => Err(UnknownOperator)?,
    };
    write!(&mut out, "{}", w)?;
  }

eprintln!("rescaled by {}: {} as {}",scale,&input,&out);
  out
}

impl Piece for SimpleShape {
  fn svg_piece(&self, pri : &PieceRenderInstructions) -> String {
    format!(r##"<use fill="{}" href="#{}"/>"##,
            self.colours[pri.face],
            pri.id_x("base"))
  }
  fn thresh_dragraise(&self, _pri : &PieceRenderInstructions)
                      -> Option<Coord> {
    Some(self.approx_dia / 2)
  }
  fn svg_select(&self, pri : &PieceRenderInstructions) -> String {
    format!(r##"<g transform="scale({})"><use href="#{}"/></g>"##,
            SELECT_SCALE,
            pri.id_x("base"))
  }
  fn svg_x_ids(&self) -> VisiblePieceIdSvgIds { &["base"] }
  fn svg_x_defs(&self, pri : &PieceRenderInstructions) -> String {
    format!(r#"<path id={} d="{}"></path>"#, pri.id_x("base"), self.path)
  }
  fn describe_html(&self, face : Option<FaceId>) -> String {
    if let Some(face) = face {
      format!("a {} {}", self.colours[face], self.desc)
    } else {
      format!("a {}", self.desc)
    }
  }
}

impl SimpleShape {
  #[throws(SVGProcessError)]
  fn new_from_path(desc: String, path: String, approx_dia: Coord,
                   colours: ColourMap) -> Self {
    SimpleShape {
      scaled_path : svg_rescale_path(&path, SELECT_SCALE)?,
      desc, approx_dia, path, colours,
    }
  }
  #[throws(SVGProcessError)]
  fn new_circle(dia: Coord, colours: ColourMap) -> Self {
    let unit_path =
      "M 0 1  a 1 1 0 1 0 0 -2 \
              a 1 1 0 1 0 0  2  z";
    let scale = (dia as f64) * 0.5;
    let path = svg_rescale_path(&unit_path, scale)?;
    Self::new_from_path("circle".to_owned(), path, dia, colours)?
  }
  #[throws(SVGProcessError)]
  fn new_square(edgelen: Coord, colours: ColourMap) -> Self {
    let unit_path =
      "M -1 -1 h 2 v 2 h -2 z";
    let scale = (edgelen as f64) * 0.5;
    let path = svg_rescale_path(&unit_path, scale)?;
    Self::new_from_path("square".to_owned(), path, edgelen, colours)?
  }
}

pub fn xxx_make_pieces() -> Result<Vec<(Pos, Box<dyn Piece>)>,SVGProcessError> {
  Ok(vec![
    ([ 90, 80 ],
     Box::new(SimpleShape::new_circle(
       20,
       index_vec![ "red".to_string(), "grey".to_string() ],
     )?)),
    ([ 90, 60 ],
     Box::new(SimpleShape::new_square(
       20,
       index_vec![ "blue".to_string(), "grey".to_string() ],
     )?)),
  ])
}
