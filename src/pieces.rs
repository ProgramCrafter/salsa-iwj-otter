
use crate::imports::*;

define_index_type! {
  pub struct FaceId = u8;
}

#[derive(Debug)]
struct SimpleShape {
  desc : String,
  path : String,
  approx_dia : Coord,
  colours : IndexVec<FaceId,Colour>,
}

const SELECT_SCALE : f64 = 1.1;

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

pub fn xxx_make_pieces() -> Vec<(Pos, Box<dyn Piece>)> {
  vec![
    ([ 90, 80 ],
     Box::new(SimpleShape {
       desc : "circle".to_owned(),
       approx_dia : 20,
       path : "M 10 0  a 10 10 0 1 0 -20 0\
                       a 10 10 0 1 0  20 0".to_owned(),
       colours : index_vec![ "red".to_string(), "grey".to_string() ],
     })),
    ([ 90, 60 ],
     Box::new(SimpleShape {
       desc : "square".to_owned(),
       approx_dia : 20,
       path : "M -10 -10 h 20 v 20 h -20 v -20".to_owned(),
       colours : index_vec![ "blue".to_string(), "grey".to_string() ],
     })),
  ]
}
