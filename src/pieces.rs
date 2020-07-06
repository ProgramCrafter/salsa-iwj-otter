
use crate::imports::*;

define_index_type! {
  pub struct FaceId = u8;
}

#[derive(Debug)]
struct SimpleShape {
  desc : String,
  shape : String,
  colours : IndexVec<FaceId,Colour>,
}

const SELECT_SCALE : f64 = 1.1;

impl Piece for SimpleShape {
  fn svg_piece(&self, pri : &PieceRenderInstructions) -> String {
    format!(r##"<use fill="{}" href="#{}"/>"##,
            self.colours[pri.face],
            pri.id_x("base"))
  }
  fn svg_select(&self, pri : &PieceRenderInstructions) -> String {
    format!(r##"<g transform="scale({})"><use href="#{}"/></g>"##,
            SELECT_SCALE,
            pri.id_x("base"))
  }
  fn svg_x_ids(&self) -> VisiblePieceIdSvgIds { &["base"] }
  fn svg_x_defs(&self, pri : &PieceRenderInstructions) -> String {
    format!(r#"<g id={}>{}</g>"#, pri.id_x("base"), self.shape)
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
       shape : r#"<circle cx="0" cy="0" r="10"/>"#.to_owned(),
       colours : index_vec![ "red".to_string(), "grey".to_string() ],
     })),
    ([ 90, 60 ],
     Box::new(SimpleShape {
       desc : "square".to_owned(),
       shape : r#"<rect x="-10" y="-10" width="20" height="20"/>"#.to_owned(),
       colours : index_vec![ "blue".to_string(), "grey".to_string() ],
     })),
  ]
}
