
struct Disc {
  colours : Vec<Colour>,
  size : Coord,
}

impl Piece for Disc {
  fn svg(&self, pr : &PiecedRecord) -> SvgData {
    format!(
      r#"<circle cs="{}" cy="{}" r="{}" style="fill: {};"/>"#,
      pr.pos[0], pr.pos[1], swlf.size, self.colour,
    ).into_bytes()
  }
}

fn xxx_testload_disc() -> Disc { Disc { colours : 
  
}
