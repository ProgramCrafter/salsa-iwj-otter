// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

//================ principal definitions of both shapes ================

macro_rules! shape_defns { {
  $( $Shape:ident  $serde:literal  ;)*
} => { paste!{


  #[dyn_upcast(OutlineTrait)]
  #[enum_dispatch(OutlineTrait)]
  #[derive(Clone,Debug,Serialize,Deserialize)]
  #[serde(tag="type")]
  pub enum Outline { $(
    #[serde(rename=$serde)] [< $Shape Outline >],
  )* }


  #[derive(Deserialize,Debug,Copy,Clone,Eq,PartialEq)]
  pub enum Shape { $(
    #[serde(rename=$serde)] [< $Shape >],
  )* }


  $(
  #[derive(Deserialize,Debug)]
  pub struct [< $Shape ShapeIndicator >];
  )*


  impl Shape {
    pub fn shapelib_loadable(self)
        -> &'static dyn shapelib::ShapeLoadableTrait
    {
      match self { $(
        Self::$Shape => &[< $Shape ShapeIndicator >] as _,
      )* }
    }
  }

} } }

shape_defns! {
  Circle "Circle";
  Rect   "Rect"  ;
}


//---------- Circle ----------

#[derive(Clone,Copy,Debug,Serialize,Deserialize)]
pub struct CircleOutline { pub diam: f64 }

#[dyn_upcast]
impl OutlineTrait for CircleOutline {
  #[throws(IE)]
  fn outline_path(&self, scale: f64) -> Html {
    svg_circle_path(self.diam * scale)?
  }
  #[throws(IE)]
  fn thresh_dragraise(&self) -> Option<Coord> {
    Some((self.diam * 0.5) as Coord)
  }
  #[throws(IE)]
  fn bbox_approx(&self) -> Rect {
    let d = (self.diam * 0.5).round() as Coord;
    Rect{ corners: [PosC::new(-d,-d), PosC::new(d, d)]}
  }
}

//---------- RectOutline ----------

#[derive(Clone,Copy,Debug,Serialize,Deserialize)]
pub struct RectOutline { pub xy: PosC<f64> }

impl RectOutline {
  // Used by code elsewhere eg deck.rs for occultation boundaries etc.
  #[throws(CoordinateOverflow)]
  pub fn rect(&self, nominal: Pos) -> RectC<Coord> {
    let offset = (self.xy * 0.5)?;
    let offset = offset.try_map(
      |c| c.floor().to_i32().ok_or(CoordinateOverflow)
    )?;
    let rect = RectC{ corners:
      [-1,1].iter().map(|&signum| Ok::<_,CoordinateOverflow>({
        (nominal + (offset * signum)?)?
      }))
        .collect::<Result<ArrayVec<_,2>,_>>()?
        .into_inner().unwrap()
    };
    rect
  }

  #[throws(CoordinateOverflow)]
  pub fn region(&self, nominal: Pos) -> Region {
    Region::Rect(self.rect(nominal)?)
  }
}

#[dyn_upcast]
impl OutlineTrait for RectOutline {
  #[throws(IE)]
  fn outline_path(&self, scale: f64) -> Html {
    let xy = (self.xy * scale)?;
    svg_rectangle_path(xy)?
  }
  #[throws(IE)]
  fn thresh_dragraise(&self) -> Option<Coord> {
    let smallest: f64 = self.xy.coords.iter().cloned()
      .map(OrderedFloat::from).min().unwrap().into();
    Some((smallest * 0.5) as Coord)
  }
  #[throws(IE)]
  fn bbox_approx(&self) -> Rect {
    let pos: Pos = self.xy.map(
      |v| ((v * 0.5).round()) as Coord
    );
    let neg = (-pos)?;
    Rect{ corners: [ neg, pos ] }
  }
}
