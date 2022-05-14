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


  $( use crate::shapelib::[< $Shape Outline >]; )*

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
