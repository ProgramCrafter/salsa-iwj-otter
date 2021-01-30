// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

// this ios going to be replaced with new "hidden" machiner
//
// For now, we are firstly removing all calls to everything except
// new_hidden_todo.
//
// Then we'll adjust all call sites of new_hidden_todo too and Lens
// can be abolished.

pub trait Lens : Debug {
  fn new_hidden_todo(&self, why: &'static str);
  /// Will be replaced by some call to an Occlusion
  fn new_hidden_pri(&self, id: VisiblePieceId,
                    angle: VisiblePieceAngle, face: FaceId)
                    -> PieceRenderInstructions;

  fn pieceid2visible(&self, piece: PieceId) -> VisiblePieceId;
  fn log_pri(&self, piece: PieceId, pc: &PieceState)
             -> PieceRenderInstructions;
  fn svg_pri(&self, piece: PieceId, pc: &PieceState, player: PlayerId)
             -> PieceRenderInstructions;
  fn massage_prep_piecestate(&self, ns : &mut PreparedPieceState);
  fn decode_visible_pieceid(&self, vpiece: VisiblePieceId, player: PlayerId)
                            -> PieceId;
}
#[derive(Debug)]
pub struct TransparentLens {
}
impl Lens for TransparentLens {
  fn new_hidden_todo(&self, _why: &'static str) { }
  fn new_hidden_pri(&self, id: VisiblePieceId,
                    angle: VisiblePieceAngle, face: FaceId)
                    -> PieceRenderInstructions {
    PieceRenderInstructions { id, angle, face }
  }

  fn pieceid2visible(&self, piece: PieceId) -> VisiblePieceId {
    let kd : slotmap::KeyData = piece.data();
    VisiblePieceId(kd)
  }
  fn log_pri(&self, piece: PieceId, pc: &PieceState)
             -> PieceRenderInstructions {
    let id = self.pieceid2visible(piece);
    let angle = make_angle_visible(pc.angle);
    PieceRenderInstructions { id, angle, face: pc.face }
  }
  fn svg_pri(&self, piece: PieceId, pc: &PieceState, _player: PlayerId)
             -> PieceRenderInstructions {
    self.log_pri(piece, pc)
  }
  fn decode_visible_pieceid(&self, vpiece: VisiblePieceId, _player: PlayerId)
                            -> PieceId {
    let kd : slotmap::KeyData = vpiece.into();
    PieceId::from(kd)
  }
  fn massage_prep_piecestate(&self, _ns : &mut PreparedPieceState) { }
}

