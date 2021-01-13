// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

pub trait Lens : Debug {
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
  // when lenses become nontrivial, make this nonconstructable
  // to find all the places where a TransparentLens was bodged
}
impl Lens for TransparentLens {
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

