// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

use slotmap::secondary;

#[derive(Default,Debug,Clone,Serialize,Deserialize)]
pub struct PerPlayerIdMap {
  f: SecondarySlotMap<PieceId, VisiblePieceId>,
  r: DenseSlotMap<VisiblePieceId, PieceId>,
}

impl PerPlayerIdMap {
  pub fn fwd(&self, piece: PieceId) -> Option<VisiblePieceId> {
    Some(*self.f.get(piece)?)
  }
  pub fn rev(&self, vis: VisiblePieceId) -> Option<PieceId> {
    Some(*self.r.get(vis)?)
  }

  fn _fwd_or_insert<R, VF, OF>(&mut self, piece: PieceId, vf: VF, of: OF) -> R
  where VF: FnOnce(VisiblePieceId) -> R,
        OF: FnOnce(secondary::OccupiedEntry<PieceId,VisiblePieceId>) -> R,
  {
    match self.f.entry(piece).expect("stale PieceId !") {
      secondary::Entry::Vacant(mut vac) => {
        if let Some((_, stale_vis)) = vac.remove_stale_entry() {
          self.r.remove(stale_vis);
        }
        let vis = self.r.insert(piece);
        vac.insert(vis);
        vf(vis)
      }
      secondary::Entry::Occupied(occ) => {
        of(occ)
      }
    }
  }

  pub fn insert(&mut self, piece: PieceId) {
    self._fwd_or_insert(piece, |_vis|(), |vis|{
      panic!("duplicate insert of {:?} {:?}", piece, vis)
    })
  }

  pub fn fwd_or_insert(&mut self, piece: PieceId) -> VisiblePieceId {
    self._fwd_or_insert(piece, |vis|vis, |occ| *occ.get())
  }
}

#[derive(Clone,Debug,Default,Serialize,Deserialize)]
pub struct GameOccults {
  // todo
}

pub fn piece_pri(
  _occults: &GameOccults, // xxx
  player: PlayerId,
  gpl: &mut GPlayerState,
  piece: PieceId,
  pc: &PieceState,
) -> PieceRenderInstructions {
  let vpiece = gpl.idmap.fwd_or_insert(piece);
  let angle = make_angle_visible(pc.angle);
  let face = pc.face;
  trace!("{} {:?} => {} face={:?} angle={:?}",
         player, piece, vpiece, face, angle);
  PieceRenderInstructions { id: vpiece, angle, face }
}

pub fn vpiece_decode(
  _gs: &GameState, // xxx
  player: PlayerId,
  gpl: &GPlayerState,
  vis: VisiblePieceId
) -> Option<PieceId> {
  let piece = gpl.idmap.rev(vis)?;
  // xxx check for occultation:
  // check that this piece is visible at all to this player,
  // or they might manipulate it despite not seeing it!
  trace!("{} {:?} <= {}", player, piece, vis);
  Some(piece)
}

pub fn massage_prep_piecestate(
  _pri: &PieceRenderInstructions, // xxx
  _ns: &mut PreparedPieceState, // xxx
) {
  // xxx hidden position involves adjusting pos and z and ??? here
}
