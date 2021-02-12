// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

use slotmap::secondary;

type OccK = OccultationKind;

visible_slotmap_key!{ OccId(b'H') }

// ========== data structures ==========

#[derive(Clone,Debug,Default,Serialize,Deserialize)]
pub struct GameOccults {
  occults: DenseSlotMap<OccId, Occultation>,
}

#[derive(Clone,Debug,Default,Serialize,Deserialize)]
// kept in synch with Occultation::pieces
pub struct PieceOccult {
  active: Option<OccId>, // kept in synch with Occultation::occulter
  passive: Option<OccId>, // kept in synch with Occultation::pieces
}

#[derive(Clone,Debug,Serialize,Deserialize)]
pub struct Occultation {
  region: Area, // automatically affect pieces here
  occulter: PieceId, // kept in synch with PieceOccult::active
  views: Vec<OccultView>,
  #[serde(default)] defview: OccultationKind,
  pieces: BTreeSet<PieceId>, // kept in synch with PieceOccult::passive
}

#[derive(Clone,Debug,Serialize,Deserialize)]
pub struct OccultView {
  players: Vec<PlayerId>,
  #[serde(default)] occult: OccultationKind,
}

#[derive(Clone,Copy,Debug,Serialize,Deserialize)]
#[derive(Eq,PartialEq,Hash)]
pub enum OccultationKind {
  Visible,
  Scrambled,
  Displaced { within: Area },
  Invisible,
}

impl Default for OccultationKind {
  fn default() -> Self { OccK::Visible }
}

impl Ord for OccultationKind {
  fn cmp(&self, rhs: &Self) -> Ordering {
    fn level(k: &OccK) -> u8 { use OccultationKind::*; match k {
      Visible       => 0,
      Scrambled     => 1,
      Displaced{..} => 2,
      Invisible     => 3,
    } }
    level(self).cmp(&level(rhs))
  }
}
impl PartialOrd for OccultationKind {
  fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
    Some(self.cmp(rhs))
  }
}

impl OccultationKind {
  fn _at_all_visible(&self) -> bool {
    match self {
      OccK::Visible |
      OccK::Scrambled |
      OccK::Displaced { .. }
        => false,
      OccK::Invisible
        => true,
    }
  }
}

impl Occultation {
  pub fn get_kind(&self, player: PlayerId) -> &OccultationKind {
    let kind = self.views.iter().find_map(|view| {
      if view.players.contains(&player) { return Some(&view.occult); }
      None
    }).unwrap_or(
      &self.defview
    );
    kind
  }
}

impl GameOccults {
  #[throws(IE)]
  fn by_id(&self, occid: OccId) -> &Occultation {
    self.occults.get(occid).ok_or_else(
      || internal_logic_error("piece missing"))?
  }

  #[throws(IE)]
  pub fn get_kind(&self, occid: OccId, player: PlayerId) -> &OccultationKind {
    let occ = self.by_id(occid)?;
    let kind = occ.get_kind(player);
    kind
  }
}

// ========== PerPlayerIdMap ==========

#[derive(Clone,Debug,Default,Serialize,Deserialize)]
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

  fn fwd_or_insert_internal<R, VF, OF>
    (&mut self, piece: PieceId, vf: VF, of: OF) -> R
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
    self.fwd_or_insert_internal(piece, |_vis|(), |vis|{
      panic!("duplicate insert of {:?} {:?}", piece, vis)
    })
  }

  pub fn fwd_or_insert(&mut self, piece: PieceId) -> VisiblePieceId {
    self.fwd_or_insert_internal(piece, |vis|vis, |occ| *occ.get())
  }
}

// ========== public entrypoints ==========

pub fn piece_pri(
  _occults: &GameOccults, // xxx
  player: PlayerId,
  gpl: &mut GPlayerState,
  piece: PieceId,
  pc: &PieceState,
) -> PieceRenderInstructions {
  let vpiece = gpl.idmap.fwd_or_insert(piece);
  let angle = VisiblePieceAngle(pc.angle);
  let face = pc.face;
  trace!("{} {:?} => {} face={:?} angle={:?}",
         player, piece, vpiece, face, angle);
  PieceRenderInstructions { id: vpiece, angle, face }
}

pub fn piece_at_all_occluded(
  _occults: &GameOccults, // xxx
  _piece: PieceId, // xxx
) -> bool {
  false
}

pub fn vpiece_decode(
  _gs: &GameState, // xxx
  player: PlayerId,
  gpl: &GPlayerState,
  vis: VisiblePieceId
) -> Option<PieceId> {
  let piece = gpl.idmap.rev(vis);
  // xxx check for occultation:
  // check that this piece is visible at all to this player,
  // or they might manipulate it despite not seeing it!
  trace!("{} {:?} <= {}", player, piece, vis);
  piece
}

pub fn massage_prep_piecestate(
  _pri: &PieceRenderInstructions, // xxx
  _ns: &mut PreparedPieceState, // xxx
) {
  // xxx hidden position involves adjusting pos and z and ??? here
}
