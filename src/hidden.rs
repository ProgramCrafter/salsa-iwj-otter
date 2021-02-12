// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

use slotmap::secondary;

type OccK = OccultationKind;
type ONI = OldNewIndex;

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
  fn at_all_visible(&self) -> bool {
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

  pub fn in_region(&self, pos: Pos) -> bool {
    self.region.contains(pos)
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

// xxx prevent addpiece and removepiece in places that would be occulted
// xxx this means this only happens on ungrab I think ?

#[throws(InternalError)]
pub fn recalculate_occultation(
  gs: &mut GameState,
  who_by: Html,
  ipieces: &PiecesLoaded,
  piece: PieceId,
  vanilla: PUFOS,
)
  -> PieceUpdate
{
  // fallible part
  let (update, occids): (_, OldNew<Option<OccId>>) = {
    let nopiece = || internal_logic_error("piece vanished");
    let ipc = ipieces.get(piece).ok_or_else(nopiece)?;
    let gpc = gs.pieces.get(piece).ok_or_else(nopiece)?;
    struct Occulted<'o> { occid: OccId, occ: &'o Occultation }

    let occulteds: OldNew<Option<Occulted>> = [
      gpc.occult.passive.map(|occid| Ok::<_,IE>(
        Occulted {
          occid,
          occ: gs.occults.occults.get(occid).ok_or_else(
            || internal_logic_error("uccultation vanished"))?,
        }
      )).transpose()?,
      gs.occults.occults.iter().find_map(|(occid, occ)| {
        if gpc.occult.active.is_some() {
          // prevent occulting pieces being occulted
          // (also prevents reflexive occultation)
          return None
        } else if occ.in_region(gpc.pos) {
          Some(Occulted { occid, occ })
        } else {
          None
        }
      }),
    ].into();

    let occids = occulteds.map(|h| h.as_ref().map(|occ| occ.occid));
    if occids.old() == occids.new() { return vanilla.into(); }

  /*
    #[throws(IE)]
    fn get_kind(gs: &GameState, occid: Option<OccultationId>, player: PlayerId)
                -> Option<(OccultationId, OccultationKind)> {
    };*/

    let mut situations: HashMap<
        OldNew<&OccultationKind>,
        Vec<PlayerId>,
      > = default();
    for (player, _gpl) in &gs.players {
      situations
        .entry(
          occulteds.as_refs().map(|occulted| {
            if let Some(occulted) = occulted {
              occulted.occ.get_kind(player)
            } else {
              &OccK::Visible
            }
          }))
        .or_default()
        .push(player);
    }

    let mut puos = SecondarySlotMap::new();
    let mut most_obscure = None;

    for (kinds, players) in &situations {
      // For each player, the message obscuration is the least obscure.
      // Then the overall obscuration is that from most afflicted player.
      most_obscure = cmp::max(
        most_obscure,
        kinds.iter().map(Deref::deref).min()
      );

      let puo = match (
        kinds.old().at_all_visible(),
        kinds.new().at_all_visible(),
      ) {
        (false, false) => None,
        (false, true ) => Some(PUO::Insert(())),
        (true,  false) => Some(PUO::Delete()),
        (true,  true ) => Some(PUO::Modify(())),
      };

      if let Some(puo) = puo {
        for player in players {
          puos.insert(*player, puo);
        }
      }
    }

    let describe_occulter = |oni| {
      let h = occulteds[oni].as_ref().ok_or_else(
        || internal_logic_error("most obscure not obscure"))?;
      let piece = h.occ.occulter;
      let ipc = ipieces.get(h.occ.occulter).ok_or_else(
        || internal_logic_error(
          format!("missing occulter piece {:?} for occid {:?}",
                  piece, h.occid)
        ))?;
      Ok::<_,IE>(ipc.describe_html(None))
    };

    let most_obscure = most_obscure.unwrap_or(&OccK::Visible); // no players!

    let log = match most_obscure {
      OccK::Visible => {
        vanilla.2
      }
      OccK::Scrambled | OccK::Displaced{..} => {
        let face = ipc.nfaces() - 1;
        let show = ipc.describe_html(Some(face.into()));
        vec![ LogEntry { html: Html(format!(
          "{} moved {} from {} to {}",
          who_by.0, &show.0,
          describe_occulter(ONI::Old)?.0,
          describe_occulter(ONI::New)?.0,
        ))}]
      },
      OccK::Invisible => vec![ LogEntry { html: Html(format!(
        "{} moved something from {} to {}",
        who_by.0,
        describe_occulter(ONI::Old)?.0,
        describe_occulter(ONI::New)?.0,
      ))}],
    };

    let update = PieceUpdate {
      wrc: WRC::Unpredictable,
      ops: PieceUpdateOps::PerPlayer(puos),
      log,
    };

    let occids = occulteds.map(|h| h.as_ref().map(|h| h.occid));
    
    (update, occids)
  };
  
  // point of no return

  // xxx shuffle some players' ids
  // xxx and/or shuffle locations

  (||{
    let mut update_pieces = |oni, upd: &dyn Fn(&mut _)| {
      if let Some(occid) = occids[oni] {
        upd(&mut gs.occults.occults.get_mut(occid).unwrap().pieces);
      }
    };
    update_pieces(ONI::Old, &|opcs|{ opcs.remove(&piece); });
    update_pieces(ONI::New, &|opcs|{ opcs.insert(piece); });
    gs.pieces.byid_mut(piece).unwrap().occult.passive = *occids.new();
  })(); // <- no ?, infallible commitment

  update
}
