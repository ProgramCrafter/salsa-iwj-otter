// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

use slotmap::secondary;

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
  pieces: BTreeSet<PieceId>, // kept in synch with PieceOccult::passive
  #[serde(flatten)] views: OccultationViews,
}

#[derive(Clone,Debug,Serialize,Deserialize)]
pub struct OccultationViews {
  views: Vec<OccultView>,
  #[serde(default)] defview: OccultationKind,
}

#[derive(Clone,Debug,Serialize,Deserialize)]
pub struct OccultView {
  #[serde(default)] occult: OccultationKind,
  players: Vec<PlayerId>,
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

impl OccultationViews {
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

impl Occultation {
  pub fn get_kind(&self, player: PlayerId) -> &OccultationKind {
    self.views.get_kind(player)
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
  gpl: &mut GPlayer,
  piece: PieceId,
  pc: &GPiece,
) -> PieceRenderInstructions {
  let vpid = gpl.idmap.fwd_or_insert(piece);
  let angle = pc.angle;
  let occluded = PriOccluded::Visible; // xxx
  trace!("{} {:?} => {} angle={:?}",
         player, piece, vpid, angle);
  PieceRenderInstructions { vpid, occluded }
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
  gpl: &GPlayer,
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
fn recalculate_occultation_general<
  RD,                                                 // return data
  LD,                                                 // log data
  VF: FnOnce(LD) -> RD,                               // ret_vanilla
  LF: FnOnce(Html, Html, Option<&Html>) -> LD,        // log_callback
  RF: FnOnce(PieceUpdateOps_PerPlayer, LD) -> RD,     // ret_callback
>(
  gplayers: &GPlayers,
  gpieces: &mut GPieces,
  goccults: &mut GameOccults,
  ipieces: &IPieces,
  piece: PieceId,
  // if no change, we return ret_vanilla(log_visible)
  log_visible: LD,
  ret_vanilla: VF,
  // otherwise we maybe call log_callback(who_by, old, new, desc)
  // or maybe we just use log_visible
  log_callback: LF,
  // and then call ret_callback(<calculated>, <logmsgs>)
  ret_callback: RF,
)
  -> RD
{
  // fallible part
  let (puos, log, occids): (_, _, OldNew<Option<OccId>>) = {
    let nopiece = || internal_logic_error("piece vanished");
    let ipc = ipieces.get(piece).ok_or_else(nopiece)?;
    let gpc = gpieces.get(piece).ok_or_else(nopiece)?;

    #[derive(Debug)]
    struct Occulted<'o> { occid: OccId, occ: &'o Occultation }

    let occulteds: OldNew<Option<Occulted>> = [
      gpc.occult.passive.map(|occid| Ok::<_,IE>(
        Occulted {
          occid,
          occ: goccults.occults.get(occid).ok_or_else(
            || internal_logic_error("uccultation vanished"))?,
        }
      )).transpose()?,
      goccults.occults.iter().find_map(|(occid, occ)| {
        dbg!(if gpc.occult.active.is_some() { // xxx remove dbg!
          // prevent occulting pieces being occulted
          // (also prevents reflexive occultation)
          return None
        } else if occ.in_region(gpc.pos) {
          Some(Occulted { occid, occ })
        } else {
          None
        })
      }),
    ].into();

    let occids = occulteds.map(|h| h.as_ref().map(|occ| occ.occid));
    if occids.old() == occids.new() { return ret_vanilla(log_visible); }

  /*
    #[throws(IE)]
    fn get_kind(gs: &GameState, occid: Option<OccultationId>, player: PlayerId)
                -> Option<(OccultationId, OccultationKind)> {
    };*/

    let mut situations: HashMap<
        OldNew<&OccultationKind>,
        Vec<PlayerId>,
      > = default();
    for (player, _gpl) in gplayers {
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
      let opiece = h.occ.occulter;
      let bad = || internal_error_bydebug(&("missing", opiece, h.occid));
      let oipc = ipieces.get(opiece).ok_or_else(bad)?;
      let ogpc = gpieces.get(opiece).ok_or_else(bad)?;
      Ok::<_,IE>(oipc.describe_html(ogpc)?)
    };

    let most_obscure = most_obscure.unwrap_or(&OccK::Visible); // no players!

    let call_log_callback =
      |show| Ok::<_,IE>(
        log_callback(describe_occulter(ONI::Old)?,
                     describe_occulter(ONI::New)?,
                     show)
      );

    let log = match most_obscure {
      OccK::Visible => {
        log_visible
      }
      OccK::Scrambled | OccK::Displaced{..} => {
        let _face = ipc.nfaces() - 1; // xxx use other thing entirely
        let show = ipc.describe_html(gpc)?;
        call_log_callback(Some(&show))?
      },
      OccK::Invisible => {
        call_log_callback(None)?
      },
    };

    let occids = occulteds.map(|h| h.as_ref().map(|h| h.occid));
    
    (puos, log, occids)
  };
  
  // point of no return

  // xxx shuffle some players' ids
  // xxx and/or shuffle locations

  (||{
    let mut update_pieces = |oni, upd: &dyn Fn(&mut _)| {
      if let Some(occid) = occids[oni] {
        upd(&mut goccults.occults.get_mut(occid).unwrap().pieces);
      }
    };
    update_pieces(ONI::Old, &|opcs|{ opcs.remove(&piece); });
    update_pieces(ONI::New, &|opcs|{ opcs.insert(piece); });
    gpieces.byid_mut(piece).unwrap().occult.passive = *occids.new();
  })(); // <- no ?, infallible commitment

  ret_callback(puos, log)
}

#[throws(InternalError)]
pub fn recalculate_occultation_piece(
  gs: &mut GameState,
  who_by: Html,
  ipieces: &IPieces,
  piece: PieceId,
  (vanilla_wrc, vanilla_op, vanilla_log): PUFOS,
)
  -> PieceUpdate
{
  recalculate_occultation_general(
    &gs.players, &mut gs.pieces, &mut gs.occults, ipieces,
    piece, vanilla_log,
    |log| (vanilla_wrc, vanilla_op, log).into(),
    |old, new, show| vec![ LogEntry { html: Html(format!(
      "{} moved {} from {} to {}",
      &who_by.0,
      if let Some(show) = show { &show.0 } else { "something" },
      &old.0, &new.0,
    ))}],
    |puos, log| PieceUpdate {
      wrc: WRC::Unpredictable,
      ops: PieceUpdateOps::PerPlayer(puos),
      log
    }
  )?
}

#[throws(IE)]
fn recalculate_occultation_ofmany(
  gplayers: &GPlayers,
  gpieces: &mut GPieces,
  goccults: &mut GameOccults,
  ipieces: &IPieces,
  ppiece: PieceId,
  updates: &mut Vec<(PieceId, PieceUpdateOps)>,
){
  recalculate_occultation_general(
    gplayers, gpieces, goccults, ipieces,
    ppiece,
    (), |_|(),
    |_,_,_|(), |puo_pp, ()|{
      updates.push((ppiece, PUOs::PerPlayer(puo_pp)));
    },
  )?;
}


#[must_use]
pub struct NascentOccultation(Occultation);

#[derive(Debug,Copy,Clone)]
pub struct UniformOccultationView(
  pub OccultationKind
);
#[derive(Debug,Copy,Clone)]
pub struct OwnerOccultationView {
  pub defview: OccultationKind,
  pub owner: PlayerId,
  pub owner_view: OccultationKind,
}

pub trait OccultationViewDef {
  fn views(self) -> Result<OccultationViews, IE>;
}
impl OccultationViewDef for UniformOccultationView {
  #[throws(IE)]
  fn views(self) -> OccultationViews { OccultationViews {
    defview: self.0,
    views: vec![]
  } }
}
impl OccultationViewDef for OwnerOccultationView {
  #[throws(IE)]
  fn views(self) -> OccultationViews { OccultationViews {
    defview: self.defview,
    views: vec![OccultView {
      players: vec![self.owner],
      occult: self.owner_view,
    }]
  } }
}

#[throws(OnlineError)]
pub fn create_occultation(
  gplayers: &GPlayers,
  gpieces: &mut GPieces,
  goccults: &mut GameOccults,
  ipieces: &IPieces,
  region: Area,
  occulter: PieceId,
  views: OccultationViews,
) -> Vec<(PieceId, PieceUpdateOps)> {
  {
    let ogpc = gpieces.byid(occulter)?;
    if ogpc.occult.active.is_some() {
      throw!(internal_logic_error("re-occulting!"))
    }
  }

  for occ in goccults.occults.values() {
    if occ.region.overlaps(&region) { throw!(OE::OverlappingOccultation) }
  }

  let mut recalc = vec![];
  for (ppiece, pgpc) in gpieces.iter() {
    if ! region.contains(pgpc.pos) { continue }
    if pgpc.occult.passive.is_some() { throw!(internal_logic_error(
      format!("piece {:?} in region, no occulters, but occulted", &pgpc)
    )) }
    recalc.push(ppiece);
  }

  let occultation = Occultation {
    region,
    occulter,
    views,
    pieces: default(),
  };
  debug!("creating occultation {:?}", &occultation);

  // Everything from here on must be undone if we get an error
  // but we hope not to get one...

  let occid = goccults.occults.insert(occultation);
  let mut updates = vec![];
  (||{
    let ogpc = gpieces.get_mut(occulter).ok_or_else(
      ||internal_logic_error("occulter vanished"))?;
    ogpc.occult.active = Some(occid);

    for &ppiece in &recalc {
      recalculate_occultation_ofmany(gplayers, gpieces, goccults, ipieces,
                                     ppiece, &mut updates)?;
    }

    Ok::<_,IE>(())
  })().map_err(|e| {
    for &ppiece in &recalc {
      let pgpc = gpieces.get_mut(ppiece).expect("had ppiece earlier");
      pgpc.occult.passive = None;
    }
    let ogpc = gpieces.get_mut(occulter).expect("had occulter earlier");
    ogpc.occult.active = None;
    goccults.occults.remove(occid).expect("inserted this earlier");
    e
  })?;

  updates
}

#[throws(IE)]
pub fn remove_occultation(
  gplayers: &GPlayers,
  gpieces: &mut GPieces,
  goccults: &mut GameOccults,
  ipieces: &IPieces,
  occulter: PieceId,
) -> Vec<(PieceId, PieceUpdateOps)> {
  let mut aggerr = AggregatedIE::new();

  let occid = if_chain! {
    if let Some(ogpc) = gpieces.get(occulter).or_else(||{
      aggerr.record(internal_logic_error(
        "removing occultation by no piece"));
      None
    });

    if let Some(occid) = ogpc.occult.active.or_else(||{
      aggerr.record(internal_logic_error(
        "removing occultation by non-active piece"));
      None
    });

    then { occid }
    else { aggerr.ok()?; panic!(); }
  };

  let occultation = aggerr.handle(
    goccults.occults.remove(occid).ok_or_else(
      || internal_logic_error("removing nonexistent occultation"))
  );
  debug!("removing occultation {:?}", &occultation);
      
  let mut updates = vec![];

  let pieces_fallback_buf;
  let pieces = if let Some(o) = &occultation { &o.pieces } else {
    pieces_fallback_buf = gpieces
      .iter()
      .filter_map(|(ppiece, pgpc)| {
        if pgpc.occult.passive == Some(occid) { Some(ppiece) }
        else { None }
      })
      .collect();
    &pieces_fallback_buf
  };
  
  for &ppiece in pieces.iter() {
    recalculate_occultation_ofmany(gplayers, gpieces, goccults, ipieces,
                                   ppiece, &mut updates)
      .unwrap_or_else(|e| {
        aggerr.record(e);
        if let Some(pgpc) = gpieces.get_mut(ppiece) {
          pgpc.occult.passive = None;
        }
      });
  }

  if let Some(ogpc) = gpieces.get_mut(occulter) {
    ogpc.occult.active = None;
  } else {
    aggerr.record(internal_logic_error("removing occultation of non-piece"));
  }

  aggerr.ok()?;

  updates
}
