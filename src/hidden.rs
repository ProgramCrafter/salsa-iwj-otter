// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;
use crate::prelude::*;

#[path="vpid.rs"] mod vpid;
pub use vpid::{PerPlayerIdMap, NotchNumber, Notch, Notches, consistency_check};

use slotmap::secondary;

type ONI = OldNewIndex;

visible_slotmap_key!{ OccId(b'H') }

// ========== data structures ==========

#[derive(Copy,Clone,Debug)]
/// Proof token.
///
/// Proof obligation when constructing.
pub struct ShowUnocculted(());

#[derive(Copy,Clone,Debug)]
pub struct OcculterRotationChecked(());

#[derive(Debug,Serialize,Deserialize)]
#[serde(transparent)]
pub struct IPieceTraitObj(Box<dyn PieceTrait>);

#[derive(Clone,Debug,Default,Serialize,Deserialize)]
pub struct GameOccults {
  occults: DenseSlotMap<OccId, Occultation>,
}

#[derive(Clone,Debug,Default,Serialize,Deserialize)]
// kept in synch with Occultation::pieces
pub struct PieceOccult {
  active: Option<OccId>, // kept in synch with Occultation::occulter
  passive: Option<Passive>, // kept in synch with Occultation::notches
}

#[derive(Clone,Copy,Debug,Serialize,Deserialize,Eq,PartialEq)]
struct Passive {
  occid: OccId,
  notch: Notch,
}

#[derive(Clone,Debug,Serialize,Deserialize)]
pub struct Occultation {
  region: Region, // automatically affect pieces here
  occulter: PieceId, // kept in synch with PieceOccult::active
  notches: Notches, // kept in synch with PieceOccult::passive
  ppiece_use_size: Pos, // taken from first piece
  #[serde(flatten)] views: OccultationViews,
}

#[derive(Clone,Debug,Serialize,Deserialize)]
pub struct OccultationViews {
  pub views: Vec<OccultView>,
  #[serde(default)] pub defview: OccultationKind,
}

#[derive(Clone,Debug,Serialize,Deserialize)]
pub struct OccultView {
  #[serde(default)] pub occult: OccultationKind,
  pub players: Vec<PlayerId>,
}

#[derive(Clone,Copy,Debug,Serialize,Deserialize)]
#[derive(Eq,PartialEq,Hash)]
pub enum OccultationKindGeneral<D> {
  Visible,
  Scrambled,
  Displaced(D),
  Invisible,
}
pub type OccultationKind = OccultationKindGeneral<(OccDisplacement,ZCoord)>;

#[derive(Clone,Debug)]
#[derive(Eq,PartialEq,Hash)]
pub enum OccultationKindAlwaysOk {
  Visible,
  // Scrambled is only allowed as the only view; enforced by our
  // OccultationViewDef trait impls
  Displaced((OccDisplacement,ZCoord)),
  Invisible,
}
impl From<OccultationKindAlwaysOk> for OccultationKind {
  fn from(i: OccultationKindAlwaysOk) -> OccultationKind {
    match i {
      OccKA::Visible      => OccKG::Visible,
      OccKA::Displaced(d) => OccKG::Displaced(d),
      OccKA::Invisible    => OccKG::Invisible,
    }
  }
}

#[derive(Clone,Debug,Serialize,Deserialize)]
#[derive(Eq,PartialEq,Hash)]
pub enum OccDisplacement {
  Stack {
    pos: Pos,
  },
  Rect {
    rect: Rect,
  },
}

impl PieceOccult {
  pub fn is_active(&self) -> bool { self.active.is_some() }

  #[throws(IE)]
  fn active_occ<'r>(&'r self, goccults: &'r GameOccults)
                    -> Option<&'r Occultation> {
    if let Some(occid) = self.active {
      let occ = goccults.occults.get(occid).ok_or_else(
        || internal_error_bydebug(&self))?;
      Some(occ)
    } else {
      None
    }
  }

  #[throws(IE)]
  pub fn active_views<'r>(&'r self, goccults: &'r GameOccults)
                          -> Option<&'r OccultationViews> {
    self.active_occ(goccults)?.map(
      |occ| &occ.views
    )
  }

  #[throws(IE)]
  pub fn active_region<'r>(&'r self, goccults: &'r GameOccults)
                           -> Option<&'r Region> {
    self.active_occ(goccults)?.map(
      |occ| &occ.region
    )
  }

  #[throws(IE)]
  pub fn active_total_ppieces(&self, goccults: &GameOccults)
                              -> Option<NotchNumber> {
    self.active_occ(goccults)?.map(
      |occ| occ.notches.len()
    )
  }

  pub fn passive_occid(&self) -> Option<OccId> { Some(self.passive?.occid) }
  pub fn passive_delete_hook(&self, goccults: &mut GameOccults,
                             piece: PieceId) {
    if_chain! {
      if let Some(Passive { occid, notch }) = self.passive;
      if let Some(occ) = goccults.occults.get_mut(occid);
      then {
        occ.notches.remove(piece, notch)
          .unwrap_or_else(|e| error!("removing occulted piece {:?}", e));
      }
    }
  }
}

impl Default for OccultationKind {
  fn default() -> Self { OccK::Visible }
}

impl Ord for OccultationKind {
  fn cmp(&self, rhs: &Self) -> Ordering {
    fn level(k: &OccK) -> u8 { use OccKG::*; match k {
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
        => true,
      OccK::Invisible
        => false,
    }
  }
}

impl<P,Z> OccultationKindGeneral<(P, Z)> {
  pub fn pri_occulted(self) -> Option<PriOccultedGeneral<P,Z>> {
    Some(match self {
      OccKG::Invisible          => return None,
      OccKG::Visible            => PriOG::Visible(ShowUnocculted(())),
      OccKG::Scrambled          => PriOG::Occulted,
      OccKG::Displaced((pos,z)) => PriOG::Displaced(pos, z),
    })
  }

}

impl<T> OccultationKindGeneral<T> {
  fn map_displaced<U,F>(&self, f: F) -> OccultationKindGeneral<U>
    where F: FnOnce(&T) -> U,
  {
    use OccKG::*;
    match self {
      Visible   => Visible,
      Scrambled => Scrambled,
      Invisible => Invisible,
      Displaced(t) => Displaced(f(t)),
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

  #[throws(IE)]
  pub fn pos_occulter(&self, goccults: &GameOccults, pos: Pos)
                      -> Option<PieceId> {
    goccults.occults.iter().find_map(|(_occid, occ)| {
      if occ.in_region(pos) {
        Some(occ.occulter)
      } else {
        None
      }
    })
  }

  pub fn is_empty(&self) -> bool {
    let GameOccults { occults } = self;
    occults.is_empty()
  }
}

// ========== public entrypoints ==========

/// None => do not render at all
pub fn piece_pri(
  _ioccults: &IOccults,
  occults: &GameOccults,
  player: PlayerId, gpl: &mut GPlayer,
  piece: PieceId, gpc: &GPiece, _ipc: &IPiece,
) -> Option<PieceRenderInstructions>
{
  let mut occk_dbg = None;
  let occulted = if_chain! {
    if let Some(Passive { occid, notch }) = gpc.occult.passive;
    if let Some(occ) = occults.occults.get(occid);
    if let Some(zg) = occ.notch_zg(notch);
    then {
      let occk = occ.views.get_kind(player)
        .map_displaced(|(displace, z)| {
          let notch: NotchNumber = notch.into();
          let pos = displace.place(occ.ppiece_use_size, notch);
          let z = z.plus_offset(notch)
            .unwrap_or_else(|e| { // eek!
              error!("z coordinate overflow ({:?}), bodging! {:?} {:?}",
                     e, piece, &z);
              z.clone()
            });
          (pos, ZLevel { z, zg })
        });

      occk_dbg = Some(occk.clone());
      match occk.pri_occulted() {
        Some(o) => o,
        None => {
          trace_dbg!("piece_pri", player, piece, occk_dbg, gpc);
          return None;
        }
      }
    }
    else {
      PriOG::Visible(ShowUnocculted(()))
    }
  };

  let vpid = gpl.idmap.fwd_or_insert(piece);
  trace_dbg!("piece_pri", player, piece, occk_dbg, vpid, occulted, gpc);
  Some(PieceRenderInstructions { vpid, occulted })
}

impl OccDisplacement {
  fn place(&self, ppiece_use_size: Pos, notch: NotchNumber) -> Pos {
    use OccDisplacement as OD;
    match self {
      OD::Stack{pos} => *pos,
      OD::Rect{rect} => (|| Some({
        let notch: Coord = notch.try_into().ok()?;
        let mut spare = ((rect.br() - rect.tl()).ok()?
                         - ppiece_use_size).ok()?;
        for s in &mut spare.coords { *s = max(*s,1) }
        let fi = 0;
        let gi = 1;
        let f_stride = max(ppiece_use_size.coords[fi] / 4, 1);
        let g_stride = max(ppiece_use_size.coords[gi] / 3, 1);
        let f_count = max(spare.coords[fi] / f_stride, 1);
        let g_count = max(spare.coords[gi] / g_stride, 1);
        let mut f_num = notch % f_count;
        let     g_num = notch / f_count;
        if g_num % 2 != 0 { f_num = f_count - 1 - f_num }
        let f_coord = rect.br().coords[fi] - ppiece_use_size.coords[fi] / 2 -
            f_stride * f_num;
        let g_coord = rect.tl().coords[gi] + ppiece_use_size.coords[gi] / 2 +
          if g_num < g_count {
            g_stride * g_num
          } else if g_num < spare.coords[gi] {
            g_num
          } else {
            spare.coords[gi] - 1
          };
        trace_dbg!("placement", spare,
                   f_stride, f_count, f_num, f_coord,
                   g_stride, g_count, g_num, g_coord);
        let mut pos = PosC::zero();
        pos.coords[fi] = f_coord;
        pos.coords[gi] = g_coord;
        pos
      }))().unwrap_or_else(||{
        rect.middle()
      })
    }
  }
}

impl ShowUnocculted {
  /// Override.  Proof obligation: this context does not require
  /// honouring occultation.
  pub const fn new_visible() -> ShowUnocculted {
    ShowUnocculted(())
  }
}

impl PieceRenderInstructions {
  /// Override.  Proof obligation: this context does not require
  /// honouring occultation.
  pub fn new_visible(vpid: VisiblePieceId) -> PieceRenderInstructions {
    PieceRenderInstructions {
      vpid,
      occulted: PriOcculted::Visible(ShowUnocculted(())),
    }
  }
}

impl IPieceTraitObj {
  pub fn new(p: Box<dyn PieceTrait>) -> Self { Self(p) }

  pub fn show(&self, _: ShowUnocculted) -> &dyn PieceTrait {
    &*self.0
  }

  pub fn into_inner(self) -> Box<dyn PieceTrait> { self.0 }

  pub fn direct_trait_access(&self) -> &dyn PieceTrait {
    &*self.0
  }
}

impl IPiece {
  #[throws(IE)]
  pub fn show_or_instead<'p>(&self, ioccults: &'p IOccults,
                         y: Option<ShowUnocculted>)
          -> Either<ShowUnocculted, /*occulted*/ &'p dyn InertPieceTrait> {
    match y {
      Some(y) => Left(y),
      None => Right({
        let occilk = self.occilk.as_ref()
          .ok_or_else(|| internal_logic_error(format!(
            "occulted non-occultable {:?}", self)))?;
        let occ_data = ioccults.ilks.get(occilk)
          .ok_or_else(|| internal_logic_error(format!(
            "occulted ilk vanished {:?} {:?}", self, occilk)))?;
        occ_data.p_occ.as_ref()
      }),
    }
  }
}

impl GPiece {
  pub fn fully_visible_to_everyone(&self) -> Option<ShowUnocculted> {
    match self.occult.passive {
      Some(_) => None,
      None => Some(ShowUnocculted(())),
    }
  }

  pub fn occulter_check_unrotated(&self, _:ShowUnocculted)
      -> Result<OcculterRotationChecked, Inapplicable> {
    if self.angle.is_rotated() { Err(Ia::OcculterAlreadyRotated) }
    else { Ok(OcculterRotationChecked(())) }
  }


  pub fn fully_visible_to(&self, goccults: &GameOccults, player: PlayerId)
                          -> Option<ShowUnocculted>
  {
    const HIDE: Option<ShowUnocculted> = None;
    const SHOW: Option<ShowUnocculted> = Some(ShowUnocculted(()));
    if_let!{ Some(passive) = &self.occult.passive; else return SHOW };
    want_let!{ Some(occ) = goccults.occults.get(passive.occid);
               else ?passive.occid; return HIDE };
    return match occ.views.get_kind(player) {
      OccK::Visible => SHOW,
      OccK::Scrambled |
      OccK::Displaced(_) |
      OccK::Invisible => HIDE,
    }
  }

  pub fn involved_in_occultation(&self) -> bool {
    self.occult.passive.is_some() ||
    self.occult.active.is_some()
  }

  #[throws(Ia)]
  pub fn forbid_involved_in_occultation(&self) {
    if self.involved_in_occultation() { throw!(Ia::Occultation) }
  }
}

pub fn vpiece_decode(
  gs: &GameState,
  player: PlayerId,
  gpl: &GPlayer,
  vis: VisiblePieceId
) -> Option<PieceId> {
  let piece: Option<PieceId> = gpl.idmap.rev(vis);
  let piece: Option<PieceId> = if_chain! {
    if let Some(p) = piece;
    if let Some(gpc) = gs.pieces.get(p);
    if let Some(Passive { occid, notch:_ }) = gpc.occult.passive;
    if let Some(occ) = gs.occults.occults.get(occid);
    let kind = occ.views.get_kind(player);
    if ! kind.at_all_visible();
    then { None }
    else { piece }
  };
  trace!("{} {:?} <= {}", player, piece, vis);
  piece
}

#[throws(InternalError)]
fn recalculate_occultation_general<
  RD: Debug,                                          // return data
  LD: Debug,                                          // log data
  VF: FnOnce() -> RD,                                 // ret_vanilla
  LF: FnOnce(Option<Html>, Option<Html>, &Html) -> LD, // log_callback
  RF: FnOnce(PieceUpdateOps_PerPlayer, LD) -> RD,     // ret_callback
>(
  gen: &mut UniqueGenGen,
  //
  gplayers: &GPlayers, gpieces: &mut GPieces,
  goccults: &mut GameOccults, ipieces: &IPieces, ioccults: &IOccults,
  //
  to_recalculate: &mut ToRecalculate, piece: PieceId,
  // if no change, we return ret_vanilla()
  ret_vanilla: VF,
  // otherwise we use log_invisible or log_callback(who_by,old,new,desc)
  log_invisible: LD,
  log_callback: LF,
  // and then call ret_callback(<calculated>, <logmsgs>)
  ret_callback: RF,
)
  -> RD
{
  #[derive(Debug,Copy,Clone)]
  struct OldNewOcculteds<O> {
    old: Option<(O, Notch)>,
    new: Option<O>,
  }
  impl<O> OldNewOcculteds<O> {
    fn main(&self) -> OldNew<Option<O>> where O:Copy {
      [ self.old.map(|(o,_n)| o), self.new ].into()
    }
    fn map<P, F:FnMut(O) -> P>(self, mut f: F) -> OldNewOcculteds<P> {
      OldNewOcculteds {
        old: self.old.map(|(o,n)| (f(o), n)),
        new: self.new.map(f),
      }
    }
    fn as_refs(&self) -> OldNewOcculteds<&O> {
      OldNewOcculteds {
        old: self.old.as_ref().map(|(o,n)| (o, *n)),
        new: self.new.as_ref()
      }
    }
  }

  let nopiece = || internal_logic_error("piece vanished");
  let ipc = ipieces.get(piece).ok_or_else(nopiece)?;

  // fallible part
  let (puos, log, occulteds): (_, _, OldNewOcculteds<OccId>) = {
    let gpc = gpieces.get(piece).ok_or_else(nopiece)?;

    #[derive(Debug,Copy,Clone)]
    struct Occulted<'o> { occid: OccId, occ: &'o Occultation }

    let occulteds = OldNewOcculteds {
      old:
        gpc.occult.passive.map(|Passive { occid, notch }| Ok::<_,IE>((
          Occulted {
            occid,
            occ: goccults.occults.get(occid).ok_or_else(
              || internal_logic_error("uccultation vanished"))?,
          },
          notch,
        ))).transpose()?,

      new:
        goccults.occults.iter().find_map(|(occid, occ)| {
          if gpc.pinned {
            // Prevent pinned pieces being occulted.  What scrambling
            // them etc. would mean is not entirely clear.
            return None
          } else if gpc.occult.active.is_some() {
            // prevent occulting pieces being occulted
            // (also prevents reflexive occultation)
            return None
          } else if ipc.occilk.is_none() {
            // if we cannot make it look identical to the others, we
            // cannot occult it beause we can't hide its identity
            return None
          } else if occ.in_region(gpc.pos) {
            Some(Occulted { occid, occ })
          } else {
            None
          }
        }),
    };
    trace_dbg!("recalculating", piece, occulteds);

    let occids = occulteds.main().map(|h| h.as_ref().map(|occ| occ.occid));
    if occids.old() == occids.new() { return ret_vanilla(); }

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
          occulteds.main().map(|occulted| {
            if let Some(occulted) = occulted {
              occulted.occ.get_kind(player)
            } else {
              &OccK::Visible
            }
          }))
        .or_default()
        .push(player);
    }

    trace_dbg!("situations", &situations);

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
      trace_dbg!("situation", &kinds, &players, &most_obscure, &puo);

      if let Some(puo) = puo {
        for player in players {
          puos.insert(*player, puo);
        }
      }
    }
    // this calculation does not seem to work?
    // to repro: bob moves a card out of bob's hand
    // alice sees "a card with a red-striped back"
    trace_dbg!("most_obscure", most_obscure);

    let describe_occulter = |oni| Ok::<_,IE>(if_chain! {
      if let Some(h) = occulteds.as_refs().main()[oni];
      let opiece = h.occ.occulter;
      let bad = || internal_error_bydebug(&("missing", opiece, h.occid));
      let oipc = ipieces.get(opiece).ok_or_else(bad)?;
      let ogpc = gpieces.get(opiece).ok_or_else(bad)?;
      let ounocc = ogpc.fully_visible_to_everyone()
        .ok_or_else(||internal_error_bydebug(&(occulteds, &ogpc)))?;
      then {
        Some(oipc.show(ounocc).describe_html(ogpc, goccults)?)
      } else {
        None
      }
    });

    let most_obscure = most_obscure.unwrap_or(&OccK::Visible); // no players!

    let call_log_callback =
      |show| Ok::<_,IE>(
        log_callback(describe_occulter(ONI::Old)?,
                     describe_occulter(ONI::New)?,
                     show)
      );

    let log = match most_obscure.map_displaced(|_|((),())).pri_occulted() {
      Some(prioc@ PriOG::Visible(_)) |
      Some(prioc@ PriOG::Occulted) |
      Some(prioc@ PriOG::Displaced(..)) => {
        let show = prioc.describe(ioccults, goccults, gpc, ipc);
        call_log_callback(&show)?
      },
      None => {
        log_invisible
      },
    };

    (puos, log, occulteds.map(|h| h.occid))
  };
  

  trace_dbg!("committing", &puos, &log, &occulteds);

  (||{
    let occultation:
       &mut dyn for<'g> FnMut(&'g mut GameOccults, OccId) -> &mut Occultation
      = &mut |goccults, occid|
      // rust-lang/rust/issues/58525
    {
      to_recalculate.mark_dirty(occid);
      goccults.occults.get_mut(occid).unwrap()
    };
    if let Some((occid, old_notch)) = occulteds.old {
      occultation(goccults, occid)
        .notches
        .remove(piece, old_notch)
        .unwrap()
    };
    let passive = if_chain!{
      if let Some(occid) = occulteds.new;
      let zg = gen.next();
      let occ = occultation(goccults, occid);
      if let Some(ilk) = wants!( ipc.occilk.as_ref() );
      then {
        if_chain!{
          if occ.notches.is_empty();
          if let Some(ilk) = wants!( ioccults.ilks.get(ilk) );
          if let Some(bbox) = want!( Ok = ilk.p_occ.bbox_approx() );
          if let Some(size) = want!( Ok = bbox.br() - bbox.tl(), ?(bbox) );
          then { occ.ppiece_use_size = size; }
        };
        let notch = occ.notches
          .insert(zg, piece);
        Some(Passive { occid, notch })
      }
      else {
        None
      }
    };
    gpieces.byid_mut(piece).unwrap().occult.passive = passive;
  })(); // <- no ?, infallible commitment

  ret_callback(puos, log)
}

#[throws(InternalError)]
pub fn recalculate_occultation_piece(
  gs: &mut GameState,
  who_by: Html,
  ipieces: &IPieces,
  ioccults: &IOccults,
  to_recalculate: &mut ToRecalculate,
  piece: PieceId,
  (vanilla_wrc, vanilla_op, vanilla_log): PUFOS,
)
  -> PieceUpdate
{
    recalculate_occultation_general(
      &mut gs.gen.unique_gen(),
      &gs.players, &mut gs.pieces, &mut gs.occults, ipieces, ioccults,
      to_recalculate, piece,
      || (vanilla_wrc, vanilla_op, vanilla_log).into(),
      vec![],
      |old, new, show| vec![ LogEntry { html: hformat!(
        "{} {}",
        &who_by,
        match (old, new) {
          (None, None) => hformat!("modified {} somehow", show),
          (Some(old), None) => hformat!("produced {} from {}", show, old),
          (None, Some(new)) => hformat!("placed {} into {}", show, new),
          (Some(old), Some(new)) => hformat!("moved {} from {} to {}",
                                            show, old, new),
        }
      )}],
      |puos, log| PieceUpdate {
        wrc: WRC::Unpredictable,
        ops: PieceUpdateOps::PerPlayer(puos),
        log
      }
    )?
}

#[throws(IE)]
fn recalculate_occultation_ofmany(
  gen: &mut UniqueGenGen,
  gplayers: &GPlayers,
  gpieces: &mut GPieces,
  goccults: &mut GameOccults,
  ipieces: &IPieces,
  ioccults: &IOccults,
  to_recalculate: &mut ToRecalculate,
  ppiece: PieceId,
  updates: &mut Vec<(PieceId, PieceUpdateOps)>,
){
  recalculate_occultation_general(
    gen,
    gplayers, gpieces, goccults, ipieces, ioccults,
    to_recalculate, ppiece,
    ||(),
    (), |_,_,_|(),
    |puo_pp, ()|{
      updates.push((ppiece, PUOs::PerPlayer(puo_pp)));
    },
  )?;
}


mod recompute {
  use super::*;

  #[derive(Debug)]
  pub struct ToRecalculate {
    outdated: HashSet<OccId>,
  }
  #[must_use]
  pub struct Implemented(UnpreparedUpdates);
  impl Debug for Implemented {
    #[throws(fmt::Error)]
    fn fmt(&self, f: &mut Formatter) {
      write!(f, "Implemented({})",
             if self.0.is_some() { "Some(..)" } else { "None" })?;
    }
  }

  impl ToRecalculate {
    pub fn with<R, F: FnOnce(Self) -> (R, Implemented)>
      (f: F) -> (R, UnpreparedUpdates)
    {
      let to_recalculate = ToRecalculate { outdated: default() };
      let (r, Implemented(uu)) = f(to_recalculate);
      (r, uu)
    }
    pub fn mark_dirty(&mut self, occid: OccId) { self.outdated.insert(occid); }
    pub fn implement(self,
                     gplayers: &mut GPlayers,
                     gpieces: &mut GPieces,
                     goccults: &mut GameOccults,
                     ipieces: &IPieces) -> Implemented {
      let mut unprepared = vec![];

      for occid in self.outdated {
        if let Some(occ) = goccults.occults.get_mut(occid) {
          vpid::permute(occid, occ, gplayers, gpieces, ipieces);
          if let Some(ipc) = ipieces.get(occ.occulter) {
            if let Some(uu) = {
              ipc
                .direct_trait_access()
                .occultation_notify_hook(occ.occulter)
            } {
              unprepared.push(uu)
            }
          }
        }
      }

      consistency_check(gplayers, gpieces, goccults);

      let unprepared = if unprepared.is_empty() {
        None
      } else {
        Some(Box::new(
          move |updates: &mut PrepareUpdatesBuffer| {
            for p in unprepared.into_iter() { p(updates) }
          }
        ) as SomeUnpreparedUpdates
        )
      };

      Implemented(unprepared)
    }
  }
}

pub use recompute::ToRecalculate;

#[must_use]
pub struct NascentOccultation(Occultation);

#[derive(Debug,Clone)]
pub struct UniformOccultationView(
  pub OccultationKind,
);
#[derive(Debug,Clone)]
pub struct OwnerOccultationView {
  pub defview: OccultationKindAlwaysOk,
  pub owner: PlayerId,
  pub owner_view: OccultationKindAlwaysOk,
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
    defview: self.defview.into(),
    views: vec![OccultView {
      players: vec![self.owner],
      occult: self.owner_view.into(),
    }]
  } }
}

#[throws(APOE)]
pub fn create_occultation(
  gen: &mut UniqueGenGen,
  max_z: &mut ZLevel,
  gplayers: &mut GPlayers,
  gpieces: &mut GPieces,
  goccults: &mut GameOccults,
  ipieces: &IPieces,
  ioccults: &IOccults,
  to_recalculate: &mut ToRecalculate,
  _: OcculterRotationChecked,
  region: Region,
  occulter: PieceId,
  views: OccultationViews,
  // Caller must promise not to fail if we succeed, so that this
  // update actually happens!
  _puos_will_return: &PUOs_Simple_Modify,
) -> Vec<(PieceId, PieceUpdateOps)> {
  // We mustn't actually store this in gpieces until we commit.
  let ogpc_z_new = piece_make_heavy(gpieces, occulter)?;

  {
    let ogpc = gpieces.get(occulter).ok_or_else(
      ||internal_logic_error("create occultation with non-piece"))?;
    if ogpc.occult.active.is_some() {
      throw!(internal_logic_error("re-occulting!"))
    }

    if let Some(displ_z) = {
      views.views
        .iter().map(|ov| &ov.occult)
        .chain(iter::once(&views.defview))
        .filter_map(|ok| { use OccKG::*; match ok {
          Visible | Scrambled | Invisible => None,
          Displaced((_region, ref z)) => Some(z)
        }})
        .max()
    } {
      // We expect that ogpc.zlevel.z.increment() is shorter than
      // the displ_z, but in case it isn't, we must look at both.
      let max_z = &mut max_z.z;
      (||{
        max_z.update_max(&ogpc_z_new.clone_mut().increment()?);
        max_z.update_max(&displ_z.plus_offset(! 0)?);
        Ok::<_,IE>(())
      })()?;
    }
  }

  for occ in goccults.occults.values() {
    if occ.region.overlaps(&region) { throw!(Ia::OverlappingOccultation) }
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
    ppiece_use_size: PosC::zero(),
    notches: default(),
  };
  debug!("creating occultation {:?}", &occultation);
  trace_dbg!("recalc", &recalc);

  // Everything from here on must be undone if we get an error
  // but we hope not to get one...

  let occid = goccults.occults.insert(occultation);
  let mut updates = vec![];

  (|| (
    (||{
      let ogpc = gpieces.get_mut(occulter).ok_or_else(
        ||internal_logic_error("occulter vanished"))?;
      ogpc.occult.active = Some(occid);
      ogpc.zlevel.z = ogpc_z_new;

      for &ppiece in &recalc {
        recalculate_occultation_ofmany(gen,
                                       gplayers, gpieces, goccults,
                                       ipieces, ioccults,
                                       to_recalculate,
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
    })
  ))()?;

  trace_dbg!("created", &updates);
  updates
}

#[throws(IE)]
pub fn remove_occultation(
  gen: &mut UniqueGenGen,
  gplayers: &mut GPlayers,
  gpieces: &mut GPieces,
  goccults: &mut GameOccults,
  ipieces: &IPieces,
  ioccults: &IOccults,
  to_recalculate: &mut ToRecalculate,
  occulter: PieceId,
) -> Vec<(PieceId, PieceUpdateOps)> {
  let mut aggerr = AggregatedIE::new();

  let occid = if_chain! {
    if let Some(ogpc) = gpieces.get(occulter);
    // This can be None if the occulter is being deleted

    if let Some(occid) = ogpc.occult.active.or_else(||{
      aggerr.record(internal_logic_error(
        "removing occultation by non-active piece"));
      None
    });

    then { occid }
    else { aggerr.ok()?; panic!(); }
  };

  let mut updates = vec![];
  let mut unbreak_pieces: Vec<PieceId> = vec![];

  match (||{
    let occ = goccults.occults.get_mut(occid).ok_or_else(
      || internal_logic_error("removing nonexistent occultation"))?;
    debug!("removing occultation {:?}", &occ);

    // We have to recalculate with the occultation still active, so
    // that the affected pieces can know what the old situation was.
    // So we set the region to empty, and do a recalculation of the
    // relevant pieces.  Only then can we get rid of the occultation.
    occ.region = Region::empty();

    let pieces: Vec<_> = occ.notches.iter().collect();
    for &ppiece in pieces.iter() {
      recalculate_occultation_ofmany(gen,
                                     gplayers, gpieces, goccults,
                                     ipieces, ioccults,
                                     to_recalculate,
                                     ppiece, &mut updates)
        .unwrap_or_else(|e| {
          aggerr.record(e);
          if let Some(pgpc) = gpieces.get_mut(ppiece) {
            pgpc.occult.passive = None;
          }
        });
    }

    // now there should be nothing
    let occ = goccults.occults.remove(occid).ok_or_else(
      || internal_logic_error("occultation vanished in recalc!"))?;
    
    unbreak_pieces.extend(occ.notches.iter());

    Ok::<_,IE>(())
  })() {
    e@ Err(_) => {
      aggerr.handle(e);
      unbreak_pieces.extend(gpieces.keys());
    }
    Ok(()) => {},
  }

  if ! unbreak_pieces.is_empty() {
    aggerr.record(internal_logic_error(format!(
      "occultation remove left pieces: {:?}", &unbreak_pieces)));

    for ppiece in unbreak_pieces { if_chain! {
      if let Some(pgpc) = gpieces.get_mut(ppiece);
      if let Some(passive) = pgpc.occult.passive;
      if passive.occid == occid;
      then {
        pgpc.occult.passive = None;
        updates.push((ppiece, PieceUpdateOp::Modify(()).into()));
      }
    }}
  }

  if let Some(ogpc) = gpieces.get_mut(occulter) {
    ogpc.occult.active = None;
  } else {
    aggerr.record(internal_logic_error("removing occultation of non-piece"));
  }

  aggerr.ok()?;

  updates
}
