// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

#[path="vpid.rs"] mod vpid;
pub use vpid::{PerPlayerIdMap, NotchNumber, Notch, Notches, consistency_check};

use slotmap::secondary;

type ONI = OldNewIndex;

visible_slotmap_key!{ OccId(b'H') }

// ========== data structures ==========

#[derive(Copy,Clone,Debug)]
pub struct ShowUnocculted(());

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
  region: Area, // automatically affect pieces here
  occulter: PieceId, // kept in synch with PieceOccult::active
  notches: Notches, // kept in synch with PieceOccult::passive
  ppiece_use_size: Pos, // taken from first piece
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
pub enum OccultationKindGeneral<D> {
  Visible,
  Scrambled,
  Displaced(D),
  Invisible,
}
pub type OccultationKind = OccultationKindGeneral<(OccDisplacement,ZCoord)>;

#[derive(Clone,Debug,Serialize,Deserialize)]
#[derive(Eq,PartialEq,Hash)]
pub enum OccDisplacement {
  Rect {
    area: Area,
  },
}

impl PieceOccult {
  pub fn is_active(&self) -> bool { self.active.is_some() }
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
  let occk = if_chain! {
    if let Some(Passive { occid, notch }) = gpc.occult.passive;
    if let Some(occ) = occults.occults.get(occid);
    if let Some(zg) = occ.notch_zg(notch);
    then {
      occ.views.get_kind(player)
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
        })
    }
    else {
      OccKG::Visible
    }
  };

  let occk_dbg = occk.clone();
  let occulted = match occk.pri_occulted() {
    Some(o) => o,
    None => {
      trace_dbg!("piece_pri", player, piece, occk_dbg, gpc);
      return None;
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
      OD::Rect{area} => (|| Some({
        let notch: Coord = notch.try_into().ok()?;
        let mut spare = ((area.0[1] - area.0[0]).ok()?
                         - ppiece_use_size).ok()?;
        for s in &mut spare.0 { *s = max(*s,1) }
        let fi = 0;
        let gi = 1;
        let f_stride = max(ppiece_use_size.0[fi] / 4, 1);
        let g_stride = max(ppiece_use_size.0[gi] / 3, 1);
        let f_count = max(spare.0[fi] / f_stride, 1);
        let g_count = max(spare.0[gi] / g_stride, 1);
        let mut f_num = notch % f_count;
        let     g_num = notch / f_count;
        if g_num % 2 != 0 { f_num = f_count - 1 - f_num }
        let base = (area.0[0] + ppiece_use_size.mean(&PosC([0,0]))).ok()?;
        let f_coord = base.0[fi] + f_stride * f_num;
        let g_coord = base.0[gi] +
          if g_num < g_count {
            g_stride * g_num
          } else if g_num < spare.0[gi] {
            g_num
          } else {
            spare.0[gi] - 1
          };
        trace_dbg!("placement", spare,
                   f_stride, f_count, f_num, f_coord,
                   g_stride, g_count, g_num, g_coord);
        let mut pos = PosC([0,0]);
        pos.0[fi] = f_coord;
        pos.0[gi] = g_coord;
        pos
      }))().unwrap_or_else(||{
        area.middle()
      })
    }
  }
}

impl ShowUnocculted {
  /// override
  pub const fn new_visible() -> ShowUnocculted {
    ShowUnocculted(())
  }
}

impl PieceRenderInstructions {
  /// override
  pub fn new_visible(vpid: VisiblePieceId) -> PieceRenderInstructions {
    PieceRenderInstructions {
      vpid,
      occulted: PriOcculted::Visible(ShowUnocculted(())),
    }
  }
}

impl IPieceTraitObj {
  pub fn new(p: Box<dyn PieceTrait>) -> Self { Self(p) }

  pub fn show(&self, _: ShowUnocculted) -> &Box<dyn PieceTrait> {
    &self.0
  }

  pub fn into_inner(self) -> Box<dyn PieceTrait> { self.0 }

  pub fn direct_trait_access(&self) -> &Box<dyn PieceTrait> {
    &self.0
  }
}

impl IPiece {
  #[throws(IE)]
  pub fn show_or_instead<'p>(&self, ioccults: &'p IOccults,
                         y: Option<ShowUnocculted>)
          -> Either<ShowUnocculted, &'p dyn OccultedPieceTrait> {
    match y {
      Some(y) => Left(y),
      None => Right({
        let occilk = self.occilk.as_ref()
          .ok_or_else(|| internal_logic_error(format!(
            "occulted non-occultable {:?}", self)))?
          .borrow();
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

  pub fn involved_in_occultation(&self) -> bool {
    self.occult.passive.is_some() ||
    self.occult.active.is_some()
  }

  #[throws(OE)]
  pub fn forbid_involved_in_occultation(&self) {
    if self.involved_in_occultation() { throw!(OE::Occultation) }
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
  to_permute: &mut ToPermute, piece: PieceId,
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

    dbgc!(&situations);

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
      dbgc!(&kinds, &players, &most_obscure, &puo);

      if let Some(puo) = puo {
        for player in players {
          puos.insert(*player, puo);
        }
      }
    }
    // this calculation does not seem to work?
    // to repro: bob moves a card out of bob's hand
    // alice sees "a card with a red-striped back"
    dbgc!(most_obscure);

    let describe_occulter = |oni| Ok::<_,IE>(if_chain! {
      if let Some(ref h) = occulteds.as_refs().main()[oni];
      let opiece = h.occ.occulter;
      let bad = || internal_error_bydebug(&("missing", opiece, h.occid));
      let oipc = ipieces.get(opiece).ok_or_else(bad)?;
      let ogpc = gpieces.get(opiece).ok_or_else(bad)?;
      let ounocc = ogpc.fully_visible_to_everyone()
        .ok_or_else(||internal_error_bydebug(&(occulteds, &ogpc)))?;
      then {
        Some(oipc.show(ounocc).describe_html(ogpc)?)
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
        let show = prioc.describe(ioccults, gpc, ipc);
        call_log_callback(&show)?
      },
      None => {
        log_invisible
      },
    };

    (puos, log, occulteds.map(|h| h.occid))
  };
  

  dbgc!(&puos, &log, &occulteds);

  (||{
    let occultation:
       &mut dyn for<'g> FnMut(&'g mut GameOccults, OccId) -> &mut Occultation
      = &mut |goccults, occid|
      // rust-lang/rust/issues/58525
    {
      to_permute.mark_dirty(occid);
      goccults.occults.get_mut(occid).unwrap()
    };
    if let Some((occid, old_notch)) = occulteds.old {
      occultation(goccults, occid)
        .notches
        .remove(piece, old_notch)
        .unwrap()
    };
    let passive = if let Some(occid) = occulteds.new {
      let zg = gen.next();
      let occ = occultation(goccults, occid);
      if_chain!{
        if occ.notches.is_empty();
        if let Some(ilk) = ipc.occilk.as_ref();            // expected, really
        let ilk = ilk.borrow();
        if let Some(ilk) = ioccults.ilks.get(ilk);         // expected, really
        if let Ok::<_,IE>(bbox) = ilk.p_occ.bbox_approx(); // expected, really
        if let Ok(size) = bbox[1] - bbox[0];               // expected, really
        then { occ.ppiece_use_size = size; }
      };
      let notch = occ.notches
        .insert(zg, piece);
      Some(Passive { occid, notch })
    } else {
      None
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
  to_permute: &mut ToPermute,
  piece: PieceId,
  (vanilla_wrc, vanilla_op, vanilla_log): PUFOS,
)
  -> PieceUpdate
{
    recalculate_occultation_general(
      &mut gs.gen.unique_gen(),
      &gs.players, &mut gs.pieces, &mut gs.occults, ipieces, ioccults,
      to_permute, piece,
      || (vanilla_wrc, vanilla_op, vanilla_log).into(),
      vec![],
      |old, new, Html(show)| vec![ LogEntry { html: Html(format!(
        "{} {}",
        &who_by.0,
        match (old, new) {
          (None, None) => format!("modified {} somehow", show),
          (Some(old), None) => format!("produced {} from {}", show, &old.0),
          (None, Some(new)) => format!("placed {} into {}", show, &new.0),
          (Some(old), Some(new)) => format!("moved {} from {} to {}",
                                            show, &old.0, &new.0),
        },
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
  gen: &mut UniqueGenGen,
  gplayers: &GPlayers,
  gpieces: &mut GPieces,
  goccults: &mut GameOccults,
  ipieces: &IPieces,
  ioccults: &IOccults,
  to_permute: &mut ToPermute,
  ppiece: PieceId,
  updates: &mut Vec<(PieceId, PieceUpdateOps)>,
){
  recalculate_occultation_general(
    gen,
    gplayers, gpieces, goccults, ipieces, ioccults,
    to_permute, ppiece,
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
  pub struct ToPermute {
    outdated: HashSet<OccId>,
  }
  #[derive(Debug)]
  pub struct Implemented(());

  impl ToPermute {
    pub fn with<R, F: FnOnce(Self) -> (R, Implemented)>(f: F) -> R {
      let to_permute = ToPermute { outdated: default() };
      let (r, Implemented(())) = f(to_permute);
      r
    }
    pub fn mark_dirty(&mut self, occid: OccId) { self.outdated.insert(occid); }
    pub fn implement(self,
                     gplayers: &mut GPlayers,
                     gpieces: &mut GPieces,
                     goccults: &mut GameOccults,
                     ipieces: &IPieces) -> Implemented {
      for occid in self.outdated {
        if let Some(occ) = goccults.occults.get_mut(occid) {
          vpid::permute(occid, occ, gplayers, gpieces, ipieces);
        }
      }

      if cfg!(debug_assertions) {
        consistency_check(gplayers, gpieces, goccults);
      }

      Implemented(())
    }
  }
}

pub use recompute::ToPermute;

#[must_use]
pub struct NascentOccultation(Occultation);

#[derive(Debug,Clone)]
pub struct UniformOccultationView(
  pub OccultationKind,
);
#[derive(Debug,Clone)]
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
  gen: &mut UniqueGenGen,
  max_z: &mut ZCoord,
  gplayers: &mut GPlayers,
  gpieces: &mut GPieces,
  goccults: &mut GameOccults,
  ipieces: &IPieces,
  ioccults: &IOccults,
  to_permute: &mut ToPermute,
  region: Area,
  occulter: PieceId,
  views: OccultationViews,
) -> Vec<(PieceId, PieceUpdateOps)> {
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
          Displaced((_area, ref z)) => Some(z)
        }})
        .max()
    } {
      // We expect that ogpc.zlevel.z.increment() is shorter than
      // the displ_z, but in case it isn't, we must look at both.
      (||{
        max_z.update_max(&ogpc.zlevel.z.clone_mut().increment()?);
        max_z.update_max(&displ_z.plus_offset(! 0)?);
        Ok::<_,IE>(())
      })()?;
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
    ppiece_use_size: PosC([0,0]),
    notches: default(),
  };
  debug!("creating occultation {:?}", &occultation);
  dbgc!(&recalc);

  // Everything from here on must be undone if we get an error
  // but we hope not to get one...

  let occid = goccults.occults.insert(occultation);
  let mut updates = vec![];

  (|| (
    (||{
      let ogpc = gpieces.get_mut(occulter).ok_or_else(
        ||internal_logic_error("occulter vanished"))?;
      ogpc.occult.active = Some(occid);

      for &ppiece in &recalc {
        recalculate_occultation_ofmany(gen,
                                       gplayers, gpieces, goccults,
                                       ipieces, ioccults,
                                       to_permute,
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

  dbgc!(&updates);
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
  to_permute: &mut ToPermute,
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
    occ.region = Area::empty();

    let pieces: Vec<_> = occ.notches.iter().collect();
    for &ppiece in pieces.iter() {
      recalculate_occultation_ofmany(gen,
                                     gplayers, gpieces, goccults,
                                     ipieces, ioccults,
                                     to_permute,
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
