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
  passive: Option<(OccId, Notch)>, // kept in synch with Occultation::pieces
}

#[derive(Clone,Debug,Serialize,Deserialize)]
pub struct Occultation {
  region: Area, // automatically affect pieces here
  occulter: PieceId, // kept in synch with PieceOccult::active
  notches: Notches, // kept in synch with PieceOccult::passive
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
pub type OccultationKind = OccultationKindGeneral<Area>;

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

// ========== PerPlayerIdMap ==========

mod vpid {
  use super::*;

  #[derive(Clone,Debug,Default,Serialize,Deserialize)]
  pub struct PerPlayerIdMap {
    pub(in super) f: SecondarySlotMap<PieceId, VisiblePieceId>,
    pub(in super) r: DenseSlotMap<VisiblePieceId, PieceId>,
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

  pub type NotchNumber = usize;

  define_index_type!{
    pub struct Notch = NotchNumber;
  }

  type NotchPtr = Option<Notch>;

  #[derive(Clone,Copy,Debug,Serialize,Deserialize)]
  enum NotchRecord {
    Free(NotchPtr),
    Piece(PieceId),
  }
  type NR = NotchRecord;

  impl NotchRecord {
    fn piece(&self) -> Option<PieceId> { match self {
      &NR::Piece(p) => Some(p),
      &NR::Free(_) => None,
    }}

    fn unwrap_free(&self) -> NotchPtr { match self {
      &NR::Free(ptr) => ptr,
      _ => panic!(),
    }}

    fn unwrap_free_mut(&mut self) -> &mut NotchPtr { match self {
      NR::Free(ptr) => ptr,
      _ => panic!(),
    }}
  }

  #[derive(Clone,Debug,Serialize,Deserialize,Default)]
  pub struct Notches {
    freelist: NotchPtr,
    table: IndexVec<Notch, NotchRecord>,
  }

  impl Notches {
    pub fn iter<'i>(&'i self) -> impl Iterator<Item=PieceId> + 'i {
      self.table.iter()
        .filter_map(|nr| match nr {
          NR::Free(_) => None,
          &NR::Piece(p) => Some(p),
        })
    }

    /// correctness: piece must not already be in this `Notches`
    pub fn insert(&mut self, piece: PieceId) -> Notch {
      let new = NR::Piece(piece);
      match self.freelist.take() {
        None => {
          self.table.push(new)
        },
        Some(old_free_head) => {
          self.freelist = self.table[old_free_head].unwrap_free();
          self.table[old_free_head] = new;
          old_free_head
        },
      }
    }

    #[throws(IE)]
    pub fn remove(&mut self, piece: PieceId, notch: Notch) {
      match self.table.get(notch) {
        Some(&NR::Piece(p)) if p == piece => { },
        _ => throw!(internal_error_bydebug(&(piece, notch, self))),
      }
      {
        let mut insert_here = &mut self.freelist;
        loop {
          let next = *insert_here;
          let next = if let Some(y) = next { y } else { break };
          if notch < next { break }
          if notch == next { panic!() };
          let next = &mut self.table[next];
          insert_here = next.unwrap_free_mut();
        }
        // Now either *insert_here==NULL or notch < insert_here->next
        let old_next = mem::replace(insert_here, Some(notch));
        self.table[notch] = NR::Free(old_next);
      }
    }

    #[cfg(test)]
    fn check(&self) {
      let mut count_free = 0;
      let mut walk = self.freelist;
      while let Some(here) = walk {
        count_free += 1;
        let next_walk = self.table[here].unwrap_free();
        if let Some(more) = next_walk {
          assert!(more > here);
        }
        walk = next_walk;
      }
      assert_eq!(
        count_free,
        self.table.iter()
          .filter(|nr| matches!(nr, NR::Free(_)))
          .count()
      );
      let pieces = self.table.iter()
        .filter_map(|nr| if let NR::Piece(p) = nr { Some(p) } else { None })
        .collect::<HashSet<_>>();
      assert_eq!(
        count_free + pieces.len(),
        self.table.len(),
      );
    }
  }
    
  #[test]
  fn exhaustive() {
    enum Can {
      Insert(PieceId),
      Remove(PieceId),
    }
    impl Debug for Can {
      fn fmt(&self, f: &mut fmt::Formatter) -> Result<(),fmt::Error> {
        let (c, p) = match self {
          Can::Insert(p) => ('+', p),
          Can::Remove(p) => ('-', p),
        };
        let (idx, _vsn) = p.data().get_idx_version();
        write!(f, "{}{}", c, idx)
      }
    }

    #[derive(Debug)]
    struct State {
      todo: Vec<Can>,
      can: Vec<Can>,
    }

    const NPIECES: usize = 4;
    const DEPTH: usize = 7;

    impl State {
      fn implement(&self) {
        eprintln!("implement {:?}", self);
        let mut notches: Notches = default();
        let mut pieces = SecondarySlotMap::new();
        for op in &self.todo { match op {
          &Can::Insert(p) => {
            let notch = notches.insert(p);
            notches.check();
            pieces.insert(p, notch);
          },
          &Can::Remove(p) => {
            let notch = pieces[p];
            notches.remove(p, notch).unwrap();
            notches.check();
          },
        }}
      }

      fn recurse(&mut self) {
        if self.todo.len() == DEPTH { self.implement(); return; }
        eprintln!("  recurse {:?}", &self);

        for i in 0..self.can.len() {
          let op = self.can.swap_remove(i);

          let l = self.can.len();
          match &op {
            &Can::Insert(p) => { self.can.push(Can::Remove(p)) },
            &Can::Remove(_) => { },
          }

          self.todo.push(op);
          self.recurse();
          let op = self.todo.pop().unwrap();

          self.can.truncate(l);

          let last = if i < self.can.len() {
            mem::replace(&mut self.can[i], op)
          } else {
            op
          };
          self.can.push(last);
        }
      }
    }


    let mut st = State {
      todo: vec![],
      can: {
        let mut slotmap: DenseSlotMap<PieceId,()> = default();
        (0..NPIECES)
          .map(|_| slotmap.insert(()))
          .map(|p| Can::Insert(p))
          .collect()
      },
    };

    st.recurse();
  }                                        

  #[allow(unused_variables)]
  pub fn permute(occid: OccId,
                 occ: &mut Occultation,
                 gplayers: &mut GPlayers,
                 gpieces: &mut GPieces,
                 ipieces: &IPieces) {
    let new_notches = {

      let mut ilks: HashMap<OccultIlkId, (
        Vec<Notch>,
        Vec<PieceId>,
      )> = HashMap::new();

      for (notch, nr) in occ.notches.table.iter_enumerated() {
        let piece = if let Some(p) = nr.piece() { p } else { continue };
        let occilk = (|| Some(ipieces.get(piece)?.occilk.as_ref()?))();
        let occilk = if let Some(o) = occilk { *o.borrow() } else {
          error!("{}", internal_error_bydebug(&(occid, &occ, &nr, piece)));
          continue;
        };
        let (notches, pieces) = ilks.entry(occilk).or_default();
        notches.push(notch);
        pieces.push(piece);
      }

      let mut new_notches = index_vec![];
      for (_occilk, (notches, pieces)) in ilks {
        // We must permute for if we have any views that are scrambled
        // or displaced obviously.  For invisible too, so that when they
        // reappear the ids have been permuted.  And that's all the
        // non-Visible views which an occultation ought to have at least
        // one of...
        //
        // We choose a single permutation of each of the subsets (for a
        // partiular ilk) of the pieces in the Occultation::notches.

        let permu = {
          let mut permu = pieces;
          let mut rng = thread_rng();
          permu.shuffle(&mut rng);
          permu
        };

        for (notch, new_piece) in itertools::zip_eq(notches, permu) {
          new_notches[notch] = NR::Piece(new_piece);
        }
      }

      new_notches
    };

    for gpl in gplayers.values_mut() {
      // xxx don't do this for Visible, check occk

      let mut fwd_updates = vec![];
      for (old, new) in izip!(&occ.notches.table, &new_notches) {
        if_chain! {
          if let Some((old, new)) = zip_eq(old.piece(), new.piece()).next();
          if let Some(vpid) = gpl.idmap.fwd(old);
          then {
            fwd_updates.push((new, vpid));
            if let Some(e) = gpl.idmap.r.get_mut(vpid) {
              *e = new;
            }
          }
        }
      }

      for (new, vpid) in fwd_updates {
        gpl.idmap.f.insert(new, vpid);
      }

    }
    occ.notches.table = new_notches;
    dbgc!(&occ);
  }
}

pub use vpid::{PerPlayerIdMap, NotchNumber, Notch, Notches};

// ========== public entrypoints ==========

/// None => do not render at all
pub fn piece_pri(
  ioccults: &IOccults,
  occults: &GameOccults,
  player: PlayerId, gpl: &mut GPlayer,
  piece: PieceId, gpc: &GPiece, ipc: &IPiece,
) -> Option<PieceRenderInstructions>
{
fn inner(
  _ioccults: &IOccults,
  occults: &GameOccults,
  player: PlayerId, gpl: &mut GPlayer,
  piece: PieceId, gpc: &GPiece, _ipc: &IPiece,
) -> Option<PieceRenderInstructions>
{
  let occk = if_chain! {
    if let Some((occid, notch)) = gpc.occult.passive;
    if let Some(occultation) = occults.occults.get(occid);
    then {
      occultation.views.get_kind(player)
        .map_displaced(|area| {
          let x: Coord = NotchNumber::from(notch).try_into().unwrap(); // xxx
          let pos = area.0[0] + PosC([x*2, 0]); // xxx
          pos.unwrap()
        })
    }
    else {
      OccKG::Visible
    }
  };

  let occulted = match occk {
    OccKG::Invisible => {
      trace_dbg!("piece_pri", gpc, occk);
      return None;
    }
    OccKG::Visible        => PriOcculted::Visible,
    OccKG::Scrambled      => PriOcculted::Occulted,
    OccKG::Displaced(pos) => PriOcculted::Displaced(pos),
  };
  let vpid = gpl.idmap.fwd_or_insert(piece);
  trace_dbg!("piece_pri", gpc, occk, vpid, occulted);
  Some(PieceRenderInstructions { vpid, occulted })
}
  inner(ioccults, occults, player, gpl, piece, gpc, ipc)
}

pub fn piece_at_all_occulted(gpc: &GPiece) -> bool {
  gpc.occult.passive.is_some()
}
pub fn piece_involved_in_occultation(gpc: &GPiece) -> bool {
  gpc.occult.passive.is_some() ||
  gpc.occult.active.is_some()
}
#[throws(OE)]
pub fn forbid_piece_involved_in_occultation(gpc: &GPiece) {
  if piece_involved_in_occultation(&gpc) { throw!(OE::Occultation) }
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
// xxx prevent occultation scrambling of grasped things

#[throws(InternalError)]
fn recalculate_occultation_general<
  RD: Debug,                                          // return data
  LD: Debug,                                          // log data
  VF: FnOnce(LD) -> RD,                               // ret_vanilla
  LF: FnOnce(Html, Html, Option<&Html>) -> LD,        // log_callback
  RF: FnOnce(PieceUpdateOps_PerPlayer, LD) -> RD,     // ret_callback
>(
  gplayers: &GPlayers,
  gpieces: &mut GPieces,
  goccults: &mut GameOccults,
  ipieces: &IPieces,
  to_recompute: &mut ToRecompute,
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

  // fallible part
  let (puos, log, occulteds): (_, _, OldNewOcculteds<OccId>) = {
    let nopiece = || internal_logic_error("piece vanished");
    let ipc = ipieces.get(piece).ok_or_else(nopiece)?;
    let gpc = gpieces.get(piece).ok_or_else(nopiece)?;

    #[derive(Debug,Copy,Clone)]
    struct Occulted<'o> { occid: OccId, occ: &'o Occultation }

    let occulteds = OldNewOcculteds {
      old:
        gpc.occult.passive.map(|(occid, notch)| Ok::<_,IE>((
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
          } else if gpc.occult.active.is_some() { // xxx remove dbg!
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
    dbgc!((piece, occulteds));

    let occids = occulteds.main().map(|h| h.as_ref().map(|occ| occ.occid));
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
    dbgc!(most_obscure);

    let describe_occulter = |oni| {
      let h = occulteds.as_refs().main()[oni];
      let h = h.as_ref().ok_or_else(
        || internal_logic_error("most obscure not obscure"))?;
      let opiece = h.occ.occulter;
      let bad = || internal_error_bydebug(&("missing", opiece, h.occid));
      let oipc = ipieces.get(opiece).ok_or_else(bad)?;
      let ogpc = gpieces.get(opiece).ok_or_else(bad)?;
      Ok::<_,IE>(oipc.p.describe_html(ogpc)?)
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
        let _face = ipc.p.nfaces() - 1; // xxx use other thing entirely
        let show = ipc.p.describe_html(gpc)?;
        call_log_callback(Some(&show))?
      },
      OccK::Invisible => {
        call_log_callback(None)?
      },
    };

    (puos, log, occulteds.map(|h| h.occid))
  };
  

  dbgc!(&puos, &log, &occulteds);
  // point of no return

  // xxx shuffle some players' ids
  // xxx and/or shuffle locations

  (||{
    let notches:
       &mut dyn for<'g> FnMut(&'g mut GameOccults, OccId) -> &mut Notches
      = &mut |goccults, occid|
      // rust-lang/rust/issues/58525
    {
      to_recompute.mark_dirty(occid);
      &mut goccults.occults.get_mut(occid).unwrap().notches
    };
    if let Some((occid, old_notch)) = occulteds.old {
      notches(goccults, occid)
        .remove(piece, old_notch)
        .unwrap()
    };
    let passive = if let Some(occid) = occulteds.new {
      let notch = notches(goccults, occid)
        .insert(piece);
      Some((occid, notch))
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
  piece: PieceId,
  (vanilla_wrc, vanilla_op, vanilla_log): PUFOS,
)
  -> PieceUpdate
{
  ToRecompute::with(|mut to_recompute| (
    recalculate_occultation_general(
      &gs.players, &mut gs.pieces, &mut gs.occults, ipieces, &mut to_recompute,
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
    ),
    to_recompute.implement(&mut gs.players, &mut gs.pieces, &mut gs.occults,
                           ipieces),
  ))?
}

#[throws(IE)]
fn recalculate_occultation_ofmany(
  gplayers: &GPlayers,
  gpieces: &mut GPieces,
  goccults: &mut GameOccults,
  ipieces: &IPieces,
  to_recompute: &mut ToRecompute,
  ppiece: PieceId,
  updates: &mut Vec<(PieceId, PieceUpdateOps)>,
){
  recalculate_occultation_general(
    gplayers, gpieces, goccults, ipieces, to_recompute,
    ppiece,
    (), |_|(),
    |_,_,_|(), |puo_pp, ()|{
      updates.push((ppiece, PUOs::PerPlayer(puo_pp)));
    },
  )?;
}


mod recompute {
  use super::*;

  #[derive(Debug)]
  pub struct ToRecompute {
    outdated: HashSet<OccId>,
  }
  #[derive(Debug)]
  pub struct Implemented { }

  impl ToRecompute {
    pub fn with<R, F: FnOnce(Self) -> (R, Implemented)>(f: F) -> R {
      let to_recompute = ToRecompute { outdated: default() };
      let (r, Implemented { }) = f(to_recompute);
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

      Implemented { }
    }
  }
}

use recompute::*;

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
  gplayers: &mut GPlayers,
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
    notches: default(),
  };
  debug!("creating occultation {:?}", &occultation);
  dbgc!(&recalc);

  // Everything from here on must be undone if we get an error
  // but we hope not to get one...

  let occid = goccults.occults.insert(occultation);
  let mut updates = vec![];

  ToRecompute::with(|mut to_recompute| (
    (||{
      let ogpc = gpieces.get_mut(occulter).ok_or_else(
        ||internal_logic_error("occulter vanished"))?;
      ogpc.occult.active = Some(occid);

      for &ppiece in &recalc {
        recalculate_occultation_ofmany(gplayers, gpieces, goccults, ipieces,
                                       &mut to_recompute,
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
    }),
    to_recompute.implement(gplayers, gpieces, goccults, ipieces),
  ))?;

  dbgc!(&updates);
  updates
}

#[throws(IE)]
pub fn remove_occultation(
  gplayers: &mut GPlayers,
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

  ToRecompute::with(|mut to_recompute| ({
    let pieces: Vec<PieceId> = if let Some(o) = &occultation {
      o.notches.iter().collect()
    } else {
      gpieces
        .iter()
        .filter_map(|(ppiece, pgpc)| {
          if pgpc.occult.passive.map(|p| p.0) == Some(occid) { Some(ppiece) }
          else { None }
        })
        .collect()
    };
  
    for &ppiece in pieces.iter() {
      recalculate_occultation_ofmany(gplayers, gpieces, goccults, ipieces,
                                     &mut to_recompute,
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
  },
    to_recompute.implement(gplayers, gpieces, goccults, ipieces),
  ));

  aggerr.ok()?;

  updates
}
