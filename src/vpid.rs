// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

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

pub type NotchNumber = u32;

define_index_type!{
  pub struct Notch = NotchNumber;
}

impl From<Notch> for u32 {
  fn from(n: Notch) -> u32 { n.index().try_into().unwrap() }
}

type NotchPtr = Option<Notch>;

#[derive(Clone,Copy,Debug,Serialize,Deserialize)]
#[derive(Eq,PartialEq)]
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
  zg: IndexVec<Notch, Generation>, // last time notch was (re)filled
  used: NotchNumber,
}

impl Occultation {
  pub fn notch_zg(&self, notch: Notch) -> Option<Generation> {
    self.notches.zg.get(notch).copied()
  }
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
  pub fn insert(&mut self, zg: Generation, piece: PieceId) -> Notch {
    let new = NR::Piece(piece);
    let notch = match self.freelist.take() {
      None => {
        let a = self.table.push(new);
        let b = self.zg.push(zg);
        // gs.max_z was updated when we created the occultation
        assert_eq!(a,b);
        a
      },
      Some(old_free_head) => {
        self.freelist = self.table[old_free_head].unwrap_free();
        self.table[old_free_head] = new;
        self.zg[old_free_head] = zg;
        old_free_head
      },
    };
    self.used += 1;
    notch
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
      self.used -= 1;
    }
  }

  pub fn is_empty(&self) -> bool { self.used == 0 }

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
      self.used as usize,
      pieces.len(),
    );
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
          let notch = notches.insert(Generation(0), p);
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
      let gpc = if let Some(gpc) = gpieces.get(piece) { gpc }else{ continue };
      if gpc.held.is_some() { continue }
      let occilk = (|| Some(ipieces.get(piece)?.occilk.as_ref()?))();
      let occilk = if let Some(o) = occilk { *o.borrow() } else {
        error!("{}", internal_error_bydebug(&(occid, &occ, &nr, piece)));
        continue;
      };
      let (notches, pieces) = ilks.entry(occilk).or_default();
      notches.push(notch);
      pieces.push(piece);
    }

    let mut new_notches = occ.notches.table.clone();
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
        config().game_rng.shuffle(&mut permu);
        permu
      };

      for (notch, new_piece) in itertools::zip_eq(notches, permu) {
        new_notches[notch] = NR::Piece(new_piece);
        gpieces.get_mut(new_piece).unwrap()
          .occult.passive.as_mut().unwrap()
          .notch
          = notch;
      }
    }

    new_notches
  };

  for (player, gpl) in gplayers.iter_mut() {
    match occ.views.get_kind(player) {
      OccK::Visible => continue,
      OccK::Scrambled |
      OccK::Displaced{..} |
      OccK::Invisible => (),
    };

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

#[cfg(debug_assertions)]
pub fn consistency_check(
  gplayers: &GPlayers,
  gpieces: &GPieces,
  goccults: &GameOccults,
) {
  for (_player, gpl) in gplayers.iter() {
    for (piece, &vpid) in gpl.idmap.f.iter() {
      if let Some(&rpiece) = gpl.idmap.r.get(vpid) {
        assert_eq!(piece, rpiece);
      }
    }
    for (vpid, &piece) in gpl.idmap.r.iter() {
      if let Some(&fvpid) = gpl.idmap.f.get(piece) {
        assert_eq!(vpid, fvpid);
      }
    }
  }

  for (piece, gpc) in gpieces.iter() {
    if let Some(occid) = gpc.occult.active {
      let occ = goccults.occults.get(occid).unwrap();
      assert_eq!(occ.occulter, piece);
      assert_eq!(&gpc.occult.passive, &None);
    }

    if let Some(Passive { occid, notch }) = gpc.occult.passive {
      let occ = goccults.occults.get(occid).unwrap();
      assert_eq!(occ.notches.table[notch], NR::Piece(piece));
    }
  }

  for (occid, occ) in goccults.occults.iter() {
    let ogpc = gpieces.get(occ.occulter).unwrap();
    assert_eq!(ogpc.occult.active, Some(occid));
    assert_eq!(occ.notches.table.len(), occ.notches.zg.len());
    let nfree1 = occ.notches.table.iter()
      .filter(|nr| nr.piece().is_none()).count();
    let mut walk = occ.notches.freelist;
    let mut nfree2 = 0;
    while let Some(here) = walk {
      nfree2 += 1;
      let next = match occ.notches.table[here] {
        NR::Free(next) => next,
        NR::Piece(_) => panic!(),
      };
      if let Some(next) = next {
        assert!(next > here);
      }
      walk = next;
    }
    assert_eq!(nfree1,  nfree2);
  }
}

