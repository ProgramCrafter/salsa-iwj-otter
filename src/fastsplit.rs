//! Pieces that can split and merge in play
//!
//! *Not* done to enable sharing of the piece image between instances
//! of "the same" piece.  Rather, to avoid having to rewrite the aux
//! file when a piece is split or merged.
//!
//! The approach is that splittable/mergeable pieces trait objects
//! are all wrapped up in fastsplit::Piece, and GPiece has an ID
//! that can make more of them so they can be recovered on game load.

use crate::prelude::*;

visible_slotmap_key!{ FastSplitId(b'F') }

impl FastSplitId {
  pub fn new_placeholder() -> Option<FastSplitId> { Some(default()) }
}

#[derive(Default,Debug,Serialize,Deserialize)]
pub struct IFastSplits {
  table: DenseSlotMap<FastSplitId, Record>
}

use crate::*; // wat, https://github.com/rust-lang/rust/pull/52234

#[derive(Serialize,Deserialize,Debug)]
struct Piece {
  // When we deserialize, we can effectiely clone the contents of the Arc.
  // But it contains an OccultIlkOwningId which is not Clone (since
  // it needs to manage the refcount in the occult ilks table).
  //
  // So instead, we serialize this to DummyPiece.  That means that
  // when we serialize, we only visit each Arc once, via Record.  So
  // when we deserialize, we preserve the original number of
  // OccultIlkOwningIds.
  //
  // When loaded, an occultable Piece has in fact *two* OccultIlkOwningIds,
  // one in the IPiece here inside this Arc (accessible from Piece
  // or Record) and one in the outer IPiece which is in the IPieces table.
  // Trying to optimise one of these away would be quite confusing.
  #[serde(skip)]
  ipc: Option<Arc<IPiece>>,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct Record {
  ipc: Arc<IPiece>,
}

impl Piece {
  fn inner(&self) -> &dyn PieceTrait {
    self.ipc
      .as_ref()
      .expect("attempted to invoke unresolved fastsplit::Piece")
      .p.direct_trait_access()
  }
}

impl_via_ambassador!{
  #[dyn_upcast]
  impl OutlineTrait for Piece { inner() } 

  #[dyn_upcast]
  impl PieceBaseTrait for Piece { inner() }

  #[typetag::serde(name="FastSplit")]
  impl PieceTrait for Piece { inner() }
}

impl Record {
  fn dummy() -> Record {
    let p = Piece { ipc: None };
    let p = IPieceTraitObj::new(Box::new(p) as _);
    let ipc = IPiece { p, occilk: None, special: default() };
    Record { ipc: Arc::new(ipc) }
  }
}

impl InstanceGuard<'_> {
  /// The new piece will be below the old one
  #[throws(ApiPieceOpError)]
  pub fn fastsplit_split<I>(
    &mut self, player: PlayerId,
    tpiece: PieceId, _: ShowUnocculted, tpc_new_z: ShouldSetZLevel,
    implementation: I
  ) -> UpdateFromOpComplex
  where I: FnOnce(&IOccults, &GOccults, &GPlayer,
                  &mut GPiece, &IPiece,
                  &mut GPiece)
                  -> Result<UpdateFromOpComplex, ApiPieceOpError>
  {
    // All this complicated machinery exists precisely to let us make
    // pieces without having to save aux.
    //
    // The "save later" part of this ought to be unnecessarily, because
    // we'll be running in piece API context which already does that.
    // But it is clearer to call the appropriate function.
    let modperm = self.modify_pieces_not_necessarily_saving_aux();

    let ig = &mut **self;
    let gpl = ig.gs.players.byid(player)?;
    let tgpc = ig.gs.pieces.byid_mut(tpiece)?;
    let tipc = ig.ipieces.get(tpiece).ok_or(Ia::PieceGone)?;

    let fs_record = (||{
      let fsid = tgpc.fastsplit?;
      ig.ifastsplits.table.get(fsid)
    })().ok_or_else(|| internal_logic_error("fastsplit on non-fastsplit"))?;

    // The new piece (the change, which stays in place) inherits the
    // old piece's Z (but we need to give it a new generation so that
    // we don't risk having two pieces with the precise same Z).  The
    // old piece becomes the amount taken and gets the specified Z.
    let npc_z = ZLevel { z: tgpc.zlevel.z.clone(), zg: ig.gs.gen };
    if ! (tpc_new_z.inspect() > &npc_z) {
      throw!(Ia::BadPieceStateForOperation);
    }

    let mut ngpc = GPiece {
      pos:           tgpc.pos,
      face:          tgpc.face,
      held:          None,
      zlevel:        npc_z,
      pinned:        tgpc.pinned,
      occult:        default(),
      angle:         tgpc.angle,
      gen:           ig.gs.gen,
      lastclient:    default(),
      last_released: default(),
      gen_before_lastclient: Generation(0),
      xdata:         default(),
      moveable:      tgpc.moveable,
      rotateable:    tgpc.rotateable,
      fastsplit:     tgpc.fastsplit,
    };

    let (t_pu, t_unprepared) = implementation(
      &ig.ioccults, &ig.gs.occults, gpl,
      tgpc, tipc,
      &mut ngpc
    )?;

    let ipc_from_record = fs_record.ipc.clone();

    // Committing.

    // This is outside the infallible closure because borrowck
    // can't see that we drop tgpc before doing stuff with ig.
    tpc_new_z.implement(tgpc);
    (||{
      let nipc = IFastSplits::make_ipc(&mut ig.ioccults.ilks, ipc_from_record);
      let npiece = ig.gs.pieces.as_mut(modperm).insert(ngpc);
      ig.ipieces.as_mut(modperm).insert(npiece, nipc);

      // "Recalculate" the occultation of the piece we're leaving behind.
      let ((), r_unprepared) = ToRecalculate::with(|mut to_permute| {
        let () = recalculate_occultation_general(
          &mut ig.gs.gen.unique_gen(),
          &mut ig.gs.players, &mut ig.gs.pieces,
          &mut ig.gs.occults, &ig.ipieces, &ig.ioccults,
          &mut to_permute, npiece,
          // We are treat this as a new piece, but it's not, semantically.
          // We don't log anything, since that new piece is not really
          // supposed to be interesting and the piece implwementation
          // is supposed to have logged whatever is needed.
          || (),
          (),
          |_old,_new,_desc| (),
          |_puopp, ()| (),
        ).unwrap_or_else(|e| {
          // not good
          error!("failed to recalculate after piece split! {:?}", &e);
        });
        ((), to_permute.implement(ig))
      });

      let n_unprepared = vec![(
        npiece,
        PUOs::Simple(PUO::Insert(())),
      )].into_unprepared(None);

      let unprepared = chain!(
        n_unprepared,
        r_unprepared,
        t_unprepared,
      ).collect();

      (t_pu, unprepared)
    })()// <- no ?, infallible (to avoid having not completed implementation
  }

  #[throws(IE)]
  pub fn fastsplit_delete(&mut self,
                          piece: PieceId,
                          #[allow(clippy::ptr_arg)]
                          _proof_that_caller_handles_logging: &Vec<LogEntry>)
                          -> (PieceUpdateOp<(),()>, UnpreparedUpdates)
  {
    let missing_e = || internal_error_bydebug(&piece);

    // See reasoning in fastsplit_split
    let modperm = self.modify_pieces_not_necessarily_saving_aux();

    let gpc = self.gs.pieces.get(piece).ok_or_else(missing_e)?;
    let ipc = self.ipieces.get(piece).ok_or_else(missing_e)?;

    // If we are merging or deleting here, presumably someone has
    // already checked that this is appropriate wrt occultations,
    // since we don't just do that willy-nilly and occultation will
    // have been checked when deciding *what* to merge with.
    let _p: &Piece = ipc.p.direct_trait_access().downcast_piece()?;

    let _fsid: &FastSplitId = gpc.fastsplit.as_ref()
      .ok_or_else(|| internal_error_bydebug(gpc))?;
    // We allow merging things with different FastSplitIds.  The
    // FastSplitId identifies not the "kind of piece", but simply
    // ancestry.  For example, all banknotes split from the same
    // original note will have the same id, but otherwise, different
    // banknotes even of the same currency have different ids.

    match ToRecalculate::with(|mut to_permute| {
      let r = self.delete_piece(modperm,&mut to_permute, piece,|_,_,_,_|());
      (r, to_permute.implement(self))
    }) {
      (Err(e), uu_p) => {
        PrepareUpdatesBuffer::only_unprepared(self, uu_p);
        throw!(e);
      },
      (Ok(((), puo, uu_d)), uu_p) => {
        (puo, chain!(uu_d, uu_p).collect())
      },
    }
  }
}

impl IFastSplits {
  /// During piece addition: make this into a new fastsplit piece family
  ///
  /// Do not just drop the result, because it contains an OwningOccultIlkId
  pub fn new_original(&mut self, ilks: &mut OccultIlks, ipc: IPiece)
                      -> (IPiece, FastSplitId) {
    let ipc = Arc::new(ipc);
    let record = Record { ipc: ipc.clone() };
    let fsid = self.table.insert(record);
    (Self::make_ipc(ilks, ipc), fsid)
  }

  /// During game load: recover a proper IPiece for a fastsplit piece
  ///
  /// Just dropping the result on error is ok, despite it containing
  /// an OwningOccultIlkId, because in that case the whole game is toast.
  #[throws(as Option)]
  pub fn recover_ipc(&self, ilks: &mut OccultIlks, fsid: FastSplitId)
                     -> IPiece {
    let record = self.table.get(fsid)?;
    Self::make_ipc(ilks, record.ipc.clone())
  }

  /// Do not just drop the result, because it contains an OwningOccultIlkId
  fn make_ipc(ilks: &mut OccultIlks, ipc: Arc<IPiece>) -> IPiece {
    let occilk = ipc.occilk.as_ref().map(|i| ilks.clone_iilk(i));
    let special = ipc.special.clone();
    let piece = Box::new(Piece { ipc: Some(ipc) });
    IPiece {
      p: IPieceTraitObj::new(piece as _),
      occilk, special,
    }
  }

  /// During save: free up any now-unused family records
  pub fn cleanup(&mut self, ilks: &mut OccultIlks) {
    self.table.retain(|_fsid, record| {
      if Arc::strong_count(&record.ipc) != 1 { return true; }
      let removed = mem::replace(record, Record::dummy());
      if_let!{
        Ok(ipc) = Arc::try_unwrap(removed.ipc);
        Err(busy) => {
          // someone upgraded a weak reference, since we have the only
          // strong one?  but we don't use weak references.  odd.
          *record = Record { ipc: busy };
          return true;
        }
      };
      if let Some(i) = ipc.occilk { ilks.dispose_iilk(i); }
      false
    })
  }
}

#[ext(pub)]
impl<'r> &'r dyn PieceTrait {
  #[throws(PieceTraitDowncastFailed<'r>)]
  fn downcast_piece_fastsplit<P: PieceTrait>(self) -> &'r P {
    self.downcast_piece::<Piece>()?
      .ipc.as_ref().ok_or_else(
        || PieceTraitDowncastFailed { p: self, why: "unresolved fastsplit"})?
      .p.direct_trait_access() // we're just digging down, this is fine
      .downcast_piece::<P>()?
  }
}
