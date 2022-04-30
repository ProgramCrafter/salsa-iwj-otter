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

#[dyn_upcast]
impl OutlineTrait for Piece {
  ambassador_impl_OutlineTrait_body_single_struct!{ inner() }
}
#[dyn_upcast]
impl PieceBaseTrait for Piece {
  ambassador_impl_PieceBaseTrait_body_single_struct!{ inner() }
}
#[typetag::serde(name="FastSplit")]
impl PieceTrait for Piece {
  ambassador_impl_PieceTrait_body_single_struct!{ inner() }
}

impl Record {
  fn dummy() -> Record {
    let p = Piece { ipc: None };
    let p = IPieceTraitObj::new(Box::new(p) as _);
    let ipc = IPiece { p, occilk: None, special: default() };
    Record { ipc: Arc::new(ipc) }
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
