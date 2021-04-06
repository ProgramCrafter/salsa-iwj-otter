// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// xxx  mao needs updating so Hang is non-awful
// xxx  need way to select hand when it is totally covered

use crate::prelude::*;

const MARGIN_INSIDE: Coord = 1;
const HANG_INSIDE:   Coord = 2;

const HANG_TILE_SHOW: Pos = PosC::new(4,8);

const INTUIT_SORT_Y_THRESH: Coord = 2;

#[throws(InternalError)]
pub fn add_ui_operations(upd: &mut Vec<UoDescription>,
                         region: &Rect) {
  if (region.br() - region.tl())?.coords.iter().any(
    |&c| c < HANG_INSIDE*2
  ) {
    // too small!
    return;
  }

  upd.push(UoDescription {
    kind: UoKind::Piece,
    def_key: 'o',
    opname: "organise".to_string(),
    desc: Html::lit("Organise").into(),
    wrc: WRC::Predictable,
  });
  upd.push(UoDescription {
    kind: UoKind::Piece,
    def_key: 'O',
    opname: "organise-sort".to_string(),
    desc: Html::lit("Organise and Sort").into(),
    wrc: WRC::Predictable,
  });
}

define_index_type!{ struct InHand = usize; }

#[derive(Copy,Clone,Debug,Ord,PartialOrd,Eq,PartialEq)]
#[derive(EnumIter)]
enum Attempt {
  Nonoverlap,
  Inside,
  Abut,
  AbutCompr,
  Hanging,
}
use Attempt as A;

impl Attempt {
  #[throws(CoordinateOverflow)]
  fn tl(self, bbox: &Rect) -> Pos {
    let cnr = bbox.tl();
    match self {
      A::Nonoverlap |
      A::Inside     => (cnr - PosC::both(MARGIN_INSIDE))?,
      A::Abut       |
      A::AbutCompr  =>  cnr,
      A::Hanging    =>  PosC::both(-HANG_INSIDE),
    }
  }

  #[throws(CoordinateOverflow)]
  fn br_real(self, bbox: &Rect) -> Pos {
    let cnr = bbox.br();
    match self {
      A::Nonoverlap |
      A::Inside     => (cnr + PosC::both(MARGIN_INSIDE))?,
      A::Abut       |
      A::AbutCompr  =>  cnr,
      A::Hanging    =>  PosC::both(HANG_INSIDE),
    }
  }

  #[throws(CoordinateOverflow)]
  fn br_tile(self, bbox: &Rect) -> Pos {
    let cnr = bbox.br();
    match self {
      A::Nonoverlap => (cnr + PosC::both(MARGIN_INSIDE))?,
      A::Inside     |
      A::Abut       => (bbox.tl() - bbox.tl().map(|v| v/ 2 ))?,
      A::AbutCompr  => (bbox.tl() - bbox.tl().map(|v| v/ 3 ))?,
      A::Hanging    => (HANG_TILE_SHOW - PosC::both(HANG_INSIDE))?,
    }
  }
}     

struct PrimaryEnt<'pe> {
  sortkey: Option<&'pe str>,
  piece: PieceId,
  pos: Pos,
  bbox: Rect,
}

type Primary<'pe> = IndexVec<InHand, PrimaryEnt<'pe>>;
type ZLevels = IndexVec<InHand, ZLevel>;
type OrderTable = IndexVec<InHand, InHand>;

#[throws(PieceOpError)]
fn recover_order(region: &Rect, pieces: &Primary, zlevels: &ZLevels)
                -> OrderTable
{
  // This is tricky.  We are trying to recover "the order" so that we
  // can "just tidy it up".  Also we want this to be stable if the
  // algorithm is rerun, and to insert any manually added pieces in
  // the right place.
  //
  // What we do is try to recover a reading order as follows.  At each
  // stage we have a "current" position, which starts out as the left
  // hand side ad the minimum y.
  //
  // Then we try to progress in reading order, defined as follows: The
  // next piece is the one which best matches the following criteria:
  //    * no more than THRESH below the last one, and not to the left of it
  //    * leftmost
  //    * northernmost
  //    * minimum z coordinate

  // This algorithm is quadratic.  320^2 = 102K
  let len = pieces.len();
  if len > 320 { throw!(POE::OrganisedPlacementOverfull) }

  let mut remain: Vec<InHand> = (0..len).map(Into::into).collect();
  let mut out = index_vec![];

  let mut last = PosC::new(
    region.tl().x(),
    if let Some(min_y) = pieces.iter().map(|ent| ent.pos.y()).min() {
      min_y
    } else {
      return out; // nothing!
    }
  );

  while let Some((inremain, &ih)) =
    remain.iter().enumerate()
    .min_by_key(|(_inremain, &ih)| {
      let p = &pieces[ih];
      let sortkey = &p.sortkey;
      let pos = &p.pos;
      let zlevel = &zlevels[ih];
      let in_rect =
        pos.x() >= last.x() &&
        pos.y() <= last.y() + INTUIT_SORT_Y_THRESH;
      ( sortkey,
        ! in_rect,
        pos.x(),
        pos.y(),
        zlevel )
    }) {
      last = pieces[ih].pos;
      out.push(ih);
      remain.swap_remove(inremain);
    }

  out
}

#[throws(InternalError)]
fn try_layout(region: &Rect,
              order: &OrderTable,
              pieces: &IndexVec<InHand, PrimaryEnt>,
              att: Attempt)
              -> Option<IndexVec<InHand, Pos>> {
  let mut out = default();
  if pieces.is_empty() { return Some(out) }

  trace_dbg!("attempt", region, att, pieces.len(), region.size()?);

  let mut cur = region.tl();
  let mut n_y = region.tl().y();
  // Invariant:
  // Everything below n_y is overwriteable
  // Everything below and to the right of cur is overwriteable

  for &ih in order {
    let PrimaryEnt { piece, bbox, .. } = &pieces[ih];
    let place = 'placed: loop {
      for xi in 0..3 {
        let place = (cur - att.tl(&bbox)?)?;
        let br_real = (place + att.br_real(&bbox)?)?;
        let tr = |w| {
          trace_dbg!("attempt inner",
                     region, att, piece, bbox, xi, cur, n_y,
                     place, br_real, w);
        };
        if br_real.x() > region.br().x() {
          tr("EOL");
          cur = PosC::new(
            region.tl().x(),
            n_y,
          );
        } else if br_real.y() > region.br().y() {
          tr("NOSPC");
          if ! matches!(att, A::Hanging) { return None }
          cur = PosC::new(
            region.tl().x(),
            region.br().y().checked_sub(HANG_INSIDE)
              .ok_or(CoordinateOverflow)?,
          );
          n_y = cur.y();
          continue;
        } else {
          tr("placed");
          break 'placed place;
        }
      }
      throw!(IE::OrganisedPlacementFailure);
    };
    let br_tile = (place + att.br_tile(&bbox)?)?;
    cur.coords[0] = br_tile.x();
    n_y = max(n_y, br_tile.y());
    out.push(place);
  }
  Some(out)
}


#[throws(ApiPieceOpError)]
pub fn ui_operation(a: &mut ApiPieceOpArgs<'_>, opname: &str,
                    _wrc: WhatResponseToClientOp, region: &Rect)
                    -> Option<UpdateFromOpComplex> {
  let do_sort = match opname {
    "organise" => false,
    "organise-sort" => true,
    _ => return None,
  };
  let ApiPieceOpArgs { ref mut gs, player,ipieces,ioccults,.. } = *a;
  let apiece = a.piece;
  let agpc = gs.pieces.byid(apiece)?;
  let aipc = ipieces.get(apiece).ok_or(internal_error_bydebug(&apiece))?;
  let gpl = gs.players.byid(player)?;
  let log = log_did_to_piece(ioccults, &gs.occults, gpl,agpc,aipc,
                             "organised")?;

  let (pieces, mut zlevels) =
    gs.pieces.iter().filter_map(|(piece, gpc)| if_chain!
  {
    if region.contains(gpc.pos);
    if gpc.held.is_none();
    if ! gpc.pinned;
    if let PieceMoveable::Yes = gpc.moveable();
    if let Some(ipc) = wants!( ipieces.get(piece), ?piece );
    if let Some(vis) = gpc.fully_visible_to(&gs.occults, player);
    let ipc = ipc.show(vis);
    if let Some(bbox) = want!( Ok = ipc.bbox_approx(), ?piece );
    let sortkey = if do_sort {
      Some(ipc.sortkey().unwrap_or(ipc.itemname()))
    } else {
      None
    };
    then {
      Some((
        PrimaryEnt { piece, bbox, sortkey, pos: gpc.pos },
        gpc.zlevel.clone())
      )
    }
    else {
      None
    }
  }).unzip::<
    _,_,
    IndexVec<InHand, PrimaryEnt>,
    IndexVec<InHand, ZLevel>,
  >();

  let order = recover_order(region, &pieces, &zlevels)?;

  zlevels.sort();

  let layout = 'laid_out: loop {
    for att in Attempt::iter() {
      if let Some(layout) = try_layout(region, &order, &pieces, att)? {
        break 'laid_out layout;
      }
    }
    throw!(internal_error_bydebug(region));
  };

  for &pos in &layout {
    // Some sanity checks
    if pos.clamped(gs.table_size).is_err() {
      throw!(APOE::ReportViaUpdate(POE::PosOffTable))
    }
    match gs.occults.pos_occulter(&gs.occults, pos)? {
      None => {},
      Some(occulter) if occulter == apiece => {},
      Some(_) => throw!(APOE::ReportViaUpdate(POE::Occultation)),
    };
  }

  // point of no return
  (||{
    let updates = {
      let mut updates = Vec::with_capacity(pieces.len());

      for (ih, pos, zlevel) in
        izip!(order, layout, zlevels)
      {
        let PrimaryEnt { piece, .. } = pieces[ih];

        want_let!{ Some(gpc) = gs.pieces.get_mut(piece); else continue; }
        gpc.pos = pos;
        gpc.zlevel = zlevel;
        updates.push((piece, PUOs::Simple(PUO::MoveQuiet(pos))));
        updates.push((piece, PUOs::Simple(PUO::SetZLevelQuiet(()))));
      }

      updates
    };

    Some((PieceUpdate {
      wrc: WRC::Predictable,
      log,
      ops: PUOs::PerPlayer(default()),
    }, updates.into_unprepared(None)))
  })() // <- no ?, shows it's infallible
}
