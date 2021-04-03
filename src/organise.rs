// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// xxx  mao needs updating so Hang is non-awful
// xxx  need way to select hand when it is totally covered

use crate::prelude::*;

const MARGIN_INSIDE: Coord = 1;
const HANG_INSIDE:   Coord = 2;

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
      A::Hanging    =>  PosC::both(HANG_INSIDE),
    }
  }
}     

#[throws(InternalError)]
fn try_layout(region: &Rect,
              pieces: &IndexVec<InHand, (Rect, PieceId)>,
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

  for (bbox, piece) in pieces {
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
  let _do_sort = match opname {
    "organise" => (),
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
    if let Some(bbox) = want!( Ok = ipc.show(vis).bbox_approx(), ?piece );
    then {
      Some((
        (bbox, piece),
        gpc.zlevel.clone())
      )
    }
    else {
      None
    }
  }).unzip::<
    _,_,
    IndexVec<InHand, (Rect, PieceId)>,
    IndexVec<InHand, ZLevel>,
  >();

  zlevels.sort();

  let layout = 'laid_out: loop {
    for att in Attempt::iter() {
      if let Some(layout) = try_layout(region, &pieces, att)? {
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

      for ((_bbox, piece), pos, zlevel) in izip!(pieces, layout, zlevels) {
        want_let!{ Some(gpc) = gs.pieces.get_mut(piece); else continue; }
        gpc.pos = pos;
        gpc.zlevel = zlevel;
        updates.push((piece, PUOs::Simple(PUO::Move(pos))));
        updates.push((piece, PUOs::Simple(PUO::SetZLevel(()))));
      }

      updates
    };

    Some((PieceUpdate {
      wrc: WRC::Predictable,
      log,
      ops: PUOs::PerPlayer(default()),
    }, updates.into_unprepared_nc()))
  })() // <- no ?, shows it's infallible
}
