// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use super::*; // we are otter::updates::movehist

pub const MOVEHIST_LENS: &[usize] = &[ 0, 1, 3, 10 ];
pub const MOVEHIST_LEN_MAX: usize = 10;
pub const MOVEHIST_LEN_DEF_I: usize = 1;

#[test]
fn movehist_lens() {
  assert_eq!(
    MOVEHIST_LENS.iter().max(),
    Some(&MOVEHIST_LEN_MAX),
  );
  assert!( MOVEHIST_LENS.get(MOVEHIST_LEN_DEF_I).is_some() );
  assert_eq!( MOVEHIST_LENS.iter().cloned().fold(None, |b, i| {
    let i = Some(i);
    assert!(i > b);
    i
  }),
              Some(MOVEHIST_LEN_MAX) );
}

#[derive(Debug,Copy,Clone,Serialize,Deserialize)]
pub struct MoveHistPosx { // usual variable: posx
  pub pos: Pos,
  pub angle: CompassAngle,
  pub facehint: Option<FaceId>,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct MoveHistEnt {
  pub held: PlayerId,
  pub posx: OldNew<MoveHistPosx>,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct GMoveHist {
  pub hist: VecDeque<MoveHistEnt>,
}

// ---------- non-public structs ----------

#[derive(Debug,Clone,Serialize,Deserialize,Default)]
pub struct GMoveHeld {
  held: SparseSecondaryMap<VisiblePieceId, GMoveHistLast>,
}

#[derive(Debug,Copy,Clone,Serialize,Deserialize)]
struct GMoveHistLast {
  held: PlayerId,
  posx: MoveHistPosx,
}

impl Default for GMoveHist {
  fn default() -> GMoveHist { GMoveHist {
    hist: VecDeque::with_capacity(MOVEHIST_LEN_MAX),
  } }
}

// We're track this on behalf of the client, based on the updates
// we are sending.  That means we don't ahve to worry about
// occultation, etc. etc.
pub fn peek_prep_update(gs: &mut GameState, peek: &PreparedUpdateEntry)
                        -> Option<PreparedUpdateEntry> {
  if_let!{ PUE::Piece(PUE_P { ops,.. }) = peek; else return None; }

  let mut pu = SecondarySlotMap::new();
  for (player, &PreparedPieceUpdate { ref op, piece,.. }) in ops { if_chain! {
    if let Some(ns) = op.new_state();
    if let Some(gpl) = wants!( gs.players.get_mut(player), ?player);
    if let Some(mut ent) = wants!( gpl.moveheld.held.entry(piece), ?piece);
    let &PreparedPieceState { pos, angle, facehint, .. } = ns;
    let new_posx = MoveHistPosx { pos, angle, facehint };

    then {
      if let slotmap::sparse_secondary::Entry::Occupied(ref mut oe) = ent {
        let last = oe.get();
        if ns.held == Some(last.held) { continue }

        // Generate an update
        let histent = MoveHistEnt {
          held: last.held,
          posx: OldNew::from([last.posx, new_posx]),
        };
        if gpl.movehist.hist.len() == MOVEHIST_LEN_MAX {
          gpl.movehist.hist.pop_front();
        }
        gpl.movehist.hist.push_back(histent.clone());
        pu.insert(player, histent);
      }

      if let Some(held) = ns.held {
        ent.insert(GMoveHistLast { held: held, posx: new_posx });
      } else {
        ent.remove();
      }
    }
  } }

  if pu.is_empty() { return None }

  Some(PUE::MoveHistEnt(pu))
}
