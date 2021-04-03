// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use super::*; // we are otter::updates::movehist

pub const LENS: &[usize] = &[ 0, 1, 3, 10 ]; // xxx want option with no name
pub const LEN_MAX: usize = 10;
pub const LEN_DEF_I: usize = 1;
pub const MIN_DIST: f64 = 7.5;

#[test]
fn movehist_lens() {
  assert_eq!(
    LENS.iter().max(),
    Some(&LEN_MAX),
  );
  assert!( LENS.get(LEN_DEF_I).is_some() );
  assert_eq!( LENS.iter().cloned().fold(None, |b, i| {
    let i = Some(i);
    assert!(i > b);
    i
  }),
              Some(LEN_MAX) );
}

#[derive(Debug,Copy,Clone,Serialize,Deserialize)]
pub struct Posx { // usual variable: posx
  pub pos: Pos,
  pub angle: CompassAngle,
  pub facehint: Option<FaceId>,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct Ent {
  pub held: PlayerId,
  pub posx: OldNew<Posx>,
  pub diff: DiffToShow,
}

#[derive(Debug,Copy,Clone,Serialize,Deserialize)]
pub enum DiffToShow {
  Moved { d: f64 },
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct PlHist {
  pub hist: VecDeque<Ent>,
}

// ---------- non-public structs ----------

#[derive(Debug,Clone,Serialize,Deserialize,Default)]
pub struct PlHeld {
  held: SparseSecondaryMap<VisiblePieceId, PlHistLast>,
}

#[derive(Debug,Copy,Clone,Serialize,Deserialize)]
struct PlHistLast {
  held: PlayerId,
  posx: Posx,
}

impl Default for PlHist {
  fn default() -> PlHist { PlHist {
    hist: VecDeque::with_capacity(LEN_MAX),
  } }
}

impl Posx {
  fn differs_significantly(&self, other: &Posx)
                           -> Option<DiffToShow> {
    use DiffToShow as D;

    match (|| Ok::<_,CoordinateOverflow> ({
      (self.pos - other.pos)?.len()?
    }))() {
      Ok(d) if d >= MIN_DIST as f64 => return Some(D::Moved{ d }),
      _ => {},
    }

    None
  }
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
    let new_posx = Posx { pos, angle, facehint };

    then {
      if let slotmap::sparse_secondary::Entry::Occupied(ref mut oe) = ent {
        let last = oe.get();
        if ns.held == Some(last.held) { continue }

        if let Some(diff) = new_posx.differs_significantly(&last.posx) {
          // Generate an update
          let histent = Ent {
            held: last.held,
            posx: OldNew::from([last.posx, new_posx]),
            diff,
          };
          if gpl.movehist.hist.len() == LEN_MAX {
            gpl.movehist.hist.pop_front();
          }
          gpl.movehist.hist.push_back(histent.clone());
          pu.insert(player, histent);
        }
      }

      if let Some(held) = ns.held {
        ent.insert(PlHistLast { held: held, posx: new_posx });
      } else {
        ent.remove();
      }
    }
  } }

  if pu.is_empty() { return None }

  Some(PUE::MoveHistEnt(pu))
}
