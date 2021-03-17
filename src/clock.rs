// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(dead_code)]

use crate::prelude::*;
use shapelib::Rectangle;

use nix::sys::time::TimeValLike as TVL;

// ==================== users ====================

struct UserInfo {
  idchar: char,
}

const N: usize = 2;

type Time = i32;

#[derive(Copy,Clone,Serialize,Deserialize)]
#[derive(Eq,Ord,PartialEq,PartialOrd,Hash)]
#[serde(try_from="u8", into="u8")]
struct User(bool);

impl<T> Index<User> for [T;2] {
  type Output = T;
  fn index(&self, index: User) -> &T { &self[index.0 as usize] }
}  
impl<T> IndexMut<User> for [T;2] {
  fn index_mut(&mut self, index: User) -> &mut T { &mut self[index.0 as usize] }
}  

const USERINFOS: [UserInfo; N] = [
  UserInfo { idchar: 'x' },
  UserInfo { idchar: 'y' },
];

const USERS: [User; N] = [ User(false), User(true) ];

impl fmt::Debug for User {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.write_char(USERINFOS[*self].idchar)
  }
}

#[derive(Debug,Clone,Copy,Error,Serialize,Deserialize)]
struct UserOutOfRangeError;
display_as_debug!{UserOutOfRangeError}

impl TryFrom<u8> for User {
  type Error = UserOutOfRangeError;
  #[throws(UserOutOfRangeError)]
  fn try_from(u: u8) -> User { User(match u {
    0 => false,
    1 => true,
    _ => throw!(UserOutOfRangeError),
  }) }
}

impl From<User> for u8 {
  fn from(user: User) -> u8 { user.0 as u8 }
}

// ==================== state ====================

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct ChessClock { // spec
  time: Time,
  #[serde(default)] per_move: usize,
}

#[derive(Debug,Serialize,Deserialize)]
struct Clock { // state
  spec: ChessClock,
}

#[derive(Debug,Serialize,Deserialize)]
struct State {
  users: [UState; 2],
  #[serde(skip)] running: Option<Running>,
}

#[typetag::serde(name="Hand")]
impl PieceXData for State {
  fn dummy() -> Self {
    State {
      users: [UState { player: default(), remaining: TVL::zero() }; N],
      running: None,
    }
  }
}


#[derive(Debug,Copy,Clone,Serialize,Deserialize)]
struct UState {
  player: PlayerId,
  #[serde(with="timespec_serde")] remaining: TimeSpec, // -ve means flag
}

#[derive(Debug,Serialize,Deserialize)]
struct Running {
  user: User,
}

mod timespec_serde {
  use super::*;

  #[derive(Serialize, Deserialize)]
  struct Timespec(i64, u32);

  #[throws(S::Error)]
  pub fn serialize<S:Serializer>(v: &TimeSpec, s: S) -> S::Ok {
    let v = Timespec(v.tv_sec().into(), v.tv_nsec().try_into().unwrap());
    Serialize::serialize(&v, s)?
  }
  #[throws(D::Error)]
  pub fn deserialize<'de, D:Deserializer<'de>>(d: D) -> TimeSpec {
    let Timespec(sec, nsec) = Deserialize::deserialize(d)?;
    libc::timespec { tv_sec: sec.into(), tv_nsec: nsec.into() }.into()
  }
}

impl ChessClock {
  fn time(&self) -> TimeSpec {
    TVL::seconds(self.time.into())
  }
}

// ==================== rendering, abstract ====================

#[derive(Debug)]
struct URender<'r> {
  st: URenderState,
  remaining: TimeSpec, // always >=0
  nick: &'r str,
}

#[derive(Debug,Copy,Clone)]
#[derive(Eq,Ord,PartialEq,PartialOrd,Hash)]
enum URenderState {
  Running,
  ActiveHeld,
  Inactive,
  Stopped,
  Reset,
  Flag,
  PMissing,
}
use URenderState as URS;

impl Clock {
  fn urender<'r>(&self, state: &State, gplayers: &'r GPlayers, gpc: &GPiece)
                 -> [URender<'r>; N]
  {
    let mut r: [URender;N] = izip!(
      USERS.iter(),
      state.users.iter()
    ).map(|(&user, ustate)| {
      let nick = gplayers.get(ustate.player).map(|gpl| gpl.nick.as_str());
      let (st, remaining, nick) =
        if ustate.remaining < TVL::zero() {
          (URS::Flag, TVL::zero(), nick.unwrap_or(""))
        } else if let Some(nick) = nick {
          (
            if let Some(running) = &state.running {
              if running.user != user {
                URS::Inactive
              } else if gpc.held.is_some() {
                URS::ActiveHeld
              } else {
                URS::Running
              }
            } else if ustate.remaining == self.spec.time() {
              URS::Reset
            } else {
              URS::Stopped
            }

            , ustate.remaining, nick
          )
        } else {
          (URS::PMissing, ustate.remaining, "")
        };

      URender { st, remaining, nick }
    })
      .collect::<ArrayVec<_>>()
      .into_inner().unwrap();

    if r.iter().filter(|ur| ur.st == URS::Reset).count() == 1 {
      for ur in &mut r {
        if ur.st == URS::Reset { ur.st = URS::Stopped }
      }
    }

    r
  }
}

// ==================== rendering ====================

const W: Coord = 50;
const H: Coord = 20;
const OUTLINE: Rectangle = Rectangle { xy: PosC([W as f64, H as f64]) };


// ==================== piece management, loading, etc. ====================

#[typetag::serde]
impl PieceSpec for ChessClock {
  #[throws(SpecError)]
  fn load(&self, _: usize, _gpc: &mut GPiece) -> PieceSpecLoaded {
    if self.time <= 0 { throw!(SpecError::NegativeTimeout) }

    let clock = Clock {
      spec: self.clone(),
    };
    PieceSpecLoaded {
      p: Box::new(clock),
      occultable: None,
    }
  }
}

#[dyn_upcast]
impl OutlineTrait for Clock {
  delegate!{
    to OUTLINE {
      fn outline_path(&self, scale: f64) -> Result<Html, IE>;
      fn thresh_dragraise(&self) -> Result<Option<Coord>, IE>;
      fn bbox_approx(&self) -> Result<[Pos;2], IE>;
    }
  }
}

#[typetag::serde(name="ChessClock")]
impl PieceTrait for Clock {
  fn nfaces(&self) -> RawFaceId { 1 }

  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, _gpc: &GPiece, _gs: &GameState, id: VisiblePieceId) {
    dbgc!("rendering", id);
    write!( &mut f.0, r##"
        <rect fill="white" stroke="black" width="{}" height="{}"/>
    "##, W, H
    )?;
/*
    let urs = 

    dbg
    for (i,u) in USERS.iter().enumerate() {
      
    }*/
  }

  #[throws(IE)]
  fn describe_html(&self, _gpc: &GPiece) -> Html {
    Html::lit("the chess clock")
  }

  fn itemname(&self) -> &str { "chess-clock" }
}
