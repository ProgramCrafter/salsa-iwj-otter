// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;
use shapelib::RectShape;

use nix::sys::time::TimeValLike as TVL;

// ========== definitions ==========

const N: usize = 2;

type Time = i32; // make humantime serde

// ==================== state ====================

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct Spec {
  time: Time,
  #[serde(default)] per_move: Time,
}

#[derive(Debug,Serialize,Deserialize)]
struct Clock { // PieceTrait
  spec: Spec,
}

#[derive(Debug,Serialize,Deserialize)]
struct State {
  users: [UState; 2],
  current: Option<Current>,
  #[serde(skip)] notify: Option<mpsc::Sender<()>>,
  #[serde(skip)] running: Option<Running>,
}


#[derive(Debug,Copy,Clone,Serialize,Deserialize)]
struct UState {
  player: PlayerId,
  #[serde(with="timespec_serde")] remaining: TimeSpec, // -ve means flag
}

#[derive(Debug,Copy,Clone,Eq,PartialEq,Serialize,Deserialize)]
struct Current {
  user: User,
}

#[derive(Debug,Clone,Copy)]
struct Running {
  expires: TimeSpec,
}

impl Spec {
  fn initial_time(&self) -> TimeSpec { TVL::seconds(self.time.into()) }
  fn per_move(&self) -> TimeSpec { TVL::seconds(self.per_move.into()) }
  fn initial(&self) -> [TimeSpec; N] {
    // White is player Y, and they will ge to go first, so the clock
    // will go from stopped to Y, and then later when it's X's turn
    // X will get an extra per_move.  Y therefore needs per_move too.
    [
      self.initial_time() + self.per_move(),
      self.initial_time(),
    ]
  }
}

impl State {
  fn new(spec: &Spec) -> Self {
    let mut state = State::dummy();
    state.reset(spec);
    state
  }

  fn reset(&mut self, spec: &Spec) {
    for (ust, t) in izip!(&mut self.users, spec.initial().iter().copied()) {
      ust.remaining = t;
    }
  }

  fn any_expired(&self) -> bool {
    self.users.iter().any(|ust| ust.remaining < TVL::zero())
  }

  fn implies_running(&self, held: Option<PlayerId>) -> Option<User> {
    if_chain! {
      if let Some(Current { user }) = self.current;
      if held.is_none();
      if ! self.any_expired();
      then { Some(user) }
      else { None }
    }
  }
}

#[typetag::serde(name="ChessClock")]
impl PieceXData for State {
  fn dummy() -> Self {
    State {
      users: [UState { player: default(), remaining: TVL::zero() }; N],
      current: None,
      notify: None,
      running: None,
    }
  }
}


// ==================== users ====================

struct UserInfo {
  idchar: char,
}

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

impl fmt::Display for User {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.write_char(USERINFOS[*self].idchar)
  }
}
impl fmt::Debug for User {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "User({})", self)
  }
}
hformat_as_display!{User}

#[derive(Debug,Clone,Copy,Error,Serialize,Deserialize)]
struct BadClockUserError;
display_as_debug!{BadClockUserError}

impl TryFrom<u8> for User {
  type Error = BadClockUserError;
  #[throws(BadClockUserError)]
  fn try_from(u: u8) -> User { User(match u {
    0 => false,
    1 => true,
    _ => throw!(BadClockUserError),
  }) }
}

impl TryFrom<char> for User {
  type Error = BadClockUserError;
  #[throws(BadClockUserError)]
  fn try_from(c: char) -> User { User(match c {
    'x' | 'X' => false,
    'y' | 'Y' => true,
    _ => throw!(BadClockUserError),
  }) }
}

impl From<User> for u8 {
  fn from(user: User) -> u8 { user.0 as u8 }
}

impl std::ops::Not for User {
  type Output = User;
  fn not(self) -> User { User(! self.0) }
}

// ==================== rendering, abstract ====================

#[derive(Debug)]
struct URender<'r> {
  st: URenderState,
  remaining: TimeSpec, // always >=0
  nick: Option<&'r str>,
}

#[derive(Debug,Copy,Clone)]
#[derive(Eq,Ord,PartialEq,PartialOrd,Hash)]
enum URenderState {
  Running,
  ActiveHeld,
  Inactive,
  OtherFlag,
  Stopped,
  Reset,
  Flag,
}
use URenderState as URS;

impl Clock {
  fn urender<'r>(&self, state: &State, held: Option<PlayerId>,
                 gplayers: &'r GPlayers) -> [URender<'r>; N]
  {
    let mut r: [URender;N] = izip!(
      USERS.iter(),
      state.users.iter(),
      self.spec.initial().iter().copied(),
    ).map(|(&user, ustate, initial)| {
      let nick = gplayers.get(ustate.player)
        .map(|gpl| gpl.nick.as_str());
      let (st, remaining) =
        if ustate.remaining < TVL::zero() {
          (URS::Flag, TVL::zero())
        } else {
          (
            if let Some(current) = &state.current {
              if current.user != user {
                URS::Inactive
              } else if state.any_expired() {
                URS::OtherFlag
              } else if held.is_some() {
                URS::ActiveHeld
              } else {
                URS::Running
              }
            } else if ustate.remaining == initial {
              URS::Reset
            } else {
              URS::Stopped
            }

            , ustate.remaining
          )
        };

      URender { st, remaining, nick }
    })
      .collect::<ArrayVec<_,2>>()
      .into_inner().unwrap();

    if r.iter().filter(|ur| ur.st == URS::Reset).count() == 1 {
      for ur in &mut r {
        if ur.st == URS::Reset { ur.st = URS::Stopped }
      }
    }

    r
  }
}

// ==================== running ====================

impl State {
  #[throws(IE)]
  fn do_start_or_stop(&mut self, piece: PieceId,
                      was_current: Option<Current>,
                      was_implied_running: Option<User>,
                      held: Option<PlayerId>,
                      spec: &Spec,
                      ig: &InstanceRef) {
    let state = self;

    if_chain! {
      if let Some(was_current) = was_current;
      if let Some(now_current) = state.current;
      if now_current != was_current;
      if ! state.any_expired();
      then {
        let remaining = &mut state.users[now_current.user].remaining;
        *remaining = *remaining + spec.per_move();
      }
    }

    if state.implies_running(held) == was_implied_running { return }

    let now = now()?;

    if_chain! {
      if let Some(was_running_user) = was_implied_running;
      if let Some(Running { expires }) = state.running;
      then { 
        state.users[was_running_user].remaining = expires - now;
      }
    }

    if_chain! {
      if let Some(now_running_user) = state.implies_running(held);
      then {
        let expires = now + state.users[now_running_user].remaining;
        state.running = Some(Running { expires });
      }
    }

    state.notify.get_or_insert_with(||{
      let (tx,rx) = mpsc::channel();
      let ts = ThreadState {
        ig: ig.downgrade_to_weak(),
        piece,
        notify: rx,
        next_wakeup: Some(now),
      };
      thread::spawn(move || {
        ts.run()
          .unwrap_or_else(|e| error!("clock thread failed: {:?}", e));
      });
      tx
    })
      .send(())
      .unwrap_or_else(|e| error!("clock send notify failed: {:?}", e));
  }
}

#[throws(IE)]
fn now() -> TimeSpec {
  clock_gettime(CLOCK_MONOTONIC).context("clock_gettime")?
}

#[derive(Debug)]
struct ThreadState {
  ig: InstanceWeakRef,
  piece: PieceId,
  notify: mpsc::Receiver<()>,
  next_wakeup: Option<TimeSpec>,
}

impl ThreadState {
  #[throws(IE)]
  fn run(mut self) {
    loop {
      match self.next_wakeup {
        Some(wakeup) => {
          let timeout = wakeup - now()?;
          if timeout > TVL::zero() {
            let timeout =
              Duration::from_nanos(timeout.tv_nsec() as u64) +
              Duration::from_secs(timeout.tv_sec() as u64);

            use mpsc::RecvTimeoutError::*;
            match self.notify.recv_timeout(timeout) {
              Err(Disconnected) => break,
              Err(Timeout) => { },
              Ok(()) => { },
            }
          }
        }
        None => {
          match self.notify.recv() {
            Err(mpsc::RecvError) => break,
            Ok(()) => { },
          }
        }
      };

      let ig = match self.ig.upgrade() {
        Some(ig) => ig,
        None => break,
      };
      let mut ig = ig.lock().context("relocking game in clock")?;

      let gpc = ig.gs.pieces.get_mut(self.piece);
      let gpc = if let Some(gpc) = gpc { gpc } else { break };
      let held = gpc.held;
      let state: &mut State = gpc.xdata_mut(|| State::dummy())?;

      self.next_wakeup =
        if let Some(user) = state.implies_running(held) {
          let now = now()?;
          let remaining = state.running.ok_or_else(
            || internal_error_bydebug(&state)
          )?.expires - now;
          state.users[user].remaining = remaining;
          let pause: TimeSpec = libc::timespec {
            tv_sec: 0,
            tv_nsec: remaining.tv_nsec(),
          }.into();
          Some(pause + now)
        } else {
          None
        };

      PrepareUpdatesBuffer::spontaneous_image(&mut ig, self.piece, None)?;
    }
  }
}

// ==================== rendering ====================

const W: Coord = 40;
const H: Coord = 14;
const OUTLINE: RectShape = RectShape { xy: PosC::new(W as f64, H as f64) };


// ==================== piece management, loading, etc. ====================

fn unprepared_update(piece: PieceId) -> UnpreparedUpdates {
  Some(Box::new(move |buf: &mut PrepareUpdatesBuffer| {
    buf.piece_update_image(piece, &None)
      .unwrap_or_else(|e| error!("failed to prep clock: {:?}", e));
  }))
}

#[typetag::serde(name="ChessClock")]
impl PieceSpec for Spec {
  #[throws(SpecError)]
  fn load(&self, PLA { gpc,.. }: PLA) -> SpecLoaded {
    if self.time <= 0 { throw!(SpecError::NegativeTimeout) }

    let clock = Clock {
      spec: self.clone(),
    };

    gpc.xdata_init(State::new(self))?;

    SpecLoaded {
      p: Box::new(clock),
      occultable: None,
      special: default(),
    }
  }
}

#[dyn_upcast]
impl OutlineTrait for Clock {
  delegate!{
    to OUTLINE {
      fn outline_path(&self, scale: f64) -> Result<Html, IE>;
      fn thresh_dragraise(&self) -> Result<Option<Coord>, IE>;
      fn bbox_approx(&self) -> Result<Rect, IE>;
    }
  }
}

#[dyn_upcast]
impl PieceBaseTrait for Clock {
  fn nfaces(&self) -> RawFaceId { 1 }

  fn itemname(&self) -> &str { "chess-clock" }
}

#[typetag::serde(name="ChessClock")]
impl PieceTrait for Clock {
  #[throws(IE)]
  fn svg_piece(&self, f: &mut Html, gpc: &GPiece, gs: &GameState,
               vpid: VisiblePieceId) {
    let state = gpc.xdata()?
      .ok_or_else(|| internal_logic_error("missing/wrong xdata"))?;
    let urenders = self.urender(state, gpc.held, &gs.players);

    // player missing, nick is red and pink

    const Y: &[f32] = &[ 7., 0. ];

    struct Show {
      text:       &'static str,
      background: &'static str,
      sigil:      &'static str,
    }

    impl URenderState {
      fn show(self) -> Show {
        use URS::*;
        let (text, background, sigil) = match self {
          Running    => ("black",  "yellow",     "&#x25b6;" /* >  */ ),
          ActiveHeld => ("black",  "yellow",     "&#x2016;" /* || */ ),
          OtherFlag  => ("black",  "yellow",     ":"                 ),
          Inactive   => ("black",  "white",      ":"                 ),
          Stopped    => ("black",  "lightblue",  "&#x25a1;" /* [] */ ),
          Reset      => ("black",  "lightgreen", "&#x25cb;" /* O  */ ),
          Flag       => ("white",  "red",        "&#x2691;" /* F  */ ),
        };
        Show { text, background, sigil }
      }
    }
    
    hwrite!(f, r##"
<g transform="translate(-20,-7)">"##,
    )?;
    for (y, u) in izip!(Y.iter(), urenders.iter()) {
      hwrite!(f, r##"
  <rect y="{}" fill="{}" width="40" height="7"/>"##,
              y,
              Html::lit(u.st.show().background),
      )?;
    }
    hwrite!(f, r##"
  <rect fill="none" stroke="black" width="40" height="14"></rect>
  <clipPath id="def.{}.cl"><rect width="40" height="14"></rect></clipPath>"##,
            &vpid
    )?;
    for (user, y, u) in izip!(USERS.iter(), Y.iter(), urenders.iter()) {
      let y = y + 6.;
      let show = u.st.show();
      let mins = u.remaining.tv_sec() / 60;
      let secs = u.remaining.tv_sec() % 60;
      let mins = mins.to_string();
      let mins_pad = Html::from_html_string("&nbsp;".repeat(3 - mins.len()));

      let font = monospace_font(6);
      hwrite!(f, r##"
  <{} x="1" y="{}" {} fill="{}" >{}{}{}</text>"##,
             HTML_TEXT_LABEL_ELEM_START,
             y, font, Html::lit(show.text),
             mins_pad, HtmlStr::from_html_str(&mins), Html::lit(show.sigil)
      )?;
      hwrite!(f, r##"
  <{} x="14" y="{}" {} fill="{}" >{:02}</text>"##,
             HTML_TEXT_LABEL_ELEM_START,
             y, font, Html::lit(show.text),
             secs
      )?;
      let nick_y = y - 0.5;
      if let Some(nick) = u.nick {
        hwrite!(f, r##"
  <{} x="21" y="{}" fill="{}" clip-path="url(#def.{}.cl)" 
   font-size="4">{}</text>
              "##,
               HTML_TEXT_LABEL_ELEM_START,
               nick_y, show.text,
               vpid,
               nick,
        )?;
      } else {
        hwrite!(f, r##"
  <{} x="27" y="{}" fill="pink" stroke="red"
   stroke-width="0.1" font-size="4">({})</text>"##,
               HTML_TEXT_LABEL_ELEM_START,
               nick_y, user,
        )?;
      }
    }
    hwrite!(f, r##"
</g>"##)?;
  }

  #[throws(IE)]
  fn describe_html(&self, _gpc: &GPiece, _goccults: &GameOccults) -> Html {
    Html::lit("the chess clock").into()
  }

  #[throws(InternalError)]
  fn add_ui_operations(&self, _: ShowUnocculted, upd: &mut Vec<UoDescription>,
                       gs: &GameState, gpc: &GPiece) {
    let state: &State = gpc.xdata_exp()?;

    let for_users = || izip!(&USERS, &USERINFOS, &state.users).map(
      |(&user, userinfo, ust)| {
        let upchar = userinfo.idchar.to_ascii_uppercase();
        let upchar = IsHtmlFormatted(upchar);
        (user, userinfo, ust, upchar)
      }
    );

    for (user, userinfo, _ust, upchar) in for_users() {
      if state.current.as_ref().map(|c| c.user) != Some(user) {
        upd.push(UoDescription {
          kind: UoKind::Piece,
          def_key: userinfo.idchar,
          opname: format!("start-{}", userinfo.idchar),
          desc: if state.current.is_none() {
            hformat!("Start, with player {}", &upchar)
          } else {
            hformat!("Make player {} current", &upchar)
          },
          wrc: WRC::Predictable,
        });
      }
    }

    if state.current.is_some() {
      upd.push(UoDescription {
        kind: UoKind::Piece,
        def_key: 'S',
        opname: "stop".to_string(),
        desc: Html::lit("Stop").into(),
        wrc: WRC::Predictable,
      });
    }
    if state.current.is_none() {
      upd.push(UoDescription {
        kind: UoKind::Piece,
        def_key: 'R',
        opname: "reset".to_string(),
        desc: Html::lit("Reset").into(),
        wrc: WRC::Unpredictable,
      });
    }

    for (_user, userinfo, ust, upchar) in for_users() {
      if let Some(_gpl) = gs.players.get(ust.player) {
        upd.push(UoDescription {
          kind: UoKind::Piece,
          def_key: upchar.0,
          opname: format!("unclaim-{}", userinfo.idchar),
          desc: hformat!("Clear player {}", &upchar),
          wrc: WRC::Unpredictable,
        });
      } else {
        upd.push(UoDescription {
          kind: UoKind::Piece,
          def_key: upchar.0,
          opname: format!("claim-{}", userinfo.idchar),
          desc: hformat!("Become player {}", &upchar),
          wrc: WRC::Unpredictable,
        });
      }
    }
  }

  #[throws(ApiPieceOpError)]
  fn ui_operation(&self, _: ShowUnocculted, args: ApiPieceOpArgs<'_>,
                  opname: &str, _wrc: WhatResponseToClientOp)
                  -> OpOutcomeThunk {
    let ApiPieceOpArgs { gs,piece,player,ioccults,ipc,ig,.. } = args;
    let gpc = gs.pieces.byid_mut(piece)?;
    let held = gpc.held;
    let gpl = gs.players.byid(player)?;
    let state: &mut State = gpc.xdata_mut_exp()?;
    let get_user = || opname.chars().next_back().unwrap().try_into().unwrap();

    enum Howish {
      UniversalImage,
      Unpredictable,
    }
    use Howish::*;

    let was_current = state.current;
    let was_implied_running = state.implies_running(held);

    let (howish,did) = match opname {
      "start-x" | "start-y" => {
        let user = get_user();
        state.current = Some(Current { user });
        (UniversalImage, format!("activated player {} at the", user))
      },
      "stop" => {
        state.current = None;
        (UniversalImage, format!("stopped"))
      },
      "reset" => {
        if state.current.is_some() {
          throw!(Ia::BadPieceStateForOperation);
        }
        state.reset(&self.spec);
        (Unpredictable, format!("reset"))
      },
      "claim-x" | "claim-y" => {
        let user = get_user();
        if let Some(_gpl) = gs.players.get(state.users[user].player) {
          throw!(Ia::BadPieceStateForOperation);
        }
        state.users[user].player = player;
        if state.users[! user].player == player {
          // OK, you want to swap
          state.users[! user].player = default();
        }
        (Unpredictable, format!("became player {} at the", user))
      },
      "unclaim-x" | "unclaim-y" => {
        let user = get_user();
        state.users[user].player = default();
        (Unpredictable, format!("cleared player {} at the", user))
      },
      _ => {
        throw!(Ia::BadPieceStateForOperation);
      }
    };

    let moveable = {
      let gplayers = &gs.players;
      if state.users.iter().any(
        |ust| gplayers.get(ust.player).is_some()
      ) {
        PieceMoveable::IfWresting
      } else {
        PieceMoveable::Yes
      }
    };

    state.do_start_or_stop(piece, was_current, was_implied_running,
                           held, &self.spec, ig)?;

    let log = log_did_to_piece(ioccults,&gs.occults, gpl, gpc, ipc, &did)
      .unwrap_or_else(|e| {
        error!("failed to log: {:?}", &e);
        vec![LogEntry { html: "<failed to log>".to_html() }]
      });

    gpc.moveable = moveable;
    
    match howish {
      Unpredictable => {
        let r: PieceUpdateFromOpSimple = (
          WhatResponseToClientOp::Unpredictable,
          PieceUpdateOp::Modify(()),
          log);
        (r.into(), default())
      }
      UniversalImage => {
        let r: UpdateFromOpComplex = (
          PieceUpdate {
            wrc: WhatResponseToClientOp::Predictable,
            log,
            ops: PieceUpdateOps::PerPlayer(default()),
          },
          unprepared_update(piece),
        );
        r
      }
    }.into()
  }

  #[throws(IE)]
  fn held_change_hook(&self,
                      ig: &InstanceRef,
                      gpieces: &mut GPieces,
                      piece: PieceId,
                      was_held: Option<PlayerId>)
                      -> UnpreparedUpdates {
    let gpc = gpieces.get_mut(piece);
    let gpc = if let Some(gpc) = gpc { gpc } else { return default() };
    let now_held = gpc.held;
    let state: &mut State = gpc.xdata_mut_exp()?;
    let was_current = state.current;
    let was_running = state.implies_running(was_held);

    if_chain! {
      if was_held == None;
      if let Some(Current { user }) = state.current;
      if now_held == Some(state.users[user].player);
      then {
        state.current = Some(Current { user: ! user });
      }
    }

    state.do_start_or_stop(piece, was_current, was_running,
                           now_held, &self.spec, ig)?;
    unprepared_update(piece)
  }

  #[throws(IE)]
  fn loaded_hook(&self, piece: PieceId, gs: &mut GameState,
                 ig: &InstanceRef) {
    // The effect of this is to reload to the amount remaining at the
    // last game save.  That's probably tolerable, and even arguably
    // better than having the clock "have kept running" during the
    // lost state.
    let gpc = gs.pieces.byid_mut(piece).context("load hook")?;
    let held = gpc.held;
    let state = gpc.xdata_mut(|| State::new(&self.spec))?;
    state.do_start_or_stop(piece, None, None, held, &self.spec, ig)?;
  }
}
