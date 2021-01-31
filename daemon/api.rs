// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use super::*;

type WRC = WhatResponseToClientOp;

type PL = PresentationLayout;

#[derive(Clone,Debug)]
pub struct InstanceAccess<'i, Id> {
  pub raw_token: &'i RawTokenVal,
  pub i: InstanceAccessDetails<Id>,
}

impl<'r, Id> FromFormValue<'r> for InstanceAccess<'r, Id>
  where Id : AccessId, OE : From<Id::Error>
{
  type Error = OER;
  #[throws(OER)]
  fn from_form_value(param: &'r RawStr) -> Self {
    let token = RawTokenVal::from_str(param.as_str());
    let i = InstanceAccessDetails::from_token(token)?;
    InstanceAccess { raw_token : token, i }
  }
}

#[derive(Debug,Serialize,Deserialize)]
struct ApiPiece<O : ApiPieceOp> {
  ctoken : RawToken,
  piece : VisiblePieceId,
  gen : Generation,
  cseq : ClientSequence,
  op : O,
}

struct ApiPieceOpArgs<'a> {
  gs: &'a mut GameState,
  player: PlayerId,
  piece: PieceId,
  p: &'a dyn Piece,
  lens: &'a dyn Lens /* used for LogEntry and PieceId but not Pos */
}

trait ApiPieceOp : Debug {
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate;

  #[throws(OnlineError)]
  fn check_held(&self, pc: &PieceState, player: PlayerId) {
    if pc.held != None && pc.held != Some(player) {
      throw!(OnlineError::PieceHeld)
    }
  }
}

#[derive(Error,Debug)]
#[error("{0}")]
pub struct OnlineErrorResponse(#[from] OnlineError);

impl From<&OnlineErrorResponse> for rocket::http::Status {
  fn from(oe: &OnlineErrorResponse) -> rocket::http::Status {
    use OnlineError::*;
    match oe.0 {
      ServerFailure(_) => Status::InternalServerError,
      NoClient | NoPlayer(_) | GameBeingDestroyed
        => Status::NotFound,
      OnlineError::PieceHeld | OnlineError::PieceGone
        => Status::Conflict,
      InvalidZCoord | BadOperation | BadJSON(_)
        => Status::BadRequest,
    }
  }
}

impl<'r> Responder<'r> for OnlineErrorResponse {
  #[throws(Status)]
  fn respond_to(self, req: &Request) -> Response<'r> {
    let msg = format!("Online-layer error\n{:?}\n{}\n", self, self);
    let status = (&self).into();
    let mut resp = Responder::respond_to(msg,req).unwrap();
    resp.set_status(status);
    resp
  }
}

fn log_did_to_piece<L: Lens + ?Sized>(
  _occults: &GameOccults,
  gpl: &mut GPlayerState, lens: &L,
  piece: PieceId, pc: &PieceState, p: &dyn Piece,
  did: &str,
) -> Vec<LogEntry> {
  vec![ LogEntry { html: Html(format!(
    "{} {} {}",
    &htmlescape::encode_minimal(&gpl.nick),
    did,
    p.describe_pri(&lens.log_pri(piece, pc)).0
  ))}]
}

#[throws(OE)]
fn api_piece_op<O: ApiPieceOp>(form : Json<ApiPiece<O>>)
                   -> impl response::Responder<'static> {
//  thread::sleep(Duration::from_millis(2000));
  let iad = lookup_token(form.ctoken.borrow())?;
  let client = iad.ident;
  let mut ig = iad.gref.lock()?;
  ig.save_game_later();
  let g = &mut *ig;
  let cl = &mut g.clients.byid_mut(client)?;
  // ^ can only fail if we raced
  cl.lastseen = Instant::now();
  let player = cl.player;
  let gs = &mut g.gs;
  let ipieces = &g.ipieces;
  let iplayers = &g.iplayers;
  let _ = iplayers.byid(player)?;
  let _ = gs.players.byid(player)?;
  let lens = TransparentLens { };
  let piece = lens.decode_visible_pieceid(form.piece, player);
  use ApiPieceOpError::*;

  match (||{
    let p = ipieces.get(piece).ok_or(OnlineError::PieceGone)?;
    let pc = gs.pieces.byid_mut(piece)?;

    let q_gen = form.gen;
    let u_gen =
      if client == pc.lastclient { pc.gen_before_lastclient }
      else { pc.gen };

    debug!("client={:?} pc.lastclient={:?} pc.gen_before={:?} pc.gen={:?} q_gen={:?} u_gen={:?}", &client, &pc.lastclient, &pc.gen_before_lastclient, &pc.gen, &q_gen, &u_gen);

    if u_gen > q_gen { throw!(PieceOpError::Conflict) }
    form.op.check_held(pc,player)?;
    let update =
      form.op.op(ApiPieceOpArgs {
        gs, player, piece,
        p: p.as_ref(),
        lens: &lens,
      })?;
    Ok::<_,ApiPieceOpError>(update)
  })() {
    Err(ReportViaUpdate(poe)) => {
      PrepareUpdatesBuffer::piece_report_error(
        &mut ig, poe,
        piece, vec![], POEPP::Unprocessed, client, form.cseq, &lens
      )?;
      debug!("api_piece_op Err(RVU): {:?}", &form);
    },
    Err(PartiallyProcessed(poe, logents)) => {
      PrepareUpdatesBuffer::piece_report_error(
        &mut ig, poe,
        piece, logents, POEPP::Partially, client, form.cseq, &lens
      )?;
      debug!("api_piece_op Err(PP): {:?}", &form);
    },
    Err(ReportViaResponse(err)) => {
      warn!("api_piece_op ERROR {:?}: {:?}", &form, &err);
      Err(err)?;
    },
    Ok(PieceUpdate { wrc, log, ops }) => {
      let mut buf = PrepareUpdatesBuffer::new(g,
                                              Some((wrc, client, form.cseq)),
                                              Some(1 + log.len()));
      
      buf.piece_update(piece, ops, &lens);
      buf.log_updates(log);

      debug!("api_piece_op OK: {:?}", &form);
    }
  }
  ""
}

macro_rules! api_route_core {
  { $fn:ident, $path:expr, $form:ident, $formdef:item,
    $( $impl:tt )*
  } => {
    #[derive(Debug,Serialize,Deserialize)]
    $formdef

    #[post($path, format="json", data="<form>")]
    #[throws(OER)]
    fn $fn(form : Json<ApiPiece<$form>>)
           -> impl response::Responder<'static> {
      api_piece_op(form)?
    }

    impl ApiPieceOp for $form {
      $( $impl )*
    }
  }
}

macro_rules! api_route {
  { $fn:ident, $path:expr,
    $( #[ $attrs:meta ] )* struct $form:ident { $( $body:tt )* }
    $( $impl:tt )*
  } => {
    api_route_core!{
      $fn, $path, $form,
      $( #[ $attrs ] )* struct $form { $( $body )* },
      $( $impl )*
    }
  };
  { $fn:ident, $path:expr,
    $( #[ $attrs:meta ] )* struct $form:ident ( $( $body:tt )* );
    $( $impl:tt )*
  } => {
    api_route_core!{
      $fn, $path, $form,
      $( #[ $attrs ] )* struct $form ( $( $body )* );,
      $( $impl )*
    }
  }
}

api_route!{
  api_grab, "/_/api/grab",
  struct ApiPieceGrab {
  }
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,player,piece,p,lens, .. } = a;
    let gpl = gs.players.byid_mut(player)?;
    let pc = gs.pieces.byid_mut(piece)?;

    if pc.held.is_some() { throw!(OnlineError::PieceHeld) }
    pc.held = Some(player);
    
    let update = PieceUpdateOp::ModifyQuiet(());
    let logents = log_did_to_piece(&gs.occults, gpl, lens, piece, pc, p,
                                   "grasped");

    (WhatResponseToClientOp::Predictable,
     update, logents).into()
  }
}

api_route!{
  api_wrest, "/_/api/wrest",
  struct ApiPieceWrest {
  }
  #[throws(OnlineError)]
  fn check_held(&self, _pc: &PieceState, _player: PlayerId) { }

  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,player,piece,p,lens, .. } = a;
    let gpl = gs.players.byid(player)?;
    let pc = gs.pieces.byid_mut(piece)?;
    let pcs = p.describe_pri(&lens.log_pri(piece, pc)).0;

    let was = pc.held;
    pc.held = Some(player);
    let was = was.and_then(|p| gs.players.get(p));    
    let was = was.map(|was| htmlescape::encode_minimal(&was.nick));

    let update = PieceUpdateOp::Modify(());

    let pls = &htmlescape::encode_minimal(&gpl.nick);

    let logent = LogEntry { html : Html(match was {
        Some(was) => format!("{} wrested {} from {}", pls, pcs, was),
        None => format!("{} wrested {}", pls, pcs),
    })};

    (WhatResponseToClientOp::Predictable,
     update, vec![logent]).into()
  }
}

api_route!{
  api_ungrab, "/_/api/ungrab",
  struct ApiPieceUngrab {
  }
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,player,piece,p,lens, .. } = a;
    let gpl = gs.players.byid_mut(player).unwrap();
    let pc = gs.pieces.byid_mut(piece).unwrap();

    if pc.held != Some(player) { throw!(OnlineError::PieceHeld) }
    pc.held = None;

    let update = PieceUpdateOp::Modify(());
    let logents = log_did_to_piece(&gs.occults, gpl, lens, piece, pc, p,
                                   "released");

    (WhatResponseToClientOp::Predictable,
     update, logents).into()
  }
}

api_route!{
  api_raise, "/_/api/setz",
  struct ApiPieceSetZ {
    z : ZCoord,
  }
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,piece, .. } = a;
    let pc = gs.pieces.byid_mut(piece).unwrap();
    pc.zlevel = ZLevel { z : self.z.clone(), zg : gs.gen };
    let update = PieceUpdateOp::SetZLevel(());
    (WhatResponseToClientOp::Predictable,
     update, vec![]).into()
  }
}

api_route!{
  api_move, "/_/api/m",
  struct ApiPieceMove(Pos);

  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,piece, .. } = a;
    let pc = gs.pieces.byid_mut(piece).unwrap();
    let (pos, clamped) = self.0.clamped(gs.table_size);
    let logents = vec![];
    pc.pos = pos;
    if clamped {
      throw!(ApiPieceOpError::PartiallyProcessed(
        PieceOpError::PosOffTable,
        logents,
      ));
    }
    let update = PieceUpdateOp::Move(self.0);
    (WhatResponseToClientOp::Predictable,
     update, logents).into()
  }
}

api_route!{
  api_rotate, "/_/api/rotate",
  struct ApiPieceRotate(CompassAngle);

  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,player,piece,p,lens, .. } = a;
    let pc = gs.pieces.byid_mut(piece).unwrap();
    let gpl = gs.players.byid_mut(player).unwrap();
    pc.angle = PieceAngle::Compass(self.0);
    let logents = log_did_to_piece(&gs.occults, gpl, lens, piece, pc, p,
                                   "rotated");
    let update = PieceUpdateOp::Modify(());
    (WhatResponseToClientOp::Predictable,
     update, logents).into()
  }
}

api_route!{
  api_pin, "/_/api/pin",
  struct ApiPiecePin (bool);

  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,player,piece,p,lens, .. } = a;
    let pc = gs.pieces.byid_mut(piece).unwrap();
    let gpl = gs.players.byid_mut(player).unwrap();
    pc.pinned = self.0;
    let update = PieceUpdateOp::Modify(());
    let logents = log_did_to_piece(
      &gs.occults, gpl, lens, piece, pc, p,
      if pc.pinned { "pinned" } else { "unpinned" },
    );
    (WhatResponseToClientOp::Predictable,
     update, logents).into()
  }
}

const DEFKEY_FLIP : UoKey = 'f';

api_route!{
  api_uo, "/_/api/k",
  struct ApiPieceUo {
    opname: String,
    wrc: WhatResponseToClientOp,
  }
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,player,piece,p,lens, .. } = a;
    '_normal_global_ops__not_loop: loop {
      let pc = gs.pieces.byid_mut(piece)?;
      let gpl = gs.players.byid_mut(player)?;
      let _: Impossible = match (self.opname.as_str(), self.wrc) {

        ("flip", wrc@ WRC::UpdateSvg) => {
          let nfaces = p.nfaces();
          pc.face = ((RawFaceId::from(pc.face) + 1) % nfaces).into();
          return (
            wrc,
            PieceUpdateOp::Modify(()),
            log_did_to_piece(&gs.occults, gpl, lens, piece, pc, p,
                             "flipped"),
          ).into()
        },

        _ => break,
      };
    }

    '_abnormal_global_ops__notloop: loop {
      let _: Impossible = match self {

        _ => break,
      };
    }

    p.ui_operation(gs, player, piece, &self.opname, self.wrc, lens)?
  }
}

pub fn mount(rocket_instance: Rocket) -> Rocket {
  rocket_instance.mount("/", routes![
    api_grab,
    api_ungrab,
    api_raise,
    api_move,
    api_rotate,
    api_wrest,
    api_pin,
    api_uo,
  ])
}
