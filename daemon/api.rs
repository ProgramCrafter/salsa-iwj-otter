// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

pub use super::*;

type PL = PresentationLayout;

#[derive(Clone,Debug)]
pub struct InstanceAccess<'i, Id> {
  pub raw_token: &'i RawTokenVal,
  pub i: InstanceAccessDetails<Id>,
}

impl<'r, Id> FromFormValue<'r> for InstanceAccess<'r, Id>
  where Id: AccessId, OE: From<Id::Error>
{
  type Error = OER;
  #[throws(OER)]
  fn from_form_value(param: &'r RawStr) -> Self {
    let token = RawTokenVal::from_str(param.as_str());
    let i = InstanceAccessDetails::from_token(token)?;
    InstanceAccess { raw_token: token, i }
  }
}

#[derive(Debug,Serialize,Deserialize)]
struct ApiPiece<O:ApiPieceOp> {
  ctoken: RawToken,
  piece: VisiblePieceId,
  gen: Generation,
  cseq: ClientSequence,
  op: O,
}

trait ApiPieceOp: Debug {
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate;

  #[throws(ApiPieceOpError)]
  fn op_complex(&self, a: ApiPieceOpArgs)
                -> (PieceUpdate, Vec<(PieceId, PieceUpdateOps)>) {
    (self.op(a)?, vec![])
  }

  #[throws(OnlineError)]
  fn check_held(&self, pc: &GPiece, player: PlayerId) {
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
      OnlineError::PieceHeld | OnlineError::PieceGone |
      OnlineError::OverlappingOccultation
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

#[throws(OE)]
fn api_piece_op<O: ApiPieceOp>(form: Json<ApiPiece<O>>)
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
  let gpl = gs.players.byid(player)?;
  let piece = vpiece_decode(gs, player, gpl, form.piece)
    .ok_or(OE::PieceGone)?;
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
      form.op.op_complex(ApiPieceOpArgs {
        gs, player, piece, ipieces,
        p: p.as_ref(),
      })?;
    Ok::<_,ApiPieceOpError>(update)
  })() {
    Err(ReportViaUpdate(poe)) => {
      PrepareUpdatesBuffer::piece_report_error(
        &mut ig, poe,
        piece, vec![], POEPP::Unprocessed, client, form.cseq,
      )?;
      debug!("api_piece_op Err(RVU): {:?}", &form);
    },
    Err(PartiallyProcessed(poe, logents)) => {
      PrepareUpdatesBuffer::piece_report_error(
        &mut ig, poe,
        piece, logents, POEPP::Partially, client, form.cseq,
      )?;
      debug!("api_piece_op Err(PP): {:?}", &form);
    },
    Err(ReportViaResponse(err)) => {
      warn!("api_piece_op ERROR {:?}: {:?}", &form, &err);
      Err(err)?;
    },
    Ok((PieceUpdate { wrc, log, ops }, updates)) => {
      let mut buf = PrepareUpdatesBuffer::new(g,
                                              Some((wrc, client, form.cseq)),
                                              Some(1 + log.len()));
      
      buf.piece_update(piece, ops);
      buf.piece_updates(updates);
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
    fn $fn(form: Json<ApiPiece<$form>>)
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
    let ApiPieceOpArgs { gs,player,piece,p, .. } = a;
    let gpl = gs.players.byid_mut(player)?;
    let pc = gs.pieces.byid_mut(piece)?;

    if pc.held.is_some() { throw!(OnlineError::PieceHeld) }
    pc.held = Some(player);
    
    let update = PieceUpdateOp::ModifyQuiet(());
    let logents = log_did_to_piece(
      &gs.occults, player, gpl, piece, pc, p,
      "grasped"
    );

    (WhatResponseToClientOp::Predictable,
     update, logents).into()
  }
}

api_route!{
  api_wrest, "/_/api/wrest",
  struct ApiPieceWrest {
  }
  #[throws(OnlineError)]
  fn check_held(&self, _pc: &GPiece, _player: PlayerId) { }

  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,player,piece,p, .. } = a;
    let pc = gs.pieces.byid_mut(piece)?;
    let players = &mut gs.players;
    let was = pc.held;
    let was = was.and_then(|p| players.get(p));
    let was = was.map(|was| htmlescape::encode_minimal(&was.nick));

    let gpl = players.byid_mut(player)?;
    let pri = piece_pri(&gs.occults, player, gpl, piece, pc);
    let pcs = p.describe_pri(pc, &pri).0;

    pc.held = Some(player);

    let update = PieceUpdateOp::Modify(());

    let pls = &htmlescape::encode_minimal(&gpl.nick);

    let logent = LogEntry { html: Html(match was {
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
    let ApiPieceOpArgs { gs,player,piece,p,ipieces, .. } = a;
    let gpl = gs.players.byid_mut(player).unwrap();
    let pc = gs.pieces.byid_mut(piece).unwrap();

    if pc.held != Some(player) { throw!(OnlineError::PieceHeld) }
    pc.held = None;

    let update = PieceUpdateOp::Modify(());
    let (logents, who_by) = log_did_to_piece_whoby(
      &gs.occults, player, gpl, piece, pc, p,
      "released"
    );

    let vanilla = (WhatResponseToClientOp::Predictable,
                   update,
                   logents);

    let update=
      recalculate_occultation_piece(
        gs,
        who_by,
        ipieces,
        piece,
        vanilla,
      ).map_err(|e| OnlineError::from(e))?;

    update
  }
}

api_route!{
  api_raise, "/_/api/setz",
  struct ApiPieceSetZ {
    z: ZCoord,
  }
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    // xxx prevent restzcking anything that is occulting
    let ApiPieceOpArgs { gs,piece, .. } = a;
    let pc = gs.pieces.byid_mut(piece).unwrap();
    pc.zlevel = ZLevel { z: self.z.clone(), zg: gs.gen };
    let update = PieceUpdateOp::SetZLevel(());
    (WhatResponseToClientOp::Predictable,
     update, vec![]).into()
  }
}

api_route!{
  api_move, "/_/api/m",
  struct ApiPieceMove(Pos);

  #[throws(OnlineError)]
  fn check_held(&self, pc: &GPiece, player: PlayerId) {
    // This will ensure that occultations are (in general) properly
    // updated, because the player will (have to) release the thing
    // again
    if pc.held != Some(player) {
      throw!(OnlineError::PieceHeld)
    }
    // xxx prevent moving anything that is occulting
  }

  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,piece, .. } = a;
    let pc = gs.pieces.byid_mut(piece).unwrap();
    let logents = vec![];
    match self.0.clamped(gs.table_size) {
      Ok(pos) => pc.pos = pos,
      Err(pos) => {
        pc.pos = pos;
        throw!(ApiPieceOpError::PartiallyProcessed(
          PieceOpError::PosOffTable,
          logents,
        ));
      }
    };
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
    let ApiPieceOpArgs { gs,player,piece,p, .. } = a;
    let pc = gs.pieces.byid_mut(piece).unwrap();
    let gpl = gs.players.byid_mut(player).unwrap();
    pc.angle = PieceAngle::Compass(self.0);
    let logents = log_did_to_piece(
      &gs.occults, player, gpl, piece, pc, p,
      "rotated"
    );
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
    let ApiPieceOpArgs { gs,player,piece,p, .. } = a;
    let pc = gs.pieces.byid_mut(piece).unwrap();
    let gpl = gs.players.byid_mut(player).unwrap();
    pc.pinned = self.0;
    let update = PieceUpdateOp::Modify(());
    let logents = log_did_to_piece(
      &gs.occults, player, gpl, piece, pc, p,
      if pc.pinned { "pinned" } else { "unpinned" },
    );
    (WhatResponseToClientOp::Predictable,
     update, logents).into()
  }
}

const DEFKEY_FLIP: UoKey = 'f';

api_route!{
  api_uo, "/_/api/k",
  struct ApiPieceUo {
    opname: String,
    wrc: WhatResponseToClientOp,
  }
  #[throws(ApiPieceOpError)]
  fn op(&self, mut a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { player,piece,p, .. } = a;
    let gs = &mut a.gs;
    '_normal_global_ops__not_loop: loop {
      let pc = gs.pieces.byid_mut(piece)?;
      let gpl = gs.players.byid_mut(player)?;
      let _: Void = match (self.opname.as_str(), self.wrc) {

        ("flip", wrc@ WRC::UpdateSvg) => {
          let nfaces = p.nfaces();
          pc.face = ((RawFaceId::from(pc.face) + 1) % nfaces).into();
          return (
            wrc,
            PieceUpdateOp::Modify(()),
            log_did_to_piece(
              &gs.occults, player, gpl, piece, pc, p,
              "flipped"
            ),
          ).into()
        },

        _ => break,
      };
    }

    '_abnormal_global_ops__notloop: loop {
      let _: Void = match self {

        _ => break,
      };
    }

    p.ui_operation(a, &self.opname, self.wrc)?
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
