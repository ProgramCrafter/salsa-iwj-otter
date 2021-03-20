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
struct ApiPiece<O:op::Complex> {
  ctoken: RawToken,
  piece: VisiblePieceId,
  gen: Generation,
  cseq: ClientSequence,
  op: O,
}

mod op {
  use super::*;

  pub trait Core: Debug { 
    #[throws(OnlineError)]
    fn check_held(&self, pc: &GPiece, player: PlayerId) {
      if pc.held != None && pc.held != Some(player) {
        throw!(OnlineError::PieceHeld)
      }
    }
  }

  pub trait Simple: Debug { 
    #[throws(ApiPieceOpError)]
    fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate;
  }
 
  pub trait Complex: Core + Debug { 
    #[throws(ApiPieceOpError)]
    fn op_complex(&self, a: ApiPieceOpArgs) -> UpdateFromOpComplex;
  }

  impl<T> Complex for T where T: Core + Simple {
    #[throws(ApiPieceOpError)]
    fn op_complex(&self, a: ApiPieceOpArgs) -> UpdateFromOpComplex {
      (self.op(a)?, vec![], None)
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
      OE::PieceHeld | OE::PieceGone |
      OE::OverlappingOccultation | OE::Occultation |
      OE::BadPieceStateForOperation
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
fn api_piece_op<O: op::Complex>(form: Json<ApiPiece<O>>)
                   -> impl response::Responder<'static> {
//  thread::sleep(Duration::from_millis(2000));
  let iad = lookup_token(form.ctoken.borrow())?;
  let client = iad.ident;
  let mut ig = iad.gref.lock()?;
  ig.save_game_later();

  ToPermute::with(|mut to_permute| {
    let r = (||{

  let g = &mut *ig;
  let cl = &mut g.clients.byid_mut(client)?;
  // ^ can only fail if we raced
  cl.lastseen = Instant::now();
  let player = cl.player;
  let gs = &mut g.gs;
  let ioccults = &g.ioccults;
  let ipieces = &g.ipieces;
  let iplayers = &g.iplayers;
  let _ = iplayers.byid(player)?;
  let gpl = gs.players.byid(player)?;
  let piece = vpiece_decode(gs, player, gpl, form.piece)
    .ok_or(OE::PieceGone)?;
  let was_held = gs.pieces.get(piece).as_ref().map(|gpc| gpc.held);
  use ApiPieceOpError::*;

  match (||{
    let ipc = ipieces.get(piece).ok_or(OnlineError::PieceGone)?;
    let gpc = gs.pieces.byid_mut(piece)?;

    let q_gen = form.gen;
    let u_gen =
      if client == gpc.lastclient { gpc.gen_before_lastclient }
      else { gpc.gen };

    debug!("client={:?} pc.lastclient={:?} pc.gen_before={:?} pc.gen={:?} q_gen={:?} u_gen={:?}", &client, &gpc.lastclient, &gpc.gen_before_lastclient, &gpc.gen, &q_gen, &u_gen);

    if u_gen > q_gen { throw!(PieceOpError::Conflict) }
    form.op.check_held(gpc,player)?;
    let update =
      form.op.op_complex(ApiPieceOpArgs {
        ioccults, gs, player, piece, ipieces, ipc,
        ig: &iad.gref,
        to_permute: &mut to_permute,
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
    Ok((PieceUpdate { wrc, log, ops }, updates, unprepared)) => {
      let mut buf = PrepareUpdatesBuffer::new(g,
                                              Some((wrc, client, form.cseq)),
                                              Some(1 + log.len()));

      buf.piece_update(piece, ops);
      buf.piece_updates(updates);
      buf.log_updates(log);
      if let Some(unprepared) = unprepared { unprepared(&mut buf); }

      debug!("api_piece_op OK: {:?}", &form);
    }
  };

  if let Some(unprepared) = if_chain! {
    let g = &mut *ig;
    if let Some(was_held) = was_held;
    if let Some(gpc) = g.gs.pieces.get_mut(piece);
    if gpc.held != was_held;
    if let Some(ipc) = &g.ipieces.get(piece);
    if let Ok(unprepared) = ipc.direct_trait_access().held_change_hook(
      &iad.gref,
      &mut g.gs.pieces,
      piece,
      was_held,
    ).map_err(|e| error!("internal error on change hook: {:?}", e));
    then { unprepared }
    else { None }
  } {
    let mut prepub = PrepareUpdatesBuffer::new(&mut ig, None, None);
    unprepared(&mut prepub);
    prepub.finish();
  }

      Ok::<(),OE>(())
    })();

    let g = &mut *ig;
    let gs = &mut g.gs;

    (r, to_permute.implement(&mut gs.players,
                             &mut gs.pieces,
                             &mut gs.occults,
                             &g.ipieces))
  })?;
  ""
}

macro_rules! api_route_core {
  { $fn:ident, $path:expr, $form:ident, $formdef:item,
    $( impl $trait:path as { $($impl:tt)* } )*
    $( as: $($simple_impl:tt)* )?
  } => {
    #[derive(Debug,Serialize,Deserialize)]
    $formdef

    #[post($path, format="json", data="<form>")]
    #[throws(OER)]
    fn $fn(form: Json<ApiPiece<$form>>)
           -> impl response::Responder<'static> {
      api_piece_op(form)?
    }

    $(
      impl $trait for $form { $($impl)* }
    )*
    $(
      impl op::Core for $form { }
      impl op::Simple for $form { $($simple_impl)* }
    )?
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

  as:
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,ioccults,player,piece,ipc, .. } = a;
    let gpl = gs.players.byid_mut(player)?;
    let gpc = gs.pieces.byid_mut(piece)?;

    let logents = log_did_to_piece(
      ioccults,gpl,gpc,ipc,
      "grasped"
    )?;

    if gpc.held.is_some() { throw!(OnlineError::PieceHeld) }
    gpc.held = Some(player);
    
    let update = PieceUpdateOp::ModifyQuiet(());

    (WhatResponseToClientOp::Predictable,
     update, logents).into()
  }
}

api_route!{
  api_wrest, "/_/api/wrest",
  struct ApiPieceWrest {
  }

  impl op::Core as {
    #[throws(OnlineError)]
    fn check_held(&self, _pc: &GPiece, _player: PlayerId) { }
  }

  impl op::Simple as {
    #[throws(ApiPieceOpError)]
    fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
      let ApiPieceOpArgs { gs,ioccults,player,piece,ipc, .. } = a;
      let gpc = gs.pieces.byid_mut(piece)?;
      let players = &mut gs.players;
      let was = gpc.held;
      let was = was.and_then(|p| players.get(p));
      let was = was.map(|was| htmlescape::encode_minimal(&was.nick));

      let gpl = players.byid_mut(player)?;
      let pri = piece_pri(ioccults, &gs.occults, player, gpl, piece, gpc, ipc)
        .ok_or(OE::PieceGone)?;

      let pcs = pri.describe(ioccults, gpc, ipc).0;

      gpc.held = Some(player);

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
}

api_route!{
  api_ungrab, "/_/api/ungrab",
  struct ApiPieceUngrab {
  }

  as:
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs {
      gs,ioccults,player,piece,ipc,ipieces,to_permute, ..
    } = a;
    let gpl = gs.players.byid_mut(player).unwrap();
    let gpc = gs.pieces.byid_mut(piece).unwrap();

    let (logents, who_by) = log_did_to_piece_whoby(
      ioccults,gpl,gpc,ipc,
      "released"
    )?;
    let who_by = who_by.ok_or(OE::PieceGone)?;

    if gpc.held != Some(player) { throw!(OnlineError::PieceHeld) }
    gpc.held = None;

    let update = PieceUpdateOp::Modify(());
    let vanilla = (WhatResponseToClientOp::Predictable,
                   update,
                   logents);
      
    if let Some(occid) = gpc.occult.passive_occid() {
      // if piece is occulted, definitely repermute its occultation
      // so that we don't leak which piece is which over repeated
      // adjustment clicks
      to_permute.mark_dirty(occid);
    };

    let update=
      recalculate_occultation_piece(
        gs,
        who_by,
        ipieces,
        ioccults,
        to_permute,
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

  as:
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,piece, .. } = a;
    let gpc = gs.pieces.byid_mut(piece).unwrap();
    if gpc.occult.is_active() { throw!(OE::Occultation) }
    gpc.zlevel = ZLevel { z: self.z.clone(), zg: gs.gen };
    let update = PieceUpdateOp::SetZLevel(());
    (WhatResponseToClientOp::Predictable,
     update, vec![]).into()
  }
}

api_route!{
  api_move, "/_/api/m",
  struct ApiPieceMove(Pos);

  impl op::Core as {
    #[throws(OnlineError)]
    fn check_held(&self, gpc: &GPiece, player: PlayerId) {
      // This will ensure that occultations are (in general) properly
      // updated, because the player will (have to) release the thing
      // again
      if gpc.held != Some(player) { throw!(OnlineError::PieceHeld) }
      if gpc.occult.is_active() { throw!(OE::Occultation) }
    }
  }

  impl op::Simple as {
    #[throws(ApiPieceOpError)]
    fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
      let ApiPieceOpArgs { gs,piece, .. } = a;
      let gpc = gs.pieces.byid_mut(piece).unwrap();
      let logents = vec![];
      match self.0.clamped(gs.table_size) {
        Ok(pos) => gpc.pos = pos,
        Err(pos) => {
          gpc.pos = pos;
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
}

api_route!{
  api_rotate, "/_/api/rotate",
  struct ApiPieceRotate(CompassAngle);

  as:
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,ioccults,player,piece,ipc, .. } = a;
    let gpc = gs.pieces.byid_mut(piece).unwrap();
    let gpl = gs.players.byid_mut(player).unwrap();
    let logents = log_did_to_piece(
      ioccults,gpl,gpc,ipc,
      "rotated"
    )?;
    gpc.angle = PieceAngle::Compass(self.0);
    let update = PieceUpdateOp::Modify(());
    (WhatResponseToClientOp::Predictable,
     update, logents).into()
  }
}

api_route!{
  api_pin, "/_/api/pin",
  struct ApiPiecePin (bool);

  as:
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,ioccults,player,piece,ipc, .. } = a;
    let gpc = gs.pieces.byid_mut(piece).unwrap();
    let gpl = gs.players.byid_mut(player).unwrap();
    let logents = log_did_to_piece(
      ioccults,gpl,gpc,ipc,
      if gpc.pinned { "pinned" } else { "unpinned" },
    )?;
    gpc.forbid_involved_in_occultation()?;
    gpc.pinned = self.0;
    let update = PieceUpdateOp::Modify(());
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

  impl op::Core as { }
  impl op::Complex as {
    #[throws(ApiPieceOpError)]
    fn op_complex(&self, mut a: ApiPieceOpArgs) -> UpdateFromOpComplex {
      let ApiPieceOpArgs { ioccults,player,piece,ipc, .. } = a;
      let gs = &mut a.gs;
      let pri = piece_pri(ioccults, &gs.occults,
                          player, gs.players.byid_mut(player)?,
                          piece, gs.pieces.byid(piece)?,
                          ipc)
        .ok_or(OE::PieceGone)?;
      let y = {
        use PriOG::*;
        match pri.occulted {
          Visible(y) => y,
          Occulted | Displaced(..) => throw!(OE::BadOperation),
        }
      };

      '_normal_global_ops__not_loop: loop {
        let gpc = gs.pieces.byid_mut(piece)?;
        let gpl = gs.players.byid_mut(player)?;
        let _: Void = match (self.opname.as_str(), self.wrc) {

          ("flip", wrc@ WRC::UpdateSvg) => {
            let nfaces = ipc.show(y).nfaces();
            let logents = log_did_to_piece(
              ioccults,gpl,gpc,ipc,
              "flipped"
            )?;
            gpc.face = ((RawFaceId::from(gpc.face) + 1) % nfaces).into();
            return ((
              wrc,
              PieceUpdateOp::Modify(()),
              logents,
            ).into(), vec![], None)
          },

          _ => break,
        };
      }

      '_abnormal_global_ops__notloop: loop {
        let _: Void = match self {

          _ => break,
        };
      }

      ipc.show(y).ui_operation(a, &self.opname, self.wrc)?
    }
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
