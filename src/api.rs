// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

type WRC = WhatResponseToClientOp;

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
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdateFromOp;

  #[throws(OnlineError)]
  fn check_held(&self, pc: &PieceState, player: PlayerId) {
    if pc.held != None && pc.held != Some(player) {
      throw!(OnlineError::PieceHeld)
    }
  }
}

#[derive(Error,Debug)]
pub enum ApiPieceOpError {
  ReportViaResponse(#[from] OnlineError),
  ReportViaUpdate(#[from] PieceOpError),
  PartiallyProcessed(PieceOpError, Vec<LogEntry>),
}
display_as_debug!(ApiPieceOpError);

impl From<PlayerNotFound> for ApiPieceOpError {
  fn from(x: PlayerNotFound) -> ApiPieceOpError {
    ApiPieceOpError::ReportViaResponse(x.into())
  }
}

pub trait Lens : Debug {
  fn pieceid2visible(&self, piece: PieceId) -> VisiblePieceId;
  fn log_pri(&self, piece: PieceId, pc: &PieceState)
             -> PieceRenderInstructions;
  fn svg_pri(&self, piece: PieceId, pc: &PieceState, player: PlayerId)
             -> PieceRenderInstructions;
  fn massage_prep_piecestate(&self, ns : &mut PreparedPieceState);
  fn decode_visible_pieceid(&self, vpiece: VisiblePieceId, player: PlayerId)
                            -> PieceId;
}
#[derive(Debug)]
pub struct TransparentLens {
  // when lenses become nontrivial, make this nonconstructable
  // to find all the places where a TransparentLens was bodged
}
impl Lens for TransparentLens {
  fn pieceid2visible(&self, piece: PieceId) -> VisiblePieceId {
    let kd : slotmap::KeyData = piece.into();
    VisiblePieceId(kd)
  }
  fn log_pri(&self, piece: PieceId, pc: &PieceState)
             -> PieceRenderInstructions {
    let id = self.pieceid2visible(piece);
    PieceRenderInstructions { id, face : pc.face }
  }
  fn svg_pri(&self, piece: PieceId, pc: &PieceState, _player: PlayerId)
             -> PieceRenderInstructions {
    self.log_pri(piece, pc)
  }
  fn decode_visible_pieceid(&self, vpiece: VisiblePieceId, _player: PlayerId)
                            -> PieceId {
    let kd : slotmap::KeyData = vpiece.into();
    PieceId::from(kd)
  }
  fn massage_prep_piecestate(&self, _ns : &mut PreparedPieceState) { }
}

impl<'r> Responder<'r> for OnlineError {
  #[throws(Status)]
  fn respond_to(self, req: &Request) -> Response<'r> {
    let msg = format!("Online-layer error\n{:?}\n{}\n", self, self);
    use rocket::http::Status;
    use OnlineError::*;
    let status = match self {
      ServerFailure(_) => Status::InternalServerError,
      NoClient | NoPlayer(_) | GameBeingDestroyed
        => Status::NotFound,
      OnlineError::PieceHeld | OnlineError::PieceGone
        => Status::Conflict,
      InvalidZCoord | BadOperation | BadJSON(_)
        => Status::BadRequest,
    };
    let mut resp = Responder::respond_to(msg,req).unwrap();
    resp.set_status(status);
    resp
  }
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

    if u_gen > q_gen { throw!(PieceOpError::Conflict) }
    form.op.check_held(pc,player)?;
    let (wrc, update, logents) =
      form.op.op(ApiPieceOpArgs {
        gs, player, piece,
        p: p.as_ref(),
        lens: &lens,
      })?;
    Ok::<_,ApiPieceOpError>((wrc, update, logents))
  })() {
    Err(ReportViaUpdate(poe)) => {
      PrepareUpdatesBuffer::piece_report_error(
        &mut ig, poe,
        piece, vec![], client, &lens
      )?;
    },
    Err(PartiallyProcessed(poe, logents)) => {
      PrepareUpdatesBuffer::piece_report_error(
        &mut ig, poe,
        piece, logents, client, &lens
      )?;
    },
    Err(ReportViaResponse(err)) => {
      warn!("api_piece_op ERROR {:?}: {:?}", &form, &err);
      Err(err)?;
    },
    Ok((wrc, update, logents)) => {
      let mut buf = PrepareUpdatesBuffer::new(g,
                                              Some((wrc, client, form.cseq)),
                                              Some(1 + logents.len()));
      
      buf.piece_update(piece, update, &lens);
      buf.log_updates(logents);

      debug!("api_piece_op OK: {:?}", &form);
    }
  }
  ""
}

#[derive(Debug,Serialize,Deserialize)]
struct ApiPieceGrab {
}
#[post("/_/api/grab", format="json", data="<form>")]
#[throws(OE)]
fn api_grab(form : Json<ApiPiece<ApiPieceGrab>>)
            -> impl response::Responder<'static> {
  api_piece_op(form)
}
impl ApiPieceOp for ApiPieceGrab {
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdateFromOp {
    let ApiPieceOpArgs { gs,player,piece,p,lens, .. } = a;
    let gpl = gs.players.byid(player)?;
    let pc = gs.pieces.byid_mut(piece)?;

    if pc.held.is_some() { throw!(OnlineError::PieceHeld) }
    pc.held = Some(player);
    
    let update = PieceUpdateOp::Modify(());

    let logent = LogEntry {
      html : Html(format!("{} grasped {}",
                     &htmlescape::encode_minimal(&gpl.nick),
                     p.describe_pri(&lens.log_pri(piece, pc)).0)),
    };

    (WhatResponseToClientOp::Predictable,
     update, vec![logent])
  }
}

#[derive(Debug,Serialize,Deserialize)]
struct ApiPieceWrest {
}
#[post("/_/api/wrest", format="json", data="<form>")]
#[throws(OE)]
fn api_wrest(form : Json<ApiPiece<ApiPieceWrest>>)
            -> impl response::Responder<'static> {
  // xxx pinning should send to back
  api_piece_op(form)
}
impl ApiPieceOp for ApiPieceWrest {
  #[throws(OnlineError)]
  fn check_held(&self, _pc: &PieceState, _player: PlayerId) { }

  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdateFromOp {
    let ApiPieceOpArgs { gs,player,piece,p,lens, .. } = a;
    let gpl = gs.players.byid(player)?;
    let pc = gs.pieces.byid_mut(piece)?;
    let pcs = p.describe_pri(&lens.log_pri(piece, pc)).0;

    let was = pc.held;
    pc.held = Some(player);
    let was = was.and_then(|p| gs.players.get(p));    

    let update = PieceUpdateOp::Modify(());

    let pls = &htmlescape::encode_minimal(&gpl.nick);

    let logent = LogEntry { html : Html(match was {
        Some(was) => format!("{} wrested {} from {}", pls, pcs,
                             &htmlescape::encode_minimal(&was.nick)),
        None => format!("{} wrested {}", pls, pcs),
    })};

    (WhatResponseToClientOp::Predictable,
     update, vec![logent])
  }
}

#[derive(Debug,Serialize,Deserialize)]
struct ApiPieceUngrab {
}
#[post("/_/api/ungrab", format="json", data="<form>")]
#[throws(OE)]
fn api_ungrab(form : Json<ApiPiece<ApiPieceUngrab>>)
              -> impl response::Responder<'static> {
  api_piece_op(form)
}
impl ApiPieceOp for ApiPieceUngrab {
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdateFromOp {
    let ApiPieceOpArgs { gs,player,piece,p,lens, .. } = a;
    let gpl = gs.players.byid(player).unwrap();
    let pc = gs.pieces.byid_mut(piece).unwrap();

    if pc.held != Some(player) { throw!(OnlineError::PieceHeld) }
    pc.held = None;
    
    let update = PieceUpdateOp::Modify(());

    let logent = LogEntry {
      html : Html(format!("{} released {}",
                     &htmlescape::encode_minimal(&gpl.nick),
                     p.describe_pri(&lens.log_pri(piece, pc)).0)),
    };

    (WhatResponseToClientOp::Predictable,
     update, vec![logent])
  }
}

#[derive(Debug,Serialize,Deserialize)]
struct ApiPieceRaise {
  z : ZCoord,
}
#[post("/_/api/setz", format="json", data="<form>")]
#[throws(OE)]
fn api_raise(form : Json<ApiPiece<ApiPieceRaise>>)
            -> impl response::Responder<'static> {
  api_piece_op(form)
}
impl ApiPieceOp for ApiPieceRaise {
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdateFromOp {
    let ApiPieceOpArgs { gs,piece, .. } = a;
    let pc = gs.pieces.byid_mut(piece).unwrap();
    pc.zlevel = ZLevel { z : self.z.clone(), zg : gs.gen };
    let update = PieceUpdateOp::SetZLevel(());
    (WhatResponseToClientOp::Predictable,
     update, vec![])
  }
}

#[derive(Debug,Serialize,Deserialize)]
struct ApiPieceMove (Pos);
#[post("/_/api/m", format="json", data="<form>")]
#[throws(OE)]
fn api_move(form : Json<ApiPiece<ApiPieceMove>>) -> impl response::Responder<'static> {
  api_piece_op(form)
}
impl ApiPieceOp for ApiPieceMove {
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdateFromOp {
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
     update, logents)
  }
}

#[derive(Debug,Serialize,Deserialize)]
struct ApiPiecePin (bool);
#[post("/_/api/pin", format="json", data="<form>")]
#[throws(OE)]
fn api_pin(form : Json<ApiPiece<ApiPiecePin>>) -> impl response::Responder<'static> {
  api_piece_op(form)
}
impl ApiPieceOp for ApiPiecePin {
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdateFromOp {
    let ApiPieceOpArgs { gs,player,piece,p,lens, .. } = a;
    let pc = gs.pieces.byid_mut(piece).unwrap();
    let gpl = gs.players.byid(player).unwrap();
    pc.pinned = self.0;
    let update = PieceUpdateOp::Modify(());
    let logents = vec![ LogEntry { html: Html(format!(
      "{} {} {}",
      &htmlescape::encode_minimal(&gpl.nick),
      if pc.pinned { "pinned" } else { "unpinned" },
      p.describe_pri(&lens.log_pri(piece, pc)).0
    ))}];
    (WhatResponseToClientOp::Predictable,
     update, logents)
  }
}

const DEFKEY_FLIP : UoKey = 'f';

#[derive(Debug,Serialize,Deserialize)]
struct ApiPieceUo { opname: String, wrc: WhatResponseToClientOp }
#[post("/_/api/k", format="json", data="<form>")]
#[throws(OE)]
fn api_uo(form : Json<ApiPiece<ApiPieceUo>>) -> impl response::Responder<'static> {
  api_piece_op(form)
}
impl ApiPieceOp for ApiPieceUo {
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdateFromOp {
    let ApiPieceOpArgs { gs,player,piece,p,lens, .. } = a;
    '_normal_global_ops__not_loop: loop {
      let pc = gs.pieces.byid_mut(piece)?;
      let gpl = gs.players.byid(player)?;
      let _: Impossible = match (self.opname.as_str(), self.wrc) {

        ("flip", wrc@ WRC::UpdateSvg) => {
          let nfaces = p.nfaces();
          pc.face = ((RawFaceId::from(pc.face) + 1) % nfaces).into();
          return (
            wrc,
            PieceUpdateOp::Modify(()),
            vec![ LogEntry { html: Html(format!(
              "{} flipped {}",
              &htmlescape::encode_minimal(&gpl.nick),
              p.describe_pri(&lens.log_pri(piece, pc)).0
            )) }])
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
    api_wrest,
    api_pin,
    api_uo,
  ])
}
