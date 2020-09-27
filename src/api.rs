// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

#[derive(Debug,Serialize,Deserialize)]
struct ApiPiece<O : ApiPieceOp> {
  ctoken : RawToken,
  piece : VisiblePieceId,
  gen : Generation,
  cseq : ClientSequence,
  op : O,
}
trait ApiPieceOp : Debug {
  #[throws(ApiPieceOpError)]
  fn op(&self, gs: &mut GameState, player: PlayerId, piece: PieceId,
        p: &dyn Piece,
        lens: &dyn Lens /* used for LogEntry and PieceId but not Pos */)
        -> (PieceUpdateOp<()>, Vec<LogEntry>);
}

#[derive(Error,Debug)]
enum ApiPieceOpError {
  ReportViaResponse(#[from] OnlineError),
  ReportViaUpdate(#[from] PieceOpError),
  PartiallyProcessed(PieceOpError, Vec<LogEntry>),
}
display_as_debug!(ApiPieceOpError);

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
      NoClient | NoPlayer | GameBeingDestroyed
        => Status::NotFound,
      OnlineError::PieceHeld | OnlineError::PieceGone
        => Status::Conflict,
      InvalidZCoord | BadJSON(_)
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
  let g_pieces = &g.pieces;
  let _ = gs.players.byid(player)?;
  let lens = TransparentLens { };
  let piece = lens.decode_visible_pieceid(form.piece, player);
  use ApiPieceOpError::*;

  match (||{
    let p = g_pieces.get(piece).ok_or(OnlineError::PieceGone)?;
    let pc = gs.pieces.byid_mut(piece)?;

    let q_gen = form.gen;
    let u_gen =
      if client == pc.lastclient { pc.gen_before_lastclient }
      else { pc.gen };

    if u_gen > q_gen { throw!(PieceOpError::Conflict) }
    if pc.held != None && pc.held != Some(player) {
      throw!(OnlineError::PieceHeld)
    };
    let (update, logents) = form.op.op(gs,player,piece,p.as_ref(),&lens)?;
    Ok::<_,ApiPieceOpError>((update, logents))
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
    Ok((update, logents)) => {
      let mut buf = PrepareUpdatesBuffer::new(g, Some((client, form.cseq)),
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
  fn op(&self, gs: &mut GameState, player: PlayerId, piece: PieceId,
        p: &dyn Piece, lens: &dyn Lens)
        -> (PieceUpdateOp<()>, Vec<LogEntry>) {
    let pl = gs.players.byid(player)?;
    let pc = gs.pieces.byid_mut(piece)?;

    if pc.held.is_some() { throw!(OnlineError::PieceHeld) }
    pc.held = Some(player);
    
    let update = PieceUpdateOp::Modify(());

    let logent = LogEntry {
      html : Html(format!("{} grasped {}",
                     &htmlescape::encode_minimal(&pl.nick),
                     p.describe_pri(&lens.log_pri(piece, pc)).0)),
    };

    (update, vec![logent])
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
  fn op(&self, gs: &mut GameState, player: PlayerId, piece: PieceId,
        p: &dyn Piece, lens: &dyn Lens)
        -> (PieceUpdateOp<()>, Vec<LogEntry>) {
    let pl = gs.players.byid(player).unwrap();
    let pc = gs.pieces.byid_mut(piece).unwrap();

    if pc.held != Some(player) { throw!(OnlineError::PieceHeld) }
    pc.held = None;
    
    let update = PieceUpdateOp::Modify(());

    let logent = LogEntry {
      html : Html(format!("{} released {}",
                     &htmlescape::encode_minimal(&pl.nick),
                     p.describe_pri(&lens.log_pri(piece, pc)).0)),
    };

    (update, vec![logent])
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
  fn op(&self, gs: &mut GameState, _: PlayerId, piece: PieceId,
        _p: &dyn Piece, _: &dyn Lens)
        -> (PieceUpdateOp<()>, Vec<LogEntry>) {
    let pc = gs.pieces.byid_mut(piece).unwrap();
    pc.zlevel = ZLevel { z : self.z, zg : gs.gen };
    let update = PieceUpdateOp::SetZLevel(pc.zlevel);
    (update, vec![])
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
  fn op(&self, gs: &mut GameState, _: PlayerId, piece: PieceId,
        _p: &dyn Piece, _lens: &dyn Lens)
        -> (PieceUpdateOp<()>, Vec<LogEntry>) {
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
    (update, logents)
  }
}

pub fn mount(rocket_instance: Rocket) -> Rocket {
  rocket_instance.mount("/", routes![
    api_grab,
    api_ungrab,
    api_raise,
    api_move,
  ])
}
