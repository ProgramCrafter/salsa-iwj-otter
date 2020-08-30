
use crate::imports::*;
use crate::http::*;

#[derive(Debug,Serialize,Deserialize)]
struct ApiPiece<O : ApiPieceOp> {
  ctoken : String,
  piece : VisiblePieceId,
  gen : Generation,
  cseq : ClientSequence,
  op : O,
}
trait ApiPieceOp : Debug {
  #[throws(GameError)]
  fn op(&self, gs: &mut GameState, player: PlayerId, piece: PieceId,
        lens: &dyn Lens /* used for LogEntry and PieceId but not Pos */)
        -> (PieceUpdateOp<()>, Vec<LogEntry>);
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

#[throws(OE)]
fn api_piece_op<O: ApiPieceOp>(form : Json<ApiPiece<O>>)
                   -> impl response::Responder<'static> {
//  thread::sleep(Duration::from_millis(2000));
  let iad = lookup_token(&form.ctoken)?;
  let client = iad.ident;
  let mut ig = iad.gref.lock()?;
  ig.save_game_later();
  let g = &mut *ig;
  let cl = &mut g.clients.byid_mut(client)?;
  // ^ can only fail if we raced
  cl.lastseen = Instant::now();
  let player = cl.player;
  let gs = &mut g.gs;
  let _ = gs.players.byid(player)?;
  let lens = TransparentLens { };
  let piece = lens.decode_visible_pieceid(form.piece, player);

  match (||{
    let pc = gs.pieces.byid_mut(piece)?;

    let q_gen = form.gen;
    let u_gen =
      if client == pc.lastclient { pc.gen_before_lastclient }
      else { pc.gen };

    eprintln!("Q_GEN={:?} U_GEN={:?}", u_gen, q_gen);

    if u_gen > q_gen { throw!(GameError::Conflict) }
    if pc.held != None && pc.held != Some(player) {
      throw!(GameError::PieceHeld)
    };
    let (update, logents) = form.op.op(gs,player,piece,&lens)?;
    Ok((update, logents))
  })() {
    Err(err) => {
      let err : GameError = err;
      if let GameError::InternalErrorSVG(svg) = err { throw!(svg) }
      eprintln!("API {:?} => {:?}", &form, &err);
      // Restating the state of this piece (with a new generation)
      // will forcibly synchronise the client which made the failing
      // request.
      let mut buf = PrepareUpdatesBuffer::new(g, None, None);
      buf.piece_update(piece, PieceUpdateOp::Modify(()), &lens);
      throw!(err);
    },
    Ok((update, logents)) => {
      let mut buf = PrepareUpdatesBuffer::new(g, Some((client, form.cseq)),
                                              Some(1 + logents.len()));
      
      buf.piece_update(piece, update, &lens);
      buf.log_updates(logents);

      eprintln!("API {:?} OK", &form);
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
  #[throws(GameError)]
  fn op(&self, gs: &mut GameState, player: PlayerId, piece: PieceId,
        lens: &dyn Lens)
        -> (PieceUpdateOp<()>, Vec<LogEntry>) {
    let pl = gs.players.byid(player).unwrap();
    let pc = gs.pieces.byid_mut(piece).unwrap();

    if pc.held.is_some() { throw!(GameError::PieceHeld) }
    pc.held = Some(player);
    
    let update = PieceUpdateOp::Modify(());

    let logent = LogEntry {
      html : format!("{} grasped {}",
                     &htmlescape::encode_minimal(&pl.nick),
                     pc.describe_html(&lens.log_pri(piece, pc))),
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
  #[throws(GameError)]
  fn op(&self, gs: &mut GameState, player: PlayerId, piece: PieceId,
        lens: &dyn Lens)
        -> (PieceUpdateOp<()>, Vec<LogEntry>) {
    let pl = gs.players.byid(player).unwrap();
    let pc = gs.pieces.byid_mut(piece).unwrap();

    if pc.held != Some(player) { throw!(GameError::PieceHeld) }
    pc.held = None;
    
    let update = PieceUpdateOp::Modify(());

    let logent = LogEntry {
      html : format!("{} released {}",
                     &htmlescape::encode_minimal(&pl.nick),
                     pc.describe_html(&lens.log_pri(piece, pc))),
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
  #[throws(GameError)]
  fn op(&self, gs: &mut GameState, _: PlayerId, piece: PieceId,
        _: &dyn Lens)
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
  #[throws(GameError)]
  fn op(&self, gs: &mut GameState, _: PlayerId, piece: PieceId,
        _lens: &dyn Lens)
        -> (PieceUpdateOp<()>, Vec<LogEntry>) {
    let pc = gs.pieces.byid_mut(piece).unwrap();
    if let (_,true) = self.0.clamped(gs.table_size) {
      throw!(GameError::PosOffTable);
    }
    pc.pos = self.0;
    let update = PieceUpdateOp::Move(self.0);
    (update, vec![])
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
