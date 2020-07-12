
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

trait Lens {
  fn log_pri(&self, piece: PieceId, pc: &PieceState)
             -> PieceRenderInstructions;
  fn svg_pri(&self, piece: PieceId, pc: &PieceState, player: PlayerId)
             -> PieceRenderInstructions;
  fn massage_prep_piecestate(&self, ns : &mut PreparedPieceState);
  fn decode_visible_pieceid(&self, vpiece: VisiblePieceId, player: PlayerId)
                            -> PieceId;
}
struct TransparentLens { }
impl Lens for TransparentLens {
  fn log_pri(&self, piece: PieceId, pc: &PieceState)
             -> PieceRenderInstructions {
    let kd : slotmap::KeyData = piece.into();
    let id = VisiblePieceId(kd);
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
  let mut g = iad.g.lock()?;
  let g = &mut *g;
  let cl = &g.clients.byid(client)?;
  // ^ can only fail if we raced
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

    if u_gen > q_gen { Err(GameError::Conflict)? }
    if pc.held != None && pc.held != Some(player) {
      Err(GameError::PieceHeld)?
    };
    let (update, logents) = form.op.op(gs,player,piece,&lens)?;
    Ok((update, logents))
  })() {
    Err(err) => {
      let err : GameError = err;
      if let GameError::InternalErrorSVG(svg) = err { Err(svg)? }
      eprintln!("API {:?} => {:?}", &form, &err);
    },
    Ok((update, logents)) => {
      let pc = gs.pieces.byid_mut(piece).expect("piece deleted by op!");

      gs.gen.increment();
      let gen = gs.gen;
      if client != pc.lastclient {
        pc.gen_before_lastclient = pc.gen;
        pc.lastclient = client;
      }
      pc.gen = gen;
      eprintln!("PC GEN_LC={:?} LC={:?}", pc.gen, pc.lastclient);

      let pri_for_all = lens.svg_pri(piece,pc,Default::default());

      let update = update.try_map_new_state(|_|{
        let mut ns = pc.prep_piecestate(&pri_for_all)?;
        lens.massage_prep_piecestate(&mut ns);
        <Result<_,SVGProcessingError>>::Ok(ns)
      })?;

      let mut us = Vec::with_capacity(1 + logents.len());

      us.push(PreparedUpdateEntry::Piece {
        client,
        sameclient_cseq : form.cseq,
        piece : pri_for_all.id,
        op : update,
      });

      for logentry in logents {
        let logentry = Arc::new(logentry);
        gs.log.push((gen, logentry.clone()));
        us.push(PreparedUpdateEntry::Log(logentry));
      }

      let update = PreparedUpdate { gen, us, };
      let update = Arc::new(update);
      eprintln!("UPDATE {:?}", &update);

      for (_tplayer, tplupdates) in &mut g.updates {
        tplupdates.log.push_back(update.clone());
        tplupdates.cv.notify_all();
      }
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

    if pc.held.is_some() { Err(GameError::PieceHeld)? }
    pc.held = Some(player);
    
    let update = PieceUpdateOp::Modify(());

    let logent = LogEntry {
      html : format!("{} grasped {}",
                     &htmlescape::encode_minimal(&pl.nick),
                     pc.describe_html(&lens.log_pri(piece, pc))?),
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

    if pc.held != Some(player) { Err(GameError::PieceHeld)? }
    pc.held = None;
    
    let update = PieceUpdateOp::Modify(());

    let logent = LogEntry {
      html : format!("{} released {}",
                     &htmlescape::encode_minimal(&pl.nick),
                     pc.describe_html(&lens.log_pri(piece, pc))?),
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

    pc.pos = self.0;
    let update = PieceUpdateOp::Move(self.0);
    (update, vec![])
  }
}

pub fn mount(rocket_instance: Rocket) -> Rocket {
  return rocket_instance.mount("/", routes![
    api_grab,
    api_ungrab,
    api_raise,
    api_move,
  ]);
}
