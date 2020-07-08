
// xxx need button(s) to kill old clients
// and/or cause players to release grabs
// ? need expiry of old clients?  limit of client count?

#![feature(proc_macro_hygiene, decl_macro)]

use rocket::{get,post,routes};
use rocket_contrib::json::Json;
//use rocket::{post};

use game::imports::*;

#[derive(Serialize,Debug)]
struct TestRenderContext { }

#[get("/")]
#[throws(OE)]
fn index() -> Template {
  let c = TestRenderContext { };
  Template::render("test",&c)
}

const RESOURCES : &[&'static str] = &["script.js", "style.css"];

#[derive(Debug)]
struct CheckedResourceLeaf { pub safe : &'static str }
#[derive(Error,Debug)]
#[error("not a valid resource path")]
struct UnknownResource{}

impl<'r> FromParam<'r> for CheckedResourceLeaf {
  type Error = UnknownResource;
  fn from_param(param: &'r RawStr) -> Result<Self, Self::Error> {
    for &safe in RESOURCES {
      if safe == param.as_str() { return Ok(CheckedResourceLeaf{ safe }) }
    }
    Err(UnknownResource{})
  }
}

#[derive(Serialize,Debug)]
struct LoadingRenderContext<'r> {
  ptoken : &'r str,
}
#[get("/<ptoken>")]
#[throws(OE)]
fn loading(ptoken : InstanceAccess<PlayerId>) -> Template {
  let c = LoadingRenderContext { ptoken : ptoken.raw_token };
  Template::render("loading",&c)
}

#[derive(Serialize,Debug)]
struct SessionRenderContext {
  ctoken : String,
  player : PlayerId,
  gen : Generation,
  uses : Vec<String>,
  defs : Vec<(VisiblePieceId,String)>,
  nick : String,
}

#[derive(Deserialize)]
struct SessionForm {
  ptoken : String,
}
#[post("/_/session", format="json", data="<form>")]
fn session(form : Json<SessionForm>) -> Result<Template,OE> {
  // make session in this game, log a message to other players
  let iad = lookup_token(&form.ptoken)?;
  let player = iad.ident;
  let c = {
    let mut ig = iad.g.lock()?;
    let ig = &mut *ig;
    let pl = ig.gs.players.byid_mut(player)?;
    let cl = Client { player };
    let client = ig.clients.insert(cl);

    let ciad = InstanceAccessDetails {
      g : iad.g.clone(),
      ident : client,
    };
    let ctoken = record_token(ciad);

    let mut uses = vec![];
    let mut alldefs = vec![];

    let mut pieces : Vec<_> = ig.gs.pieces.iter().collect();

    pieces.sort_by_key(|(_,pr)| pr.raised);

    for (gpid, pr) in pieces {
      let pri = PieceRenderInstructions {
        id : make_pieceid_visible(gpid),
        face : pr.face,
      };
      let defs = pr.make_defs(&pri);
      alldefs.push((pri.id, defs));

      uses.push(format!(
        r##"<use id="{}" href="#{}" data-piece="{}" data-gplayer="" x="{}" y="{}"/>"##,
                        pri.id_use(),
                        pri.id_piece(),
                        pri.id,
                        pr.pos[0], pr.pos[1]));
    }

    let src = SessionRenderContext {
      ctoken : ctoken.0,
      gen : ig.gs.gen,
      player,
      defs : alldefs,
      uses,
      nick : pl.nick.clone(),
    };
    eprintln!("SRC {:?}", &src);
    src
  };
  Ok(Template::render("session",&c))
}

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
  fn log_pri(&self, piece: PieceId, pc: &PieceRecord)
             -> PieceRenderInstructions;
  fn svg_pri(&self, piece: PieceId, pc: &PieceRecord, player: PlayerId)
             -> PieceRenderInstructions;
  fn massage_prep_piecestate(&self, ns : &mut PreparedPieceState);
  fn decode_visible_pieceid(&self, vpiece: VisiblePieceId, player: PlayerId)
                            -> PieceId;
}
struct TransparentLens { }
impl Lens for TransparentLens {
  fn log_pri(&self, piece: PieceId, pc: &PieceRecord)
             -> PieceRenderInstructions {
    let kd : slotmap::KeyData = piece.into();
    let id = VisiblePieceId(kd);
    PieceRenderInstructions { id, face : pc.face }
  }
  fn svg_pri(&self, piece: PieceId, pc: &PieceRecord, _player: PlayerId)
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

      let update = update.map_new_state(|_|{
        let mut ns = pc.prep_piecestate(&pri_for_all);
        lens.massage_prep_piecestate(&mut ns);
        ns
      });

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

    if pc.held != Some(player) { Err(GameError::PieceHeld)? }
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
}
#[post("/_/api/raise", format="json", data="<form>")]
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
    pc.raised = gs.gen;
    let update = PieceUpdateOp::Raise();
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

#[get("/_/updates/<ctoken>/<gen>")]
#[throws(OE)]
fn updates(ctoken : InstanceAccess<ClientId>, gen: u64)
           -> impl response::Responder<'static> {
  let gen = Generation(gen);
  let iad = ctoken.i;
  let content = sse::content(iad, gen)?;
  let content = response::Stream::chunked(content, 4096);
  const CTYPE : &str = "text/event-stream; charset=utf-8";
  let ctype = ContentType::parse_flexible(CTYPE).unwrap();
  // xxx set CORS allowed header
  response::content::Content(ctype,content)
}  

#[get("/_/<leaf>")]
fn resource(leaf : CheckedResourceLeaf) -> io::Result<NamedFile> {
  let template_dir = "templates";
  NamedFile::open(format!("{}/{}", template_dir, leaf.safe))
}  

fn main() {
  xxx_global_setup();

  let helmet = SpaceHelmet::default()
    .enable(NoSniff::Enable)
    .enable(Frame::Deny)
    .enable(Referrer::NoReferrer);

  rocket::ignite()
    .attach(helmet)
    .attach(Template::fairing())
    .mount("/", routes![
      index,
      loading,
      session,
      resource,
      updates,
      api_grab,
      api_ungrab,
      api_raise,
      api_move,
    ])
    .launch();
}
