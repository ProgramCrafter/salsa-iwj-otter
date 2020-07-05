
// xxx need button(s) to kill old clients
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
  defs : Vec<String>,
  uses : Vec<String>,
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
    let mut defs = vec![];
    for (gpid, pr) in &ig.gs.pieces {
      let pri = PieceRenderInstructions {
        id : make_pieceid_visible(gpid),
        face : pr.face,
      };
      defs.push(format!(r##"<g id="{}">{}</g>"##,
                        pri.id_piece(),
                        pr.p.svg_piece(&pri)));
      defs.push(format!(r##"
                          <g id="{}"
                             stroke="black"
               	             stroke-dasharray="3 1  1 1  1 1"
	                     fill="none">
                           {}
                          </g>
                        "##,
                        pri.id_select(),
                        pr.p.svg_select(&pri)));
      defs.push(pr.p.svg_x_defs(&pri));

      uses.push(format!(
        r##"<use href="#{}" data-piece="{}" data-gplayer="" x="{}" y="{}"/>"##,
                        pri.id_piece(),
                        pri.id,
                        pr.pos[0], pr.pos[1]));
    }

    SessionRenderContext {
      ctoken : ctoken.0,
      gen : ig.gs.gen,
      player,
      defs,
      uses,
      nick : pl.nick.clone(),
    }
  };
  Ok(Template::render("test",&c))
}

#[derive(Debug,Serialize,Deserialize)]
struct ApiGrab {
  ctoken : String,
  piece : VisiblePieceId,
  gen : Generation,
  cseq : ClientSequence,
}
#[post("/_/api/grab", format="json", data="<form>")]
#[throws(OE)]
fn api_grab(form : Json<ApiGrab>) -> impl response::Responder<'static> {
  let iad = lookup_token(&form.ctoken)?;
  let client = iad.ident;
  let mut g = iad.g.lock()?;
  let g = &mut *g;
  let cl = &g.clients.byid(client)?;
  // ^ can only fail if we raced
  let player = cl.player;
  let r : Result<(),GameError> = (||{
    let piece = decode_visible_pieceid(form.piece);
    let gs = &mut g.gs;
    let p = gs.pieces.byid_mut(piece)?;
    let q_gen = form.gen;
    let u_gen =
      if client == p.lastclient { p.gen_lastclient }
      else { p.gen_before_lastclient };
    if u_gen > q_gen { Err(GameError::Conflict)? }
    if p.held != None { Err(GameError::PieceHeld)? };
    p.held = Some(player);
    gs.gen.increment();
    let gen = gs.gen;
    if client != p.lastclient {
      p.gen_before_lastclient = p.gen_lastclient;
      p.lastclient = client;
    }
    let json = UpdatePayload::PieceUpdate(piece, p.mk_update());
    let json = serde_json::to_string(&json).expect("convert to json");
    let update = PreparedUpdate {
      gen,
      client,
      piece,
      cseq : form.cseq,
      json,
    };
    let update = Arc::new(update);
    // split vie wthing would go here
    p.gen_lastclient = gen;
    for (_tplayer, tplupdates) in &mut g.updates {
      tplupdates.log.push_back(update.clone());
      tplupdates.cv.notify_all();
    }
    Ok(())
  })();
  eprintln!("API {:?} => {:?}", &form, &r);
  ""
}

#[derive(Debug,Serialize,Deserialize)]
struct ApiUngrab {
  t : String,
  p : VisiblePieceId,
}
#[post("/_/api/ungrab", format="json", data="<form>")]
#[throws(OE)]
fn api_ungrab(form : Json<ApiUngrab>) -> impl response::Responder<'static> {
  eprintln!("API {:?}", &form);
  ""
}

#[derive(Debug,Serialize,Deserialize)]
struct ApiMove {
  t : String,
  p : VisiblePieceId,
  l : Pos,
}
#[post("/_/api/m", format="json", data="<form>")]
#[throws(OE)]
fn api_move(form : Json<ApiMove>) -> impl response::Responder<'static> {
  eprintln!("API {:?}", &form);
  ""
}

#[get("/_/updates/<ctoken>/<gen>")]
#[throws(OE)]
fn updates(ctoken : InstanceAccess<ClientId>, gen: u64)
           -> impl response::Responder<'static> {
  let gen = Generation(gen);
  let iad = ctoken.i;
  let content = sse::content(iad, gen)?;
  let content = response::Stream::chunked(content, 4096 /* xxx */);
  const CTYPE : &str = "text/event-stream; charset=utf-8";
  let ctype = ContentType::parse_flexible(CTYPE).unwrap();
  // xxx set CORS allowed header
  response::content::Content(ctype,content)
}  

#[get("/_/<leaf>")]
fn resource(leaf : CheckedResourceLeaf) -> io::Result<NamedFile> {
  let template_dir = "templates"; // xxx
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
      api_move,
    ])
    .launch();
}
