
// xxx need button(s) to kill old clients
// ? need expiry of old clients?  limit of client count?

#![feature(proc_macro_hygiene, decl_macro)]

use rocket::{get,post,routes};
use rocket_contrib::json::Json;
//use rocket::{post};

use game::imports::*;

type RE = E;

#[derive(Serialize,Debug)]
struct TestRenderContext { }

#[get("/")]
fn index() -> Result<Template,RE> {
  let c = TestRenderContext { };
  Ok(Template::render("test",&c))
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
fn loading(ptoken : InstanceAccess<PlayerId>) -> Result<Template,RE> {
  let c = LoadingRenderContext { ptoken : ptoken.raw_token };
  Ok(Template::render("loading",&c))
}

#[derive(Serialize,Debug)]
struct SessionRenderContext {
  ctoken : String,
  player : PlayerId,
  defs : Vec<String>,
  uses : Vec<String>,
}

#[derive(Deserialize)]
struct SessionForm {
  ptoken : String,
}
#[post("/_/session", format="json", data="<form>")]
fn session(form : Json<SessionForm>) -> Result<Template,RE> {
  // make session in this game, log a message to other players
  let iad = lookup_token(&form.ptoken)
    .ok_or_else(|| anyhow!("unknown token"))?;
  let player = iad.ident;
  let c = {
    let mut ig = iad.g.lock().map_err(|e| anyhow!("lock poison {:?}",&e))?;
    let _pl = ig.gs.players.get_mut(player)
      .ok_or_else(|| anyhow!("player deleted"))?;
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
        r##"<use href="#{}" data-p="{}" data-g="" x="{}" y="{}"/>"##,
                        pri.id_piece(),
                        pri.id,
                        pr.pos[0], pr.pos[1]));
    }

    SessionRenderContext {
      ctoken : ctoken.0,
      player,
      defs,
      uses,
    }
  };
  Ok(Template::render("test",&c))
}

#[derive(Error,Debug)]
#[error("operation error {:?}",self)]
enum OpError {
  Conflict,
  PieceGone,
  PieceHeld,
}

#[derive(Debug,Serialize,Deserialize)]
struct ApiGrab {
  t : String,
  p : VisiblePieceId,
  g : Generation,
  s : ClientSequence,
}
#[post("/_/api/grab", format="json", data="<form>")]
#[throws(RE)]
fn api_grab(form : Json<ApiGrab>) -> impl response::Responder<'static> {
  let iad = lookup_token(&form.t).ok_or_else(||anyhow!("unknown token"))?;
  let client = iad.ident;
  let mut g = iad.g.lock().map_err(|e| anyhow!("lock poison {:?}",&e))?;
  let g = &mut *g;
  let cl = &g.clients.get(client).ok_or_else(||anyhow!("unknown client"))?;
  // ^ can only fail if we raced
  let player = cl.player;
  let r : Result<(),OpError> = (||{
    let piece = decode_visible_pieceid(form.p);
    let gs = &mut g.gs;
    let p = gs.pieces.get_mut(piece).ok_or(OpError::PieceGone)?;
    let q_gen = form.g;
    let u_gen =
      if client == p.lastclient { p.gen_lastclient }
      else { p.gen_before_lastclient };
    if u_gen > q_gen { Err(OpError::Conflict)? }
    if p.held != None { Err(OpError::PieceHeld)? };
    p.held = Some(player);
    gs.gen.increment();
    let gen = gs.gen;
    if client != p.lastclient {
      p.gen_before_lastclient = p.gen_lastclient;
      p.lastclient = client;
    }
    let update = Update {
      gen,
      u : UpdatePayload::PieceUpdate(piece, p.mk_update()),
    };
    let update = Arc::new(update);
    // split vie wthing would go here
    p.gen_lastclient = gen;
    for (_tplayer, tplupdates) in &mut g.updates {
      tplupdates.log.push_back((client, update.clone()));
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
#[throws(RE)]
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
#[throws(RE)]
fn api_move(form : Json<ApiMove>) -> impl response::Responder<'static> {
  eprintln!("API {:?}", &form);
  ""
}

#[derive(Serialize)]
enum XUpdate {
  TestCounter { value: usize },
}

#[get("/_/updates/<ctoken>/<gen>")]
#[throws(E)]
fn updates(ctoken : InstanceAccess<ClientId>, gen: Generation)
           -> impl response::Responder<'static> {
  let iad = ctoken.i;
  let content = sse::content(iad);
  let content = response::Stream::chunked(content, 1);
  const CTYPE : &str = "text/event-stream; charset=utf-8";
  let ctype = ContentType::parse_flexible(CTYPE).unwrap();
  response::content::Content(ctype,content)
}  

#[Get("/_/<leaf>")]
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
      sse::updates,
      api_grab,
      api_ungrab,
      api_move,
    ])
    .launch();
}
