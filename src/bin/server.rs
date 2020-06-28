
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
  token : &'r str,
}

#[get("/<token>")]
fn loading(token : InstanceAccess) -> Result<Template,RE> {
  let c = LoadingRenderContext { token : token.raw_token };
  Ok(Template::render("loading",&c))
}

#[derive(Serialize,Debug)]
struct SessionRenderContext {
  clientid : ClientId,
  player : PlayerId,
  defs : Vec<String>,
  uses : Vec<String>,
}

#[derive(Deserialize)]
struct SessionForm {
  token : String,
}
#[post("/_/session", format="json", data="<form>")]
fn session(form : Json<SessionForm>) -> Result<Template,RE> {
  // make session in this game, log a message to other players
  let iad = lookup_token(&form.token).ok_or_else(|| anyhow!("unknown token"))?;
  let c = {
    let mut ig = iad.i.lock().map_err(|e| anyhow!("lock poison {:?}",&e))?;
    let _player = ig.gs.players.get_mut(iad.player)
      .ok_or_else(|| anyhow!("player deleted"))?;
    let client = Client { };
    let clientid = ig.clients[iad.player].insert(client);

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
      clientid,
      player : iad.player,
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
  c : ClientId,
  p : VisiblePieceId,
  g : Counter,
  s : ClientSequence,
}
#[post("/_/api/grab", format="json", data="<form>")]
#[throws(RE)]
fn api_grab(form : Json<ApiGrab>) -> impl response::Responder<'static> {
  let iad = lookup_token(&form.t).ok_or_else(||anyhow!("unknown token"))?;
  let mut g = iad.i.lock().map_err(|e| anyhow!("lock poison {:?}",&e))?;
  let client = form.c;
  let r : Result<(),OpError> = (||{
    let p = decode_visible_pieceid(form.p);
    let gs = &mut g.gs;
    let p = gs.pieces.get_mut(p).ok_or(OpError::PieceGone)?;
    let q_gen = form.g;
    let u_gen =
      if client == p.lastclient { p.gen_lastclient }
      else { p.gen_before_lastclient };
    if p.gen > u_gen { Err(OpError::Conflict)? }
    if p.held != None { Err(OpError::PieceHeld)? };
    p.held = Some(iad.player);
    gs.gen += 1;
    let gen = gs.gen;
    if client != p.lastclient {
      p.gen_before_lastclient = p.gen_lastclient;
      p.lastclient = client;
    }
    p.gen_lastclient = gen;
    for (tplayer, tpl) in g.gs.players {
      for (tclient, cl) in ig.clients.get(tplayer) {
        if tclient == cl {
          cl.transmit_update(client, Update {
            gen,
            u : GameUpdate::ClientSequence(form.s)
          });
        } else {
          cl.transmit_update(client, Update {
            gen,
            u : GameUpdate::PieceUpdate(p, p.update()),
          });
        }          
      }
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
enum Update {
  TestCounter { value: usize },
}

type TestCounter = BufReader<TestCounterInner>;
#[derive(Debug)]
struct TestCounterInner { next : usize, }
impl Read for TestCounterInner {
  fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
    thread::sleep(Duration::from_millis(500));
    let message = Update::TestCounter { value : self.next };
    let data = serde_json::to_string(&message)?;
    let data = format!("data: {}\n\n", &data);
    // eprintln!("want to return into &[;{}] {:?}", buf.len(), &data);
    self.next += 1;
    buf[0..data.len()].copy_from_slice(data.as_bytes());
    Ok(buf.len())
  }
}

/*
#[derive(Deserialize)]
struct APIForm {
  t : String,
  c : ClientId,
}
 */

#[get("/_/updates/<token>/<clientid>")]
#[throws(RE)]
fn updates(token : &RawStr, clientid : u64) -> impl response::Responder<'static> {
  let iad = lookup_token(token.as_str()).ok_or_else(|| anyhow!("unknown token"))?;
  let clientid = slotmap::KeyData::from_ffi(clientid);
  let clientid = clientid.into();
  let _ = {
    let mut ig = iad.i.lock().map_err(|e| anyhow!("lock poison {:?}",&e))?;
    let g = &mut ig.gs;
    let _player = g.players.get_mut(iad.player)
      .ok_or_else(|| anyhow!("user deleted"))?;
    let _client = ig.clients[iad.player].get_mut(clientid)
      .ok_or_else(|| anyhow!("client deleted"))?;
  };
  let tc = TestCounterInner { next : 0 };
  let tc = BufReader::new(tc);
  let ch = response::Stream::chunked(tc, 1);
  let ct = ContentType::parse_flexible("text/event-stream; charset=utf-8").
    unwrap();
  response::content::Content(ct,ch)
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
