
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
  clientid : u64,
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
    let mut g = iad.i.lock().map_err(|e| anyhow!("lock poison {:?}",&e))?;
    let user = g.users.get_mut(iad.user).ok_or_else(|| anyhow!("user deleted"))?;
    let client = Client { };
    let clientid : slotmap::KeyData = user.clients.insert(client).into();

    let mut uses = vec![];
    let mut defs = vec![];
    for (gpid, pr) in &g.gs.pieces {
      let id : slotmap::KeyData = gpid.into();
      let pri = PieceRenderInstructions {
        id : VisiblePieceId(id.as_ffi()),
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
                        pri.id_piece(),
                        pr.p.svg_select(&pri)));
      defs.push(pr.p.svg_x_defs(&pri));

      uses.push(format!(r##"<use href="#{}" data-p="{}" x="{}" y="{}"/>"##,
                        pri.id_piece(),
                        pri.id,
                        pr.pos[0], pr.pos[1]));
    }

    SessionRenderContext {
      clientid : clientid.as_ffi(),
      defs,
      uses,
    }
  };
  Ok(Template::render("test",&c))
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
    let mut g = iad.i.lock().map_err(|e| anyhow!("lock poison {:?}",&e))?;
    let user = g.users.get_mut(iad.user).ok_or_else(|| anyhow!("user deleted"))?;
    let _client = user.clients.get_mut(clientid).ok_or_else(|| anyhow!("client deleted"))?;
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
    ])
    .launch();
}
