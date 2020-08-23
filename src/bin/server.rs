
// xxx need button(s) to cause players to release grabs ?

#![feature(proc_macro_hygiene, decl_macro)]

use rocket::{get,routes};

use game::imports::*;

#[derive(Serialize,Debug)]
struct TestRenderContext { }

#[get("/")]
#[throws(OE)]
fn index() -> Template {
  let c = TestRenderContext { };
  Template::render("test",&c)
}

const RESOURCES : &[&str] = &["script.js", "style.css"];

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

#[throws(StartupError)]
fn main() {
  let config_filename = env::args().nth(1);
  ServerConfig::read(config_filename.as_ref().map(String::as_str))?;

  load_games()?;

  let cl = CommandListener::new()?;
  cl.spawn()?;

  let helmet = SpaceHelmet::default()
    .enable(NoSniff::Enable)
    .enable(Frame::Deny)
    .enable(Referrer::NoReferrer);

  let r = rocket::ignite()
    .attach(helmet)
    .attach(Template::fairing())
    .mount("/", routes![
      index,
      loading,
      resource,
      updates,
    ]);
  let r = game::session::mount(r);
  let r = game::api::mount(r);
  r.launch();
}
