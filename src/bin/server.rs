
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
  ptoken : &'r RawTokenVal,
}
#[get("/<ptoken>")]
#[throws(OE)]
fn loading(ptoken : InstanceAccess<PlayerId>) -> Template {
  let c = LoadingRenderContext { ptoken : ptoken.raw_token };
  Template::render("loading",&c)
}

#[get("/_/updates/<ctoken>/<gen>")]
#[throws(OE)]
fn updates<'r>(ctoken : InstanceAccess<ClientId>, gen: u64,
           cors: rocket_cors::Guard<'r>)
           -> impl response::Responder<'r> {
  let gen = Generation(gen);
  let iad = ctoken.i;
  let content = sse::content(iad, gen)?;
  let content = response::Stream::chunked(content, 4096);
  const CTYPE : &str = "text/event-stream; charset=utf-8";
  let ctype = ContentType::parse_flexible(CTYPE).unwrap();
  cors.responder(response::content::Content(ctype,content))
}  

#[get("/_/<leaf>")]
fn resource(leaf : CheckedResourceLeaf) -> io::Result<NamedFile> {
  NamedFile::open(format!("{}/{}", config().template_dir, leaf.safe))
}  

#[throws(StartupError)]
fn main() {
  let config_filename = env::args().nth(1);
  ServerConfig::read(config_filename.as_ref().map(String::as_str))?;

  std::env::set_var("ROCKET_CLI_COLORS","off");

  flexi_logger::Logger::with(config().log.clone()).start()?;

  load_games()?;

  let cl = CommandListener::new()?;
  cl.spawn()?;

  let helmet = SpaceHelmet::default()
    .enable(NoSniff::Enable)
    .enable(Frame::Deny)
    .enable(Referrer::NoReferrer);

  let c = config();

  let mut cbuilder = rocket::config::Config::build(
    if c.debug {
      rocket::config::Environment::Development
    } else {
      eprintln!("requesting Production");
      rocket::config::Environment::Production
    }
  );

  if c.debug {
    cbuilder = cbuilder.address("127.0.0.1")
  }
  cbuilder = cbuilder.workers(c.rocket_workers);
  if let Some(port) = c.http_port {
    cbuilder = cbuilder.port(port);
  }
  cbuilder.extras.insert("template_dir".to_owned(),
                         c.template_dir.clone().into());

  thread::spawn(game_flush_task);

  let cors_state = {
    use rocket_cors::*;
    let opts = CorsOptions::default()
      .allowed_origins(AllowedOrigins::all())
      .allowed_methods(iter::once(rocket::http::Method::Get.into()).collect());
    opts.validate().expect("cors options");
    opts.to_cors().expect("cors")
  };

  let rconfig = cbuilder.finalize()?;

  let r = rocket::custom(rconfig)
    .attach(helmet)
    .attach(Template::fairing())
    .manage(cors_state)
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
