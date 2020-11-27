// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![feature(proc_macro_hygiene, decl_macro)]

use rocket::{get,routes};
use rocket_contrib::serve::StaticFiles;
use rocket::response::Content;
use rocket::fairing;

use otter::imports::*;

#[derive(Serialize,Debug)]
struct FrontPageRenderContext { }

#[derive(Copy,Clone,Debug)]
enum ResourceLocation { Main, Wasm(&'static str), }
type RL = ResourceLocation;

const RESOURCES : &[(&'static str, ResourceLocation, ContentType)] = &[
  ("script.js",    RL::Main,                       ContentType::JavaScript),
  ("LICENCE",      RL::Main,                       ContentType::Plain),
  ("libre",        RL::Main,                       ContentType::HTML),
  ("AGPLv3",       RL::Main,                       ContentType::Plain),
  ("CC-BY-SA-3.0", RL::Main,                       ContentType::Plain),
  ("CC-BY-SA-4.0", RL::Main,                       ContentType::Plain),
  ("wasm.wasm",    RL::Wasm("otter_wasm_bg.wasm"), ContentType::WASM),
  ("wasm.js",      RL::Wasm("otter_wasm.js"),      ContentType::JavaScript),
];

#[derive(Debug)]
struct CheckedResourceLeaf {
  safe_leaf: &'static str,
  locn: ResourceLocation,
  ctype: ContentType,
}

#[derive(Error,Debug)]
#[error("not a valid resource path")]
struct UnknownResource{}

impl<'r> FromParam<'r> for CheckedResourceLeaf {
  type Error = UnknownResource;
  fn from_param(param: &'r RawStr) -> Result<Self, Self::Error> {
    for &(safe_leaf, locn, ref ctype) in RESOURCES {
      if safe_leaf == param.as_str() {
        return Ok(CheckedResourceLeaf {
          safe_leaf, locn,
          ctype: ctype.clone(),
        })
      }
    }
    Err(UnknownResource{})
  }
}

#[derive(Serialize,Debug)]
struct LoadingRenderContext<'r> {
  ptoken: &'r RawTokenVal,
  layout: PresentationLayout,
}
#[get("/")]
#[throws(OE)]
fn loading_p(ptoken: WholeQueryString) -> Template {
  loading(PresentationLayout::Portrait, ptoken)?
}
// xxx also do p, make it an account/player property
#[get("/<layout>")]
#[throws(OE)]
fn loading_l(layout: AbbrevPresentationLayout, ptoken: WholeQueryString)
             -> Template {
  loading(layout.0, ptoken)?
}

#[throws(OE)]
fn loading(layout: PresentationLayout, ptoken: WholeQueryString) -> Template {
  if let Some(ptoken) = ptoken.0 {
    let c = LoadingRenderContext {
      ptoken: RawTokenVal::from_str(ptoken),
      layout,
    };
    Template::render("loading",&c)
  } else {
    let c = FrontPageRenderContext { };
    Template::render("front",&c)
  }
}

struct WholeQueryString<'r>(pub Option<&'r str>);

impl<'a,'r> FromRequest<'a,'r> for WholeQueryString<'a> {
  type Error = Impossible;
  fn from_request(r: &'a rocket::Request<'r>) -> rocket::Outcome<Self, (rocket::http::Status, <Self as rocket::request::FromRequest<'a, 'r>>::Error), ()> {
    eprintln!("REQUEST uri={:?}", &r.uri());
    let q = r.uri().query();
    rocket::Outcome::Success(WholeQueryString(q))
  }
}

#[get("/_/updates?<ctoken>&<gen>")]
#[throws(OE)]
fn updates<'r>(ctoken : InstanceAccess<ClientId>, gen: u64,
           cors: rocket_cors::Guard<'r>)
           -> impl response::Responder<'r> {
  let gen = Generation(gen);
  let iad = ctoken.i;
  debug!("starting update stream {:?}", &iad);
  let client = iad.ident;
  let content = sse::content(iad, gen)?;
  let content = DebugReader(content, client);
  let content = response::Stream::chunked(content, 4096);
  const CTYPE : &str = "text/event-stream; charset=utf-8";
  let ctype = ContentType::parse_flexible(CTYPE).unwrap();
  cors.responder(response::content::Content(ctype,content))
}  

#[get("/_/<leaf>")]
#[throws(io::Error)]
fn resource<'r>(leaf : CheckedResourceLeaf) -> impl Responder<'r> {
  let path = match leaf.locn {
    RL::Main => format!("{}/{}", config().template_dir, leaf.safe_leaf),
    RL::Wasm(s) => format!("{}/{}", config().wasm_dir, s),
  };
  Content(leaf.ctype, NamedFile::open(path)?)
}  

#[derive(Debug,Copy,Clone)]
struct ContentTypeFixup;
impl fairing::Fairing for ContentTypeFixup {
  fn info(&self) -> fairing::Info {
    fairing::Info {
      name: "ContentTypeFixup",
      kind: fairing::Kind::Response,
    }
  }
  fn on_response(&self, _: &Request<'_>, response: &mut Response<'_>) {
    match response.content_type() {
      None => {
        response.set_header(ContentType::Plain);
      },
      Some(ct) if ct == ContentType::GZIP => {
        // the only thing we serve with a .gz extension are the
        // compressed source code tarballs
        use rocket::http::hyper::header;
        response.set_header(header::TransferEncoding(vec![
          header::Encoding::Gzip,
        ]));
        response.set_header(ContentType::TAR);
      },
      _ => { /* hopefully OK */ },
    }
  }
}

#[throws(StartupError)]
fn main() {
  // todo test suite for cli at least
  // todo test suite for web api

  let config_filename = env::args().nth(1);
  ServerConfig::read(config_filename.as_ref().map(String::as_str))?;

  std::env::set_var("ROCKET_CLI_COLORS","off");

  let c = config();

  flexi_logger::Logger::with(c.log.clone()).start()?;

  {
    let check = format!("{}/otter/index.html", &c.bundled_sources);
    fs::metadata(&check)
      .context(check.clone())
      .context("check bundled-sources directory")?;
  }

  shapelib::load()?;

  load_accounts()?;
  load_games(&mut AccountsGuard::lock(), &mut games_lock())?;

  let cl = CommandListener::new()?;
  cl.spawn()?;

  let helmet = SpaceHelmet::default()
    .enable(NoSniff::Enable)
    .enable(Frame::Deny)
    .enable(Referrer::NoReferrer);

  let mut cbuilder = rocket::config::Config::build(
    if c.debug {
      rocket::config::Environment::Development
    } else {
      info!("requesting Production");
      rocket::config::Environment::Production
    }
  );

  if c.debug {
    cbuilder = cbuilder.address("127.0.0.1");
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
    .attach(ContentTypeFixup)
    .attach(helmet)
    .attach(Template::fairing())
    .manage(cors_state)
    .mount("/", routes![
      loading_l,
      loading_p,
      resource,
      updates,
    ])
    .mount("/_/src", StaticFiles::from(&c.bundled_sources))
    ;

  let r = otter::session::mount(r);
  let r = otter::api::mount(r);

  thread::spawn(client_periodic_expiry);
  thread::spawn(logs_periodic_expiry);

  r.launch();
}
