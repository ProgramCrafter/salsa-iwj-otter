// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![feature(proc_macro_hygiene, decl_macro)]

use otter::imports::thiserror;

pub mod imports;

pub mod api;
pub mod cmdlistener;
pub mod session;

pub use rocket::http::Status;
pub use rocket::http::{ContentType, RawStr};
pub use rocket::request::Request;
pub use rocket::request::{FromFormValue, FromParam, FromRequest, LenientForm};
pub use rocket::response;
pub use rocket::response::NamedFile;
pub use rocket::response::{Responder, Response};
pub use rocket::{get, post, routes};
pub use rocket::{Rocket, State};
pub use rocket_contrib::helmet::*;
pub use rocket_contrib::json::Json;
pub use rocket_contrib::templates::tera::{self, Value};
pub use rocket_contrib::templates::Engines;
pub use rocket_contrib::templates::Template;

pub use crate::api::InstanceAccess;
pub use crate::api::{OnlineErrorResponse};
pub use crate::cmdlistener::*;

pub type OER = OnlineErrorResponse;

use rocket::fairing;
use rocket::response::Content;
use rocket_contrib::serve::StaticFiles;

use otter::prelude::*;

#[derive(Serialize,Debug)]
struct FrontPageRenderContext {
  debug_js_inject: Arc<String>,
}

#[derive(Copy,Clone,Debug)]
enum ResourceLocation { Main, Wasm(&'static str), }
type RL = ResourceLocation;

const RESOURCES: &[(&'static str, ResourceLocation, ContentType)] = &[
  ("script.js",    RL::Main,                       ContentType::JavaScript),
  ("LICENCE",      RL::Main,                       ContentType::Plain),
  ("libre",        RL::Main,                       ContentType::HTML),
  ("shapelib.html", RL::Main,                      ContentType::HTML),
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
        });
      }
    }
    Err(UnknownResource{})
  }
}

type PlayerQueryString<'r> = WholeQueryString<InstanceAccess<'r, PlayerId>>;

#[derive(Serialize,Debug)]
struct LoadingRenderContext<'r> {
  game: String,
  nick: String,
  layout: PresentationLayout,
  ptoken: &'r RawTokenVal,
  debug_js_inject: Arc<String>,
  movehist_lens: JsonString<&'r [usize]>,
  movehist_len_i: usize,
  movehist_len_max: usize,
}
#[get("/")]
#[throws(OER)]
fn loading_p(ia: PlayerQueryString) -> Template {
  loading(None, ia)?
}
#[get("/<layout>")]
#[throws(OER)]
fn loading_l(layout: Parse<AbbrevPresentationLayout>, ia: PlayerQueryString)
             -> Template {
  loading(Some((layout.0).0), ia)?
}

#[throws(OE)]
fn loading(layout: Option<PresentationLayout>, ia: PlayerQueryString)
           -> Template
{
  if let Some(ia) = ia.0 {
    let g = ia.i.gref.lock()?;
    let gpl = g.gs.players.byid(ia.i.ident)?;
    let layout = layout.unwrap_or(gpl.layout);
    let c = LoadingRenderContext {
      nick: gpl.nick.clone(),
      game: g.name.to_string(),
      ptoken: &ia.raw_token,
      debug_js_inject: config().debug_js_inject.clone(),
      movehist_lens: JsonString(movehist::LENS),
      movehist_len_i: movehist::LEN_DEF_I,
      movehist_len_max: movehist::LEN_MAX,
      layout,
    };
    Template::render("loading", &c)
  } else {
    let c = FrontPageRenderContext {
      debug_js_inject: config().debug_js_inject.clone(),
    };
    Template::render("front", &c)
  }
}

struct WholeQueryString<T>(pub Option<T>);

impl<'a,'r,T> FromRequest<'a,'r> for WholeQueryString<T>
  where T: 'a + FromFormValue<'a>,
        T::Error: Debug,
        for <'x> &'x T::Error: Into<rocket::http::Status>,
{
  type Error = <T as FromFormValue<'a>>::Error;
  fn from_request(r: &'a rocket::Request<'r>)
      -> rocket::Outcome<Self, (rocket::http::Status, Self::Error), ()>
  {
    eprintln!("REQUEST uri={:?}", &r.uri());
    match r.uri().query().map(|s| {
      let s = RawStr::from_str(s);
      FromFormValue::from_form_value(s)
    }).transpose() {
      Ok(v) => rocket::Outcome::Success(WholeQueryString(v)),
      Err(e) => rocket::Outcome::Failure(((&e).into(), e)),
    }
  }
}

pub struct Parse<T: FromStr>(pub T);

impl<'r, T> FromParam<'r> for Parse<T>
  where T: FromStr,
        <T as FromStr>::Err: Debug,
//  where  : Into<OE>
{
  type Error = <T as FromStr>::Err;
  #[throws(Self::Error)]
  fn from_param(param: &'r RawStr) -> Parse<T> {
    Parse(param.as_str().parse()?)
  }
}

pub struct BundleToken(pub AssetUrlToken);
impl<'r> FromFormValue<'r> for BundleToken {
  type Error = BundleDownloadError;
  #[throws(BundleDownloadError)]
  fn from_form_value(param: &'r RawStr) -> Self {
    BundleToken(param.as_str().parse()?)
  }
}

#[get("/_/updates?<ctoken>&<gen>")]
#[throws(OER)]
fn updates<'r>(ctoken: InstanceAccess<ClientId>, gen: u64,
               cors: rocket_cors::Guard<'r>)
               -> impl response::Responder<'r> {
  let gen = Generation(gen);
  let iad = ctoken.i;
  debug!("starting update stream {:?}", &iad);
  let client = iad.ident;
  let content = sse::content(iad, gen)?;
  let content = DebugReader(content, client);
  let content = response::Stream::chunked(content, 4096);
  const CTYPE: &str = "text/event-stream; charset=utf-8";
  let ctype = ContentType::parse_flexible(CTYPE).unwrap();
  cors.responder(response::content::Content(ctype, content))
}

#[get("/_/<leaf>")]
#[throws(io::Error)]
fn resource<'r>(leaf: CheckedResourceLeaf) -> impl Responder<'r> {
  let path = match leaf.locn {
    RL::Main => format!("{}/{}", config().template_dir, leaf.safe_leaf),
    RL::Wasm(s) => format!("{}/{}", config().wasm_dir, s),
  };
  Content(leaf.ctype, NamedFile::open(path)?)
}

#[derive(Error,Debug)]
pub enum BundleDownloadError {
  BadAssetUrlToken(#[from] BadAssetUrlToken),
  NotFound,
  IE(#[from] IE),
}
display_as_debug!{BundleDownloadError}
use BundleDownloadError as BDE;

impl<'r> Responder<'r> for BundleDownloadError {
  fn respond_to(self, _: &rocket::Request)
                -> Result<rocket::Response<'r>, rocket::http::Status> {
    Err((&self).into())
  }
}

impl From<&BundleDownloadError> for rocket::http::Status {
  fn from(e: &BundleDownloadError) -> rocket::http::Status {
    use rocket::http::Status as S;
    match e {
      BDE::NotFound => S::NotFound,
      BDE::BadAssetUrlToken(_) => S::Forbidden,
      BDE::IE(_) => S::InternalServerError,
    }
  }
}

#[get("/_/bundle/<instance>/<id>")]
#[throws(BundleDownloadError)]
fn bundle<'r>(instance: Parse<InstanceName>,
              id: Parse<bundles::Id>,
              token: WholeQueryString<BundleToken>)
              -> impl Responder<'r>
{
  if_let!{ Some(BundleToken(token)) = token.0; else throw!(BadAssetUrlToken) };
  let instance = &instance.0;
  let id = id.0;
  let gref = Instance::lookup_by_name_unauth(instance)
    .map_err(|_| BadAssetUrlToken)?;
  let ig = gref.lock().map_err(|_| BadAssetUrlToken)?;
  let auth = {
    let ig = ig.by_ref(Authorisation::authorise_any());
    ig.asset_url_key.check("bundle", &(instance, id), &token)?
  }.map(|(_,id)| id);
  let path = id.path(&ig, auth);
  let f = match rocket::response::NamedFile::open(&path) {
    Err(e) if e.kind() == ErrorKind::NotFound => throw!(BDE::NotFound),
    Err(e) => throw!(IE::from(AE::from(e).context(path).context("bundle"))),
    Ok(y) => y,
  };
  drop(ig);
  let ctype = match id.kind {
    bundles::Kind::Zip => ContentType::ZIP,
  };
  Content(ctype, f)
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
      }
      Some(ct) if ct == ContentType::GZIP => {
        // the only thing we serve with a .gz extension are the
        // compressed source code tarballs
        use rocket::http::hyper::header;
        response.set_header(header::TransferEncoding(vec![
          header::Encoding::Gzip,
        ]));
        response.set_header(ContentType::TAR);
      }
      _ => { /* hopefully OK */ }
    }
  }
}

#[derive(Debug,Copy,Clone)]
struct ReportStartup;
impl fairing::Fairing for ReportStartup {
  fn info(&self) -> fairing::Info {
    fairing::Info {
      name: "ReportStartup",
      kind: fairing::Kind::Launch,
    }
  }
  fn on_launch(&self, _rocket: &Rocket) {
    println!("{}", DAEMON_STARTUP_REPORT);
    std::io::stdout().flush().unwrap_or_else(
      |e| warn!("failed to report started: {:?}", &e)
    );
  }
}

#[throws(StartupError)]
fn main() {
  // todo test suite for cli at least
  // todo test suite for web api

  use structopt::StructOpt;
  #[derive(StructOpt)]
  struct Opts {
    #[structopt(long)]
    report_startup: bool,

    config_filename: Option<String>,
  }

  let opts = Opts::from_args();

  ServerConfig::read(opts.config_filename.as_ref().map(String::as_str),
                     PathResolveMethod::Chdir)?;

  std::env::set_var("ROCKET_CLI_COLORS", "off");

  let c = config();

  flexi_logger::Logger::with(log_config().clone()).start()?;

  debug!("resolved config: {:#?}", c);

  if c.check_bundled_sources {
    let check = format!("{}/otter/index.html", &c.bundled_sources);
    fs::metadata(&check)
      .context(check.clone())
      .context("check bundled-sources directory")?;
  }

  nwtemplates::init()?;
  shapelib::load_global_libs(&config().shapelibs)?;

  c.lock_save_area()?;
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

  let mut r = rocket::custom(rconfig)
    .attach(ContentTypeFixup)
    .attach(helmet)
    .attach(Template::fairing())
    .manage(cors_state)
    .mount("/", routes![
      loading_l,
      loading_p,
      bundle,
      resource,
      updates,
    ])
    .mount("/_/src", StaticFiles::from(&c.bundled_sources))
    ;

  if opts.report_startup {
    r = r.attach(ReportStartup);
  }

  let r = crate::session::mount(r);
  let r = crate::api::mount(r);

  thread::spawn(client_periodic_expiry);
  thread::spawn(logs_periodic_expiry);

  r.launch();
}
