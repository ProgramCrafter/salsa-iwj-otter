// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![feature(lint_reasons)]
#![feature(proc_macro_hygiene, decl_macro)]

use otter::imports::thiserror;

pub mod imports;

pub mod api;
pub mod cmdlistener;
pub mod session;
pub mod sse;

pub use std::pin::Pin;

pub use crate::api::InstanceAccess;
pub use crate::api::{FatalErrorResponse};
pub use crate::cmdlistener::*;

pub type FER = FatalErrorResponse;

use actix_web::{route, post, HttpServer, Responder};
//App, 
use actix_web::{HttpResponse, HttpResponseBuilder, ResponseError};
use actix_web::{HttpRequest, FromRequest};
use actix_web::services;
use actix_web::dev::HttpServiceFactory;
use actix_web::web::{self, Bytes, Data, Json, Path, Query};
use actix_web::body::BoxBody;
use actix_web::http::header;
use actix_web::http::{Method, StatusCode};
use actix_web::middleware;
use actix_files::NamedFile;
use actix_cors::Cors;

use otter::prelude::*;
use otter::imports::tera_standalone as tera;
use tera::Tera;

const CT_JAVASCRIPT: mime::Mime = mime::APPLICATION_JAVASCRIPT_UTF_8;
const CT_TEXT:      mime::Mime = mime::TEXT_PLAIN_UTF_8;
const CT_HTML:      mime::Mime = mime::TEXT_HTML_UTF_8;
const CT_ZIP: &'static str = "application/zip";
const CT_WASM: &'static str = "application/wasm";

trait IntoMime: Debug {
  fn into_mime(&self) -> mime::Mime;
}
impl IntoMime for &str {
  fn into_mime(&self) -> mime::Mime { self.parse().expect(self) }
}
impl IntoMime for mime::Mime {
  fn into_mime(&self) -> mime::Mime { self.clone() }
}
type ConstContentType = &'static dyn IntoMime;

#[derive(Serialize,Debug)]
struct FrontPageRenderContext {
  debug_js_inject: Arc<String>,
}

pub type Template = HttpResponse;

pub struct Templates {
  tera: tera::Tera,
}

impl Templates {
  #[throws(StartupError)]
  pub fn new(template_dir: &str) -> Self {
    let tera = Tera::new(&format!("{}/*.tera", template_dir))
    .context("initialise templates")?;
    Templates { tera }
  }

  #[throws(InternalError)]
  pub fn render<C: Serialize>(&self, template: &str, c: C) -> Template {
    #[throws(InternalError)]
    fn inner(tmpls: &Templates, template: &str, c: tera::Result<tera::Context>)
             -> Template {
      let s = tmpls.tera.render(&format!("{}.tera", template), &c?)?;
      HttpResponseBuilder::new(StatusCode::OK)
        .content_type(CT_HTML)
        .body(s)
    }

    let c = tera::Context::from_serialize(c);
    inner(self, template, c)?
  }
}

#[derive(Copy,Clone,Debug)]
enum ResourceLocation { Main, Wasm(&'static str), }
type RL = ResourceLocation;

const RESOURCES: &[(&'static str, ResourceLocation, ConstContentType)] = &[
  ("script.js",    RL::Main,                       &CT_JAVASCRIPT),
  ("LICENCE",      RL::Main,                       &CT_TEXT),
  ("libre",        RL::Main,                       &CT_HTML),
  ("shapelib.html", RL::Main,                      &CT_HTML),
  ("AGPLv3",       RL::Main,                       &CT_TEXT),
  ("CC-BY-SA-3.0", RL::Main,                       &CT_TEXT),
  ("CC-BY-SA-4.0", RL::Main,                       &CT_TEXT),
  ("wasm.wasm",    RL::Wasm("otter_wasm_bg.wasm"), &CT_WASM),
  ("wasm.js",      RL::Wasm("otter_wasm.js"),      &CT_JAVASCRIPT),
];

#[derive(Debug)]
struct CheckedResourceLeaf {
  safe_leaf: &'static str,
  locn: ResourceLocation,
  ctype: ConstContentType,
}

#[derive(Error,Debug)]
#[error("not a valid resource path")]
struct UnknownResource{}

impl FromStr for CheckedResourceLeaf {
  type Err = UnknownResource;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    for &(safe_leaf, locn, ref ctype) in RESOURCES {
      if safe_leaf == s {
        return Ok(CheckedResourceLeaf {
          safe_leaf, locn,
          ctype: ctype.clone(),
        });
      }
    }
    Err(UnknownResource{})
  }
}

type PlayerQueryString = WholeQueryString<
    InstanceAccess<PlayerId>,
    FER,
  >;

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
#[route("/", method="GET", method="HEAD")]
#[throws(FER)]
async fn loading_p(ia: PlayerQueryString,
                   templates: Data<Templates>) -> Template {
  loading(None, ia, templates)?
}
#[route("/{layout}", method="GET", method="HEAD")]
#[throws(FER)]
async fn loading_l(layout: Path<Parse<AbbrevPresentationLayout>>,
                   ia: PlayerQueryString,
                   templates: Data<Templates>)
             -> Template {
  loading(Some((layout.0).0), ia, templates)?
}

#[throws(Fatal)]
fn loading(layout: Option<PresentationLayout>, ia: PlayerQueryString,
           templates: Data<Templates>)
           -> Template
{
  if let Some(ia) = ia.0 {
    let g = ia.i.gref.lock()?;
    let gpl = g.gs.players.byid(ia.i.ident)?;
    let layout = layout.unwrap_or(gpl.layout);
    let c = LoadingRenderContext {
      nick: gpl.nick.clone(),
      game: g.name.to_string(),
      ptoken: ia.raw_token.borrow(),
      debug_js_inject: config().debug_js_inject.clone(),
      movehist_lens: JsonString(movehist::LENS),
      movehist_len_i: movehist::LEN_DEF_I,
      movehist_len_max: movehist::LEN_MAX,
      layout,
    };
    templates.render("loading", &c)?
  } else {
    let c = FrontPageRenderContext {
      debug_js_inject: config().debug_js_inject.clone(),
    };
    templates.render("front", &c)?
  }
}

struct WholeQueryString<T,E>(pub Option<T>, PhantomData<E>);
impl<T,E> From<Option<T>> for WholeQueryString<T,E> {
  fn from(v: Option<T>) -> Self { Self(v, default()) }
}

impl<T,E> FromRequest for WholeQueryString<T,E>
where T: FromStr,
      E: ResponseError + 'static,
      T::Err: Into<E>
//      T::Error: Debug,
{
  type Future = futures::future::Ready<Result<WholeQueryString<T,E>, E>>;
  type Error = E;
  fn from_request(req: &HttpRequest, _: &mut actix_web::dev::Payload)
                  -> Self::Future {
    futures::future::ready(
      req.uri().query()
        .map(|s| s.parse())
        .transpose()
        .map_err(Into::into)
        .map(WholeQueryString::from)
    )
  }
}

#[derive(Debug)]
pub struct Parse<T: FromStr>(pub T);

impl<'de,T> Deserialize<'de> for Parse<T>
where T: FromStr,
      T::Err: std::error::Error,
{
  #[throws(D::Error)]
  fn deserialize<D: Deserializer<'de>>(d: D) -> Self {
    struct ParseVisitor;
    impl<'vde> serde::de::Visitor<'vde> for ParseVisitor {
      type Value = Cow<'vde, str>;
      #[throws(E)]
      fn visit_str<E:Error>(self, v: &str) -> Self::Value {
        v.to_owned().into()
      }
      #[throws(E)]
      fn visit_borrowed_str<E:Error>(self, v: &'vde str) -> Self::Value {
        v.into()
      }
      #[throws(fmt::Error)]
      fn expecting(&self, f: &mut Formatter<'_>) {
        write!(f, "string, from URL path")?;
      }
    }
    
    let s = d.deserialize_str(ParseVisitor)?;
    let v = s.parse().map_err(|e: T::Err| D::Error::custom(e.to_string()))?;
    Parse(v)
  }
}

//pub struct BundleToken(pub AssetUrlToken);
/*
impl<'r> FromFormValue<'r> for BundleToken {
  type Error = BundleDownloadError;
  #[throws(BundleDownloadError)]
  fn from_form_value(param: &'r RawStr) -> Self {
    BundleToken(param.as_str().parse()?)
  }
}
*/

fn updates_cors() -> Cors {

  Cors::default()
      .allowed_methods([Method::GET])
}

#[derive(Debug, Deserialize)]
struct UpdatesParams {
  ctoken: Parse<InstanceAccess<ClientId>>,
  gen: u64,
}

#[route("/_/updates", method="GET", wrap="updates_cors()")]
#[throws(FER)]
async fn updates_route(query: Query<UpdatesParams>) -> impl Responder {
  let UpdatesParams { ctoken, gen } = query.into_inner();
  let gen = Generation(gen);
  let iad = ctoken.0.i;
  debug!("starting update stream {:?}", &iad);
  let content = sse::content(iad, gen)?;
  HttpResponse::build(StatusCode::OK)
    .content_type("text/event-stream; charset=utf-8")
    .streaming(content)
}

#[route("/_/{leaf}", method="GET", method="HEAD")]
#[throws(io::Error)]
async fn resource(leaf: Path<Parse<CheckedResourceLeaf>>) -> impl Responder {
  let leaf = leaf.into_inner().0;
  let path = match leaf.locn {
    RL::Main => format!("{}/{}", config().template_dir, leaf.safe_leaf),
    RL::Wasm(s) => format!("{}/{}", config().wasm_dir, s),
  };
  NamedFile::open(path)?
    .disable_content_disposition()
    .prefer_utf8(true)
    .set_content_type(leaf.ctype.into_mime())
}

#[derive(Error,Debug)]
pub enum BundleDownloadError {
  BadAssetUrlToken(#[from] BadAssetUrlToken),
  NotFound,
  IE(#[from] IE),
}
display_as_debug!{BundleDownloadError}
use BundleDownloadError as BDE;

impl ResponseError for BundleDownloadError {
  fn status_code(&self) -> StatusCode {
    match self {
      BDE::NotFound            => StatusCode::NOT_FOUND,
      BDE::BadAssetUrlToken(_) => StatusCode::FORBIDDEN,
      BDE::IE(_)               => StatusCode::INTERNAL_SERVER_ERROR,
    }
  }
}

#[route("/_/bundle/{instance}/{id}", method="GET", method="HEAD")]
#[throws(BundleDownloadError)]
async fn bundle_route(path: Path<(
  Parse<InstanceName>,
  Parse<bundles::Id>,
)>, token: WholeQueryString<AssetUrlToken, BundleDownloadError>
) -> impl Responder {
  let (instance, id) = path.into_inner();
  if_let!{ Some(token) = token.0; else throw!(BadAssetUrlToken) };
  let instance = &instance.0;
  let id = id.0;
  let gref = Instance::lookup_by_name_unauth(instance)
    .map_err(|_| BadAssetUrlToken)?;
  let ig = gref.lock().map_err(|_| BadAssetUrlToken)?;
  let auth = {
    let ig = ig.by_ref(Authorisation::promise_any());
    ig.asset_url_key.check("bundle", &(instance, id), &token)?
  }.map(|(_,id)| id);
  let path = id.path(&ig, auth);
  let f = match NamedFile::open(&path) {
    Err(e) if e.kind() == ErrorKind::NotFound => throw!(BDE::NotFound),
    Err(e) => throw!(IE::from(AE::from(e).context(path).context("bundle"))),
    Ok(y) => y,
  };
  drop(ig);
  let ctype = match id.kind {
    bundles::Kind::Zip => CT_ZIP,
  };
  f
    .disable_content_disposition()
    .set_content_type(ctype.into_mime())
}

/*
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
*/

fn on_launch() {
  println!("{}", DAEMON_STARTUP_REPORT);
  std::io::stdout().flush().unwrap_or_else(
    |e| warn!("failed to report started: {:?}", &e)
  );
}

#[ext]
impl StatusCode {
  fn respond_text(self, t: &dyn Display) -> HttpResponse {
    HttpResponse::build(self)
      .content_type(CT_TEXT)
      .body(t.to_string())
  }
}

async fn not_found_handler(method: Method) -> impl Responder {
  match method {
    Method::GET | Method::HEAD | Method::POST =>
      StatusCode::NOT_FOUND.respond_text(&"404 Not found."),
    _  =>
      StatusCode::METHOD_NOT_ALLOWED.respond_text(&"Unsupported HTTP method"),
  }
}

#[actix_web::main] // not compatible with fehler
async fn main() -> Result<(),StartupError> {
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
  let templates = Templates::new(&c.template_dir)?;
  load_accounts()?;
  load_games(&mut AccountsGuard::lock(), &mut games_lock())?;

  let cl = CommandListener::new()?;
  cl.spawn()?;

//  let updates = updates.wrap(

  let c = Arc::new(c);
  let templates = Data::new(templates);

  let http = HttpServer::new({
    let c = c.clone();
  move || {

    let json_config = actix_web::web::JsonConfig::default()
      .content_type(|ctype| ctype == mime::APPLICATION_JSON)
      .content_type_required(true);

     let src_service = actix_files::Files::new("/_/src", &c.bundled_sources)
       .show_files_listing()
       .redirect_to_slash_directory()
       .index_file("index.html")
       .disable_content_disposition();

    let app = actix_web::App::new()
      .service(services![
        loading_l,
        loading_p,
        bundle_route,
        updates_route,
        resource,
        session::routes(),
        api::routes(),
      ])
      .app_data(json_config)
      .app_data(templates.clone())
      .service(src_service)
      .default_service(web::to(not_found_handler))
      .wrap(middleware::DefaultHeaders::new()
            .add((header::X_CONTENT_TYPE_OPTIONS, "nosniff"))
            .add((header::X_FRAME_OPTIONS, "DENY"))
            .add((header::REFERRER_POLICY, "no-referrer"))
      )
      ;

    app

  }});

  let mut http = http
    .disable_signals();

  let (addrs, def_port): (&[&dyn IpAddress], _) = if c.debug {
    (&[&Ipv6Addr::LOCALHOST,   &Ipv4Addr::LOCALHOST  ], 8000)
  } else {
    (&[&Ipv6Addr::UNSPECIFIED, &Ipv4Addr::UNSPECIFIED], 80)
  };
  let port = c.http_port.unwrap_or(def_port);

  for addr in addrs {
    let addr = addr.with_port(port);
    http = http.bind(addr)
      .with_context(|| format!("bind {:?}", addr))?;
  }

  thread::spawn(game_flush_task);

  thread::spawn(client_periodic_expiry);
  thread::spawn(logs_periodic_expiry);

  if opts.report_startup {
    on_launch();
  }

  http.run().await.context("after startup")?;

  Ok(())
}
