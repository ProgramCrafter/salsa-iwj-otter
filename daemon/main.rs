// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![feature(lint_reasons)]
#![feature(proc_macro_hygiene, decl_macro)]

use otter::imports::*;

pub mod imports;

pub mod api;
pub mod cmdlistener;
pub mod session;
pub mod sse;

pub use std::pin::Pin;
pub use futures::future;
pub use futures::FutureExt as _;

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
  tera: Tera,
}

impl Templates {
  #[throws(StartupError)]
  pub fn new(template_dir: &str) -> Self {
    let mut tera = Tera::default();
    (||{
      let files: Vec<_> = 
        fs::read_dir(template_dir).context("open directory")?
        .filter_map_ok(|entry| {
          let leaf = entry.file_name().into_string().ok()?;
          let leaf = leaf.strip_suffix(".tera")?;
          Some((entry.path(), Some(leaf.to_owned())))
        })
        .try_collect().context("read directory")?;
      tera.add_template_files(files).context("process templates")?;
      Ok::<_,StartupError>(())
    })().with_context(|| format!("{:?}", template_dir))?;
    Templates { tera }
  }

  #[throws(InternalError)]
  pub fn render<C: Serialize>(&self, template: &str, c: C) -> Template {
    #[throws(InternalError)]
    fn inner(tmpls: &Templates, template: &str, c: tera::Result<tera::Context>)
             -> Template {
      let s = tmpls.tera.render(template, &c?)?;
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
struct UnknownResource;

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
async fn r_loading_p(ia: PlayerQueryString,
                   templates: Data<Templates>) -> Template {
  loading(None, ia, templates)?
}
#[route("/{layout}", method="GET", method="HEAD")]
#[throws(FER)]
async fn r_loading_l(layout: Path<Parse<AbbrevPresentationLayout>>,
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
  type Future = future::Ready<Result<WholeQueryString<T,E>, E>>;
  type Error = E;
  fn from_request(req: &HttpRequest, _: &mut actix_web::dev::Payload)
                  -> Self::Future {
    future::ready(
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
where T: FromStr + Debug,
      T::Err: std::error::Error + Debug,
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
async fn r_updates(query: Query<UpdatesParams>) -> impl Responder {
  let UpdatesParams { ctoken, gen } = query.into_inner();
  let gen = Generation(gen);
  let iad = ctoken.0.i;
  debug!("starting update stream {:?}", &iad);
  let content = sse::content(iad, gen)?;
  HttpResponse::build(StatusCode::OK)
    .content_type("text/event-stream; charset=utf-8")
    .streaming(content)
}

// Actix dispatches to the first matching URL pattern in the list of
// routes, and does not try another one even if this route says 404.
// This pattern is quite general, so it has to come last.
//
// An alternative would be to use a Guard.  But the guard doesn't get
// to give information directly to the route function.  We would have
// to use the GuardContext to thread the CheckedResource through the
// Extension type map.  It's easier just to make sure this URL pattern
// is last in the list.
#[route("/_/{leaf}", method="GET", method="HEAD")]
#[throws(io::Error)]
async fn r_resource(leaf: Path<Parse<CheckedResourceLeaf>>) -> impl Responder {
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

  fn error_response(&self) -> HttpResponse<BoxBody> { error_response(self) }
}

#[route("/_/bundle/{instance}/{id}", method="GET", method="HEAD")]
#[throws(BundleDownloadError)]
async fn r_bundle(path: Path<(
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

fn error_response<E>(self_: &E) -> HttpResponse<BoxBody>
where E: ResponseError + Debug + Display
{
  let status = self_.status_code();
  if status == StatusCode::INTERNAL_SERVER_ERROR {
    error!("responding with internal error -- {} -- {:?}", self_, self_);
  }
  self_.status_code().respond_text(
    &format_args!("{}\n{:?}\n", self_, self_)
  )
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
        // We name these r_* because actix's #[route] macro defines
        // them as unit structs, not functions or data values.  The
        // result is that they are *type names* which makes them
        // impossible to locally rebind.
        r_loading_l,
        r_loading_p,
        r_bundle,
        r_updates,
        session::routes(),
        api::routes(),
        r_resource, // Must come after more specific URL paths
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
      .wrap(middleware::Logger::default())
      ;

    app

  }});

  let mut http = http
    .disable_signals();

  for addr in &c.listen {
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
