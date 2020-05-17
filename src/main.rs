
#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use] extern crate rocket;

extern crate rocket_contrib; // why do we need this ?
extern crate serde;
extern crate thiserror;
extern crate anyhow;

mod imports;
use imports::*;

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

#[get("/updates")]
fn updates() -> response::Content<response::Stream<&'static [u8]>> {
  let ch = response::Stream::chunked(b"42".as_ref(), 1);
  let ct = ContentType::parse_flexible("text/event-stream; charset=utf-8").
    unwrap();
  response::content::Content(ct,ch)
}  

#[get("/<leaf>")]
fn resource(leaf : CheckedResourceLeaf) -> io::Result<NamedFile> {
  let template_dir = "templates"; // xxx
  NamedFile::open(format!("{}/{}", template_dir, leaf.safe))
}  

fn main() {
  let helmet = SpaceHelmet::default()
    .enable(NoSniff::Enable)
    .enable(Frame::Deny)
    .enable(Referrer::NoReferrer);

  rocket::ignite()
    .attach(helmet)
    .attach(Template::fairing())
    .mount("/", routes![
      index,
      resource,
      updates,
    ])
    .launch();
}
