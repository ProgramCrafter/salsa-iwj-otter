
#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use] extern crate rocket;

extern crate rocket_contrib; // why do we need this ?
extern crate serde;
extern crate thiserror;
extern crate anyhow;

mod imports;
use imports::*;

type RE = E;

pub type InstanceName = String;

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

type TestCounter = BufReader<TestCounterInner>;
#[derive(Debug)]
struct TestCounterInner { next : usize, }
impl Read for TestCounterInner {
  fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
    thread::sleep(Duration::from_millis(500));
    let data = format!("data: {}\n\n", self.next);
    self.next += 1;
    buf[0..data.len()].copy_from_slice(data.as_bytes());
    Ok(buf.len())
  }
}

struct MainRenderContext { };

struct GraspForm {
  a : InstanceAccess,
  pn : usize,
  grasped : bool,
};
#[post("/_/api/grasp")]
fn mainpage(f : GraspForm) -> impl xxx json somehow response::Responder<'static> {
  let mut g = f.a.i.lock();
  let p = g.pieces.
}

#[post("/<access>")]
fn mainpage(access : InstanceAccess) -> impl response::Responder<'static> {
  let c = MainRenderContext { };
  Template::render("main",&c)
}

/*

  let tc = TestCounterInner { next : 0 };
  let tc = BufReader::new(tc);
  let ch = response::Stream::chunked(tc, 1);
  let ct = ContentType::parse_flexible("text/event-stream; charset=utf-8").
    unwrap();
  response::content::Content(ct,ch)
}  
*/

#[get("/_/<leaf>")]
fn resource(leaf : CheckedResourceLeaf) -> io::Result<NamedFile> {
  let template_dir = "templates"; // xxx
  NamedFile::open(format!("{}/{}", template_dir, leaf.safe))
}  

fn main() {
  testload()?;

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
