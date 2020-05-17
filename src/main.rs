
#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use] extern crate rocket;

extern crate rocket_contrib; // why do we need this ?
extern crate serde;

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

fn main() {
  let helmet = SpaceHelmet::default()
    .enable(NoSniff::Enable)
    .enable(Frame::Deny)
    .enable(Referrer::NoReferrer);

  rocket::ignite()
    .attach(helmet)
    .attach(Template::fairing())
    .mount("/", routes![
      index
    ])
    .launch();
}
