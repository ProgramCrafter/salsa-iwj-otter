
#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use] extern crate rocket;

extern crate rocket_contrib; // why do we need this ?

mod imports;
use imports::*;

#[get("/t")]
fn index() -> &'static str {
  "Hello, world!"
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
