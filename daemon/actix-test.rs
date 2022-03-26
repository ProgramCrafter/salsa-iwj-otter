
#![allow(unused_imports)]

use actix_web::{get, head, web, route, App, HttpServer, Responder};
use actix_web::FromRequest;
use actix_web::HttpRequest;
use actix_web::http::Method;
use actix_web::HttpResponse;
use actix_web::dev::Payload;
use actix_web::middleware;
use actix_cors::Cors;

use std::convert::Infallible;

//use otter::imports::*;

//use futures::Future;

#[get("/{id}/{name}/index.html")]
async fn index(params: web::Path<(u32, String)>) -> impl Responder {
    let (id, name) = params.into_inner();
    format!("Hello {}! id:{}", name, id)
}

#[derive(Debug, Default)]
struct Remain {
  #[allow(dead_code)]
  q: Option<String>,
}

impl FromRequest for Remain {
  type Future = futures::future::Ready<Result<Remain, Infallible>>;
  type Error = Infallible;
  fn from_request(req: &HttpRequest, _: &mut Payload)
                  -> Self::Future {
    let q = req.uri().query().map(ToOwned::to_owned);
    let r = Remain { q };
    futures::future::ready(Ok(r))
  }
}

//#[route("/wombat", method="GET", method="HEAD")]
#[get("/wombat")]
async fn wombat(remain: Remain) -> impl Responder {
    format!("Hello {:?}", remain)
}

fn update_cors() -> Cors {
  Cors::default()
      .allowed_methods([Method::GET])

}

//#[route("/wombat", method="GET", method="HEAD")]
#[route("/foo", method="GET", method="HEAD",
        wrap = "update_cors()")]
//#[get("/foo")]
async fn foo() -> impl Responder {
  "foo\r\n"
}

use fehler::throws;
async fn not_found_handler(method: Method) -> impl Responder {
  match method {
    Method::GET | Method::HEAD => HttpResponse::NotFound()
      .content_type("text/plain; charset=utf-8")
      .body("Not found.")
    ,
    _  => HttpResponse::MethodNotAllowed().finish(),
  }
}

#[actix_web::main] // or #[tokio::main]
async fn main() -> std::io::Result<()> {
  HttpServer::new(|| App::new()
                  .service(wombat)
                  .service(index)
                  .service(foo)
                  .default_service(web::to(not_found_handler))
                  .wrap(
middleware::DefaultHeaders::new().add((
  actix_web::http::header::X_CONTENT_TYPE_OPTIONS,
  "nosniff"
))
                  )
  )
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
