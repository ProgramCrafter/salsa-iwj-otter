
use actix_web::{get, web, App, HttpServer, Responder};
use actix_web::FromRequest;
use actix_web::HttpRequest;
use actix_web::http::Method;
use actix_web::HttpResponse;
use actix_web::dev::Payload;

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

#[get("/wombat")]
async fn wombat(remain: Remain) -> impl Responder {
    format!("Hello {:?}", remain)
}

use fehler::throws;
#[throws(actix_web::Error)]
async fn not_found_handler(method: Method) -> impl Responder {
  match method {
    Method::GET => HttpResponse::NotFound().body("Not found.")
    ,
    _  => HttpResponse::MethodNotAllowed().finish(),
  }
}

#[actix_web::main] // or #[tokio::main]
async fn main() -> std::io::Result<()> {
  HttpServer::new(|| App::new()
                  .service(wombat)
                  .service(index)
                  .default_service(web::to(not_found_handler))
  )
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
