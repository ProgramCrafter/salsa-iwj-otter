
use actix_web::{get, web, App, HttpServer, Responder};
use actix_web::FromRequest;
use actix_web::HttpRequest;
use actix_web::dev::Payload;

use std::convert::Infallible;

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

#[actix_web::main] // or #[tokio::main]
async fn main() -> std::io::Result<()> {
  HttpServer::new(|| App::new()
                  .service(wombat)
                  .service(index)
  )
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
