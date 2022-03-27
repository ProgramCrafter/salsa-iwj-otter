use actix_files::Files;
use actix_web::{middleware::Logger, App, HttpServer};
use actix_web::{get, Responder, HttpResponse};
use otter::imports::*;

#[get("/wombat")]
async fn wombat() -> impl Responder {
  HttpResponse::Ok().body("WOMBAT\n")
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    log::info!("starting HTTP server at http://localhost:8080");

    HttpServer::new(|| {
        App::new()
            // We allow the visitor to see an index of the images at `/images`.
            .service(wombat)
            .service(Files::new("/images", "static/images/").show_files_listing())
            // Serve a tree of static files at the web root and specify the index file.
            // Note that the root path should always be defined as the last item. The paths are
            // resolved in the order they are defined. If this would be placed before the `/images`
            // path then the service for the static images would never be reached.
            .service(Files::new("/", "./static/root/").index_file("index.html"))
            // Enable the logger.
            .wrap(Logger::default())
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
