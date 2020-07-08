
use rocket::request::Request;
use rocket::response::{Response,Responder};

use crate::imports::*;

impl<'r> Responder<'r> for OnlineError {
  #[throws(Status)]
  fn respond_to(self, req: &Request) -> Response<'r> {
    let msg = format!("Online-layer error\n{:?}\n{}\n", self, self);
    use rocket::http::Status;
    use OnlineError::*;
    let status = match self {
      GameCorrupted => Status::InternalServerError,
      NoClient | NoPlayer => Status::NotFound,
      InvalidZCoord => Status::BadRequest,
    };
    let mut resp = Responder::respond_to(msg,req).unwrap();
    resp.set_status(status);
    resp
  }
}
