
pub use rocket::request::Request;
pub use rocket::response::{Response,Responder};
pub use rocket::{post,get,routes};
pub use rocket_contrib::json::Json;
pub use rocket::http::Status;

use crate::imports::*;

impl<'r> Responder<'r> for OnlineError {
  #[throws(Status)]
  fn respond_to(self, req: &Request) -> Response<'r> {
    let msg = format!("Online-layer error\n{:?}\n{}\n", self, self);
    use rocket::http::Status;
    use OnlineError::*;
    let status = match self {
      ServerFailure(_) => Status::InternalServerError,
      NoClient | NoPlayer | GameBeingDestroyed
        => Status::NotFound,
      OnlineError::PieceHeld | OnlineError::PieceGone
        => Status::Conflict,
      InvalidZCoord | BadJSON(_)
        => Status::BadRequest,
    };
    let mut resp = Responder::respond_to(msg,req).unwrap();
    resp.set_status(status);
    resp
  }
}
