
use crate::imports::*;

pub struct MgmtConnection {
  read : io::Lines<BufReader<UnixStream>>,
  write : BufWriter<UnixStream>,
}

impl MgmtConnection {
  fn connect() {
    todo!();
//    let mut both = UnixStream:;connect(SOCKET_PATH)?;
  }
}
