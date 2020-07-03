
#![feature(proc_macro_hygiene, decl_macro)]

use crate::imports::*;

#[derive(Copy,Clone,Debug,Eq,PartialEq,Ord,PartialOrd)]
#[derive(Serialize,Deserialize)]
#[serde(transparent)]
pub struct UpdateId (i64);

use vecdeque_stableix::StableIndexOffset;
use std::ops::Neg;

const UPDATE_READER_SIZE : usize = 1024*32;
const UPDATE_MAX_FRAMING_SIZE : usize = 200;
const UPDATE_KEEPALIVE : Duration = Duration::from_secs(14);

impl Neg for UpdateId {
  type Output = Self;
  fn neg(self) -> Self { UpdateId(-self.0) }
}

impl StableIndexOffset for UpdateId {
  fn try_increment(&mut self) -> Option<()> { self.0.try_increment() }
  fn try_decrement(&mut self) -> Option<()> { self.0.try_decrement() }
  fn index_input(&self, input: Self) -> Option<usize> {
    self.0.index_input(input.0)
  }
  fn index_output(&self, inner: usize) -> Option<Self> {
    self.0.index_output(inner).map(|v| UpdateId(v))
  }
  fn zero() -> Self { UpdateId(0) }
}

struct UpdateReader {
  player : PlayerId,
  client : ClientId,
  to_send : UpdateId, // xxx race for setting this initially
  ami : Arc<Mutex<Instance>>,
}

impl Read for UpdateReader {
  fn read(&mut self, mut buf: &mut [u8]) -> io::Result<usize> {
    let em : fn(&'static str) -> io::Error = |s|
    io::Error::new(io::ErrorKind::Other, anyhow!(s));

    let amig = self.ami.lock().map_err(|_| em("poison"))?;
    let orig_wanted = buf.len();

    let pu = &mut amig.updates.get(self.player)
      .ok_or_else(|| em("player gonee"))?;
    loop {
      let next = match pu.log.get(self.to_send) {
        Some(next) => next,  None => { break }
      };
      let next_len = UPDATE_MAX_FRAMING_SIZE + next.json.len();
      if next_len > buf.len() { break }

      if next.client == self.client {
        write!(buf, r#"
event: recorded
data: {{ gen: {}, piece: {}, cseq:{} }}
"#,
               &next.gen, &next.piece, &next.client_seq);
      } else {
        write!(buf, r#"
id: {}
data: {}
"#,
               &self.to_send,
               &next.json);
      }
    }
    loop {
      let generated = orig_wanted - buf.len();
      if generated > 0 { return generated }

      amig = self.cv.wait_timeout(amig, UPDATE_KEEPALIVE)?.0;
      write!(buf,r#"
: keepalive
"#);
    }
  }
}

    /*
    loop {
                    e
      let send_from = (||{
        let l = self.updates.len();
        let last_probe = match updates.last() {
          None => return None,
          Some(&now) if self.last_sent > now.gen => return l+1,
          _ => l,
        };
        let (lo, hi /* half-open */) = loop {
          let depth = l - last_probe;
          depth *= 2;
          if depth > l { break (0, last_probe) }
          let probe = l - depth;
          let here = updates[probe];
          if here.gen < l 

        if let Some(&now) =  {
          if  { return None }
        }
        let probe = inst.updates.len() - 1;
        let (lo, hi) = loop {
          if search == 0 { break }
          search -= 1;
          tu = inst.updates[search];
          if 

        let lo = 0;
        
      };
    loop {
         implement this! 
    }
    for (tclient, tcl) in &mut g.clients {
      if tclient == client {
        tcl.transmit_update(&Update {
          gen,
          u : UpdatePayload::ClientSequence(piece, form.s),
        });
      } else {
        tcl.transmit_update(&update);
      }          
    }
     */
/*

    thread::sleep(Duration::from_millis(500));
    let message = XUpdate::TestCounter { value : self.next };
    let data = serde_json::to_string(&message)?;
    let data = format!("data: {}\n\n", &data);
    // eprintln!("want to return into &[;{}] {:?}", buf.len(), &data);
    self.next += 1;
    buf[0..data.len()].copy_from_slice(data.as_bytes());
    Ok(buf.len())
  }
}*/

/*
#[derive(Deserialize)]
struct APIForm {
  t : String,
  c : ClientId,
}
 */

#[throws(E)]
pub fn content(iad : InstanceAccessDetails<ClientId>, gen: Generation)
  -> impl Read {
  let client = iad.ident;

  let content = {
    let mut ig = iad.g.lock().map_err(|e| anyhow!("lock poison {:?}",&e))?;
    let g = &mut ig.gs;
    let cl = ig.clients.get(client).ok_or_else(|| anyhow!("no client"))?;
    let player = cl.player;
    let ami = iad.g.clone();

    let to_send = UpdateId(42); // xxx
    
    UpdateReader { player, client, to_send, ami }
  };
  BufReader::with_capacity(UPDATE_READER_SIZE, content)
}
