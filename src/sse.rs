#![allow(clippy::while_let_loop)]
#![allow(clippy::blocks_in_if_conditions)]

use crate::imports::*;

use vecdeque_stableix::StableIndexOffset;
use std::ops::Neg;

// ---------- basic definitions ----------

#[derive(Copy,Clone,Debug,Eq,PartialEq,Ord,PartialOrd)]
#[derive(Serialize,Deserialize)]
#[serde(transparent)]
pub struct UpdateId (i64);

const UPDATE_READER_SIZE : usize = 1024*32;
const UPDATE_MAX_FRAMING_SIZE : usize = 200;
const UPDATE_KEEPALIVE : Duration = Duration::from_secs(14);

struct UpdateReaderWN {
  player : PlayerId,
  client : ClientId,
  to_send : UpdateId,
}

struct UpdateReader {
  wn: UpdateReaderWN,
  need_flush : bool,
  gref : InstanceRef,
  keepalives : Wrapping<u32>,
  init_confirmation_send : iter::Once<()>,
}

impl Deref for UpdateReader {
  type Target = UpdateReaderWN;
  fn deref(&self) -> &UpdateReaderWN { &self.wn }
}

#[derive(Error,Debug)]
#[error("WouldBlock error misreported!")]
struct FlushWouldBlockError{}

impl UpdateReaderWN {
  #[throws(io::Error)]
  fn write_next<U>(&mut self, mut buf: &mut U, next: &PreparedUpdate)
                   where U : Write {
    let tu = next.for_transmit(self.client);

    write!(buf, "data: ")?;
    serde_json::to_writer(&mut buf, &tu)?;
    write!(buf, "\n\
                 id: {}\n\n",
           self.to_send)?;

    debug!("sending to {:?} {:?}: {:?}",
           &self.player, &self.client, &tu);

    self.to_send.try_increment().unwrap();
  }
}

impl Read for UpdateReader {
  fn read(&mut self, orig_buf: &mut [u8]) -> Result<usize,io::Error> {
    let em : fn(&'static str) -> io::Error =
      |s| io::Error::new(io::ErrorKind::Other, anyhow!(s));

    let mut ig = self.gref.lock().map_err(|_| em("poison"))?;
    let orig_wanted = orig_buf.len();
    let mut buf = &mut *orig_buf;

    if self.init_confirmation_send.next().is_some() {
      write!(buf, "event: commsworking\n\
                   data: server online {} {} G{}\n\n",
             self.player, self.client, self.to_send)?;
    }

    let pu = &mut ig.updates.get(self.player)
      .ok_or_else(|| em("player gonee"))?;

    loop {
      let next = match pu.read_log().get(self.to_send) {
        Some(next) => next,  None => { break }
      };
      let next_len = UPDATE_MAX_FRAMING_SIZE + next.json_len();
      if next_len > buf.len() { break }

      // xxx handle overflow by allocating
      self.wn.write_next(&mut buf, &next)
        .map_err(|e| {
          error!("UpdateReader.write_next: {} {} {:?}",
                 &self.player, &self.client, e);
          e
        })?;

    }

    let cv = pu.get_cv();

    loop {
      let generated = orig_wanted - buf.len();
      if generated > 0 {
        self.need_flush = true;
        return Ok(generated)
      }

      if self.need_flush {
        self.need_flush = false;
        return Err(io::Error::new(io::ErrorKind::WouldBlock,
                                  FlushWouldBlockError{}));
      }

      if (||{
        (*ig).gs.players.get(self.player)?;
        let client = ig.clients.get_mut(self.client)?;
        client.lastseen = Instant::now();
        Some(())
      })() == None { return Ok(0) }

      ig.c = cv.wait_timeout(ig.c, UPDATE_KEEPALIVE)
        .map_err(|_| em("poison"))?.0;

      write!(buf, "event: commsworking\n\
                   data: server online {} {} G{} K{}\n\n",
             self.player, self.client, self.to_send, self.keepalives)?;
      self.keepalives += Wrapping(1);
/*
      write!(buf,": keepalive\n\n")?; */
    }
  }
}

// ---------- support implementation ----------

impl Neg for UpdateId {
  type Output = Self;
  fn neg(self) -> Self { UpdateId(-self.0) }
}

impl Display for UpdateId {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    Display::fmt(&self.0,f)
  }
}

impl StableIndexOffset for UpdateId {
  fn try_increment(&mut self) -> Option<()> { self.0.try_increment() }
  fn try_decrement(&mut self) -> Option<()> { self.0.try_decrement() }
  fn index_input(&self, input: Self) -> Option<usize> {
    self.0.index_input(input.0)
  }
  fn index_output(&self, inner: usize) -> Option<Self> {
    self.0.index_output(inner).map(UpdateId)
  }
  fn zero() -> Self { UpdateId(0) }
}

// ---------- entrypoint for dribbling the http response ----------

#[throws(OE)]
pub fn content(iad : InstanceAccessDetails<ClientId>, gen: Generation)
  -> impl Read {
  let client = iad.ident;

  let content = {
    let mut g = iad.gref.lock()?;
    let _g = &mut g.gs;
    let cl = g.clients.byid(client)?;
    let player = cl.player;
    trace!("updates content iad={:?} player={:?} cl={:?} updates={:?}",
           &iad, &player, &cl, &g.updates);
    let gref = iad.gref.clone();

    let log = &g.updates.byid(player)?.read_log();

    let to_send = match log.into_iter().rev()
      .find(|(_,update)| update.gen <= gen) {
        None => log.front_index(),
        Some((mut i,_)) => { i.try_increment(); i },
      };
    
    UpdateReader {
      need_flush : false,
      keepalives : Wrapping(0),
      gref,
      init_confirmation_send : iter::once(()),
      wn : UpdateReaderWN {
        player, client, to_send,
      },
    }
  };
  let content = BufReader::with_capacity(UPDATE_READER_SIZE, content);
  //DebugReader(content)
  content
}
