// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(clippy::while_let_loop)]
#![allow(clippy::blocks_in_if_conditions)]

use crate::prelude::*;

use vecdeque_stableix::Offset as StableIndexOffset;
use std::ops::Neg;

// ---------- basic definitions ----------

#[derive(Copy,Clone,Debug,Eq,PartialEq,Ord,PartialOrd)]
#[derive(Serialize,Deserialize)]
#[serde(transparent)]
pub struct UpdateId(i64);

const UPDATE_READER_SIZE: usize = 1024*32;
const UPDATE_MAX_FRAMING_SIZE: usize = 200;
const UPDATE_KEEPALIVE: Duration = Duration::from_secs(14);
const UPDATE_EXPIRE: Duration = Duration::from_secs(66);

struct UpdateReaderWN {
  player: PlayerId,
  client: ClientId,
  to_send: UpdateId,
}

struct UpdateReader {
  wn: UpdateReaderWN,
  overflow: Option<io::Cursor<Box<[u8]>>>,
  need_flush: bool,
  gref: InstanceRef,
  keepalives: Wrapping<u32>,
  ending_send: Option<io::Cursor<Box<[u8]>>>,
  init_confirmation_send: iter::Once<()>,
}
deref_to_field!{UpdateReader, UpdateReaderWN, wn} // no DerefMut

#[derive(Error,Debug)]
#[error("WouldBlock error misreported!")]
struct FlushWouldBlockError{}

impl UpdateReaderWN {
  #[throws(io::Error)]
  fn write_next<U>(&mut self, mut buf: &mut U, tz: &Timezone,
                   next: &PreparedUpdate)
                   where U: Write {
    let tu = next.for_transmit(tz, self.player, self.client);

    write!(buf, "data: ")?;
    serde_json::to_writer(&mut buf, &tu)?;
    write!(buf, "\n\
                 id: {}\n\n",
           self.to_send)?;

    debug!("sending to {:?} {:?}: #{} {:?}",
           &self.player, &self.client, self.to_send, &tu);

    self.to_send.try_increment().unwrap();
  }

  fn trouble<T:Debug>(&self, m: &'static str, info: T) -> io::Error {
    error!("update sending error: {}: {} {}: {:?}",
           m, &self.player, &self.client, &info);
    io::Error::new(io::ErrorKind::Other, anyhow!("internal error"))
  }
}

impl Read for UpdateReader {
  fn read(&mut self, orig_buf: &mut [u8]) -> Result<usize, io::Error> {
    if let Some(ref mut ending) = self.ending_send {
      return ending.read(orig_buf);
    }

    let mut ig = self.gref.lock()
      .map_err(|e| self.trouble("game corrupted", &e))?;

    let orig_wanted = orig_buf.len();
    let mut buf = &mut *orig_buf;

    if self.init_confirmation_send.next().is_some() {
      write!(buf, "event: commsworking\n\
                   data: init {} {} G{}\n\n",
             self.player, self.client, ig.gs.gen)?;
    }

    let g = &mut *ig;
    let iplayer = &mut match g.iplayers.get_mut(self.player) {
      Some(x) => x,
      None => {
        let data = format!("event: player-gone\n\
                            data: No longer in the game\n\n")
          .into_bytes().into_boxed_slice();
        assert_eq!(self.ending_send, None);
        let ending = self.ending_send.get_or_insert(io::Cursor::new(data));
        return ending.read(orig_buf);
      },
    };

    let pu = &mut iplayer.u;

    loop {
      if let Some(ref mut overflow) = self.overflow {
        let got = overflow.read(&mut buf)
          .map_err(|e| self.wn.trouble("overflow failed", &e))?;
        debug!("read from overflow {} {} len={}",
               &self.player, &self.client, got);
        if got == 0 { self.overflow = None }
        buf = &mut buf[got..];
      }

      let next = match pu.read_log().get(self.to_send) {
        Some(next) => next,
        None => {
          if self.to_send < pu.read_log().front_index()
          && buf.len() == orig_wanted {
            write!(buf, "event: updates-expired\ndata: {}\n\n",
                   self.to_send)
              .map_err(|e| self.wn.trouble("notify updates expired", &e))?;
            debug!("updates expired for {} {}, telling client (#{})",
                   &self.wn.player, &self.wn.client, self.to_send);
            self.wn.to_send = UpdateId::max_value();
            // ^ just stops us spewing, hopefully client will notice
          }
          break
        }
      };
      let next_len = UPDATE_MAX_FRAMING_SIZE + next.json_len(self.player);
      if next_len > buf.len() {
        if buf.len() != orig_wanted { break }

        if self.overflow.is_some() {
          throw!(self.wn.trouble("overflow mismanaged",()));
        }
        self.overflow = {
          let mut overflow = Vec::with_capacity(next_len);
          self.wn.write_next(&mut overflow, &iplayer.ipl.tz, &next)
            .map_err(|e| self.wn.trouble("overflow.write_next",&e))?;
          debug!("overflow {} {}, len={}",
                 &self.wn.player, &self.wn.client, &overflow.len());
          Some(io::Cursor::new(overflow.into_boxed_slice()))
        };
        continue;
      }

      self.wn.write_next(&mut buf, &iplayer.ipl.tz, &next)
        .map_err(|e| self.wn.trouble("UpdateReader.write_next",&e))?;

      let before = next.when - UPDATE_EXPIRE;
      pu.expire_upto(before);
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

      cv.wait_for(&mut ig.c, UPDATE_KEEPALIVE);

      write!(buf, "event: commsworking\n\
                   data: online {} {} G{}\n\n",
             self.player, self.client, ig.gs.gen)?;
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
    Display::fmt(&self.0, f)
  }
}

impl Bounded for UpdateId {
  fn max_value() -> Self { UpdateId(Bounded::max_value()) }
  fn min_value() -> Self { UpdateId(Bounded::min_value()) }
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

#[throws(Fatal)]
pub fn content(iad: InstanceAccessDetails<ClientId>, gen: Generation)
  -> impl Read {
  let client = iad.ident;

  let content = {
    let mut g = iad.gref.lock()?;
    let _g = &mut g.gs;
    let cl = g.clients.byid(client)?;
    let player = cl.player;
    trace!("updates content iad={:?} player={:?} cl={:?}",
           &iad, &player, &cl);
    let gref = iad.gref.clone();

    let log = &g.iplayers.byid(player)?.u.read_log();

    let to_send = match log.into_iter().rev()
      .find(|(_,update)| update.gen <= gen) {
        None => UpdateId::min_value(),
        Some((mut i,_)) => { i.try_increment(); i },
      };

    UpdateReader {
      need_flush: false,
      keepalives: Wrapping(0),
      overflow: None,
      gref,
      ending_send: default(),
      init_confirmation_send: iter::once(()),
      wn: UpdateReaderWN {
        player, client, to_send,
      },
    }
  };
  let content = BufReader::with_capacity(UPDATE_READER_SIZE, content);
  //DebugReader(content)
  content
}
