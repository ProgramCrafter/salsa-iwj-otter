// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(clippy::while_let_loop)]
#![allow(clippy::blocks_in_if_conditions)]

use otter::prelude::*;

// ---------- basic definitions ----------

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

#[derive(Debug)]
struct BufForRead<'b> {
  csr: io::Cursor<&'b mut [u8]>,
}
impl Write for BufForRead<'_> {
  #[throws(io::Error)]
  fn write(&mut self, d: &[u8]) -> usize { self.csr.write(d)? }
  #[throws(io::Error)]
  fn flush(&mut self) { self.csr.flush()? }
}
impl BufForRead<'_> {
  fn reset_to_start(&mut self) { self.csr.set_position(0) }
  fn generated(&self) -> usize { self.csr.position().try_into().unwrap() }
  fn at_start(&self) -> bool { self.generated() == 0 }
  fn remaining(&self) -> usize {
    self.csr.get_ref().len() -
    usize::try_from(self.csr.position()).unwrap()
  }

  fn copy_from<R: InfallibleBufRead>(&mut self, mut read: R) {
    let rbuf = read.fill_buf().unwrap();
    let did = self.csr.write(rbuf).unwrap();
    read.consume(did);
  }

  #[throws(io::Error)]
  fn just_copy_from<R: InfallibleBufRead>(&mut self, read: R) -> usize {
    self.copy_from(read);
    self.generated()
  }
}
trait InfallibleBufRead: BufRead { }
impl<T> InfallibleBufRead for io::Cursor<T> where io::Cursor<T>: BufRead { }
impl<T> InfallibleBufRead for &mut T where T: InfallibleBufRead { }

impl Read for UpdateReader {
  fn read(&mut self, buf: &mut [u8]) -> Result<usize, io::Error> {
    let mut buf = BufForRead{ csr: io::Cursor::new(buf) };
    if buf.remaining() == 0 { return Ok(0) }

    if let Some(ref mut ending) = self.ending_send {
      return buf.just_copy_from(ending);
    }

    let mut ig = self.gref.lock()
      .map_err(|e| self.trouble("game corrupted", &e))?;

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
        buf.reset_to_start();
        return buf.just_copy_from(ending);
      },
    };

    let pu = &mut iplayer.u;

    loop {
      if let Some(ref mut overflow) = self.overflow {
        buf.copy_from(&mut *overflow);
        if usize::try_from(overflow.position()).unwrap()
        == overflow.get_ref().len() {
          self.overflow = None
        }
        debug!("read from overflow {} {}",
               &self.player, &self.client);
      }

      let next = match pu.read_log().get(self.to_send) {
        Some(next) => next,
        None => {
          if self.to_send < pu.read_log().front_index()
          && buf.at_start() {
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
      if next_len > buf.remaining() {
        if ! buf.at_start() { break }

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

    let generated = buf.generated();
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
    self.need_flush = true;
    return Ok(buf.generated());
  }
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
