// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(clippy::while_let_loop)]
#![allow(clippy::blocks_in_if_conditions)]

use otter::prelude::*;

use super::*;

// ---------- basic definitions ----------

const UPDATE_READER_SIZE: usize = 1024*32;
const UPDATE_KEEPALIVE: Duration = Duration::from_secs(14);
const UPDATE_EXPIRE: Duration = Duration::from_secs(66);

struct UpdateReaderWN {
  player: PlayerId,
  client: ClientId,
  to_send: UpdateId,
}

#[derive(Deref)] // no DerefMut
struct UpdateReader {
  #[deref] wn: UpdateReaderWN,
  overflow: Option<io::Cursor<Box<[u8]>>>,
  gref: InstanceRef,
  keepalives: Wrapping<u32>,
  ending_send: Option<io::Cursor<Box<[u8]>>>,
  init_confirmation_send: iter::Once<()>,
}

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

type BufForSend = Vec<u8>;

#[derive(Debug, Default)]
struct BufForRead {
  buf: Vec<u8>,
}
impl Write for BufForRead {
  #[throws(io::Error)]
  fn write(&mut self, d: &[u8]) -> usize { self.buf.write(d)? }
  #[throws(io::Error)]
  fn flush(&mut self) { self.buf.flush()? }
}
impl BufForRead {
  fn reset_to_start(&mut self) { self.buf.truncate(0) }
  fn at_start(&self) -> bool { self.buf.len() == 0 }
  fn len(&self) -> usize { self.buf.len() }

  fn copy_from<R: InfallibleBufRead>(&mut self, mut read: R) {
    let rbuf = read.fill_buf().unwrap();
    let did = self.write(rbuf).unwrap();
    read.consume(did);
  }

  fn just_copy_from<R: InfallibleBufRead>(mut self, read: R) -> BufForSend {
    self.copy_from(read);
    self.finish()
  }

  fn finish(self) -> BufForSend { assert!(! self.buf.is_empty()); self.buf }
  fn finish_eof() -> BufForSend { vec![] }
}
trait InfallibleBufRead: BufRead { }
impl<T> InfallibleBufRead for io::Cursor<T> where io::Cursor<T>: BufRead { }
impl<T> InfallibleBufRead for &mut T where T: InfallibleBufRead { }

#[derive(Error,Debug)]
pub enum SSEUpdateGenerationError {
  ImpossibleIoWriteError(#[from] io::Error), // write of Vec<u8> failed
  GameBeingDestroyed(#[from] GameBeingDestroyed),
}
display_as_debug!{SSEUpdateGenerationError}

impl UpdateReader {
  #[throws(SSEUpdateGenerationError)]
  async fn read(&mut self) -> BufForSend {
    let mut buf = BufForRead::default();

    if let Some(ref mut ending) = self.ending_send {
      return buf.just_copy_from(ending);
    }

    let mut ig = self.gref.lock()?;

    if self.init_confirmation_send.next().is_some() {
      write!(buf, "event: commsworking\n\
                   data: init {} {} G{}\n\n",
             self.player, self.client, ig.gs.gen)?;
    }

    let g = &mut *ig;
    let iplayer = &mut match g.iplayers.get_mut(self.player) {
      Some(x) => x,
      None => {
        // Ideally this would be handled by us throwing, and
        // the content unfold handling the error.
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

      self.wn.write_next(&mut buf, &iplayer.ipl.tz, next)
        .map_err(|e| self.wn.trouble("UpdateReader.write_next",&e))?;

      if buf.len() >= UPDATE_READER_SIZE { return buf.finish() }

      let before = next.when - UPDATE_EXPIRE;
      pu.expire_upto(before);
    }

    let cv = pu.get_cv();

    if buf.len() > 0 {
      return buf.finish();
    }

    if (||{
      (*ig).gs.players.get(self.player)?;
      let client = ig.clients.get_mut(self.client)?;
      client.lastseen = Instant::now();
      Some(())
    })() == None { return BufForRead::finish_eof() }

    let was_gen = ig.gs.gen;

    match tokio::time::timeout(
      UPDATE_KEEPALIVE,
      cv.wait_no_relock(ig.c)
    ).await {
      Err(_elapsed) => { },
      Ok(baton) => baton.dispose(),
    };

    write!(buf, "event: commsworking\n\
                 data: online {} {} G{}\n\n",
           self.player, self.client, was_gen)?;
    self.keepalives += Wrapping(1);
    return buf.finish();
  }
}

// ---------- entrypoint for dribbling the http response ----------

#[throws(Fatal)]
pub fn content(iad: InstanceAccessDetails<ClientId>, gen: Generation)
 -> Pin<Box<dyn futures::Stream<Item=Result<Bytes, SSEUpdateGenerationError>>>>
{
  let client = iad.ident;

  let update_reader = {
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

  Box::pin(futures::stream::try_unfold(
    update_reader, |mut update_reader| async
  {
    let got = update_reader.read().await?;
    Ok(if got.len() > 0 {
      Some((Bytes::from(got), update_reader))
    } else {
      None
    })
  })) as _
}
