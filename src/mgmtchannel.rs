// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;
use crate::commands::*;

#[derive(Debug,Error)]
pub enum MgmtChannelReadError {
  #[error("unexpected EOF")]         EOF,
  #[error("parse MessagePack: {0}")] Parse(String),
  #[error("{0}")]                    IO(#[from] io::Error),
}

#[derive(Debug,Error)]
pub enum MgmtChannelWriteError {
  Serialize(rmp_serde::encode::Error), // but not ValueWriteError so no from
  IO(#[from] io::Error),
}
display_as_debug!{MgmtChannelWriteError}

impl From<rmp_serde::encode::Error> for MgmtChannelWriteError {
  fn from(re: rmp_serde::encode::Error) -> MgmtChannelWriteError {
    use rmp_serde::encode::Error::*;
    use MgmtChannelWriteError as MCWE;
    use rmp::encode::ValueWriteError as RVWE;
    match re {
      InvalidValueWrite(RVWE::InvalidMarkerWrite(ioe)) => MCWE::IO(ioe),
      InvalidValueWrite(RVWE::InvalidDataWrite  (ioe)) => MCWE::IO(ioe),
      ser@ (UnknownLength | InvalidDataModel(_) |
            DepthLimitExceeded | Syntax(_)) => MCWE::Serialize(ser),
    }
  }
}

pub struct MgmtChannel<R:Read, W:Write> {
  pub read:  FrameReader<R>,
  pub write: FrameWriter<W>,
}

impl<R,W> Debug for MgmtChannel<R,W> where R: Read, W: Write {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    f.write_str("MgmtChannel{...}")?
  }
}

pub type ClientMgmtChannel = MgmtChannel<
    Box<dyn ReadDebug  + Send + 'static>,
    Box<dyn WriteDebug + Send + 'static>,
  >;

pub trait ReadDebug: Read + Debug { }
pub trait WriteDebug: Write + Debug { }
impl<T> ReadDebug for T where T: Read + Debug { }
impl<T> WriteDebug for T where T: Write + Debug { }

impl ClientMgmtChannel {
  #[throws(AE)]
  pub fn connect(socket_path: &str) -> Self {
    let unix = UnixStream::connect(socket_path)
      .with_context(||socket_path.to_owned())
      .context("connect to server")?; 
    let read = unix.try_clone().context("dup the client connection")?;
    let write = unix;
    MgmtChannel::new_boxed(read, write)
  }

  pub fn new_boxed<R,W>(read: R, write: W) -> Self
  where R: ReadDebug  + Send + 'static,
        W: WriteDebug + Send + 'static {
    MgmtChannel::new_raw(Box::new(read), Box::new(write))
  }
}

impl MgmtChannel<TimedFdReader,TimedFdWriter> {
  #[throws(AE)]
  pub fn new_timed<U>(conn: U) -> Self
  where U: IoTryClone + Read + Write + IntoRawFd + Send + 'static,
  {
    let read = conn.try_clone().context("dup the command stream")?;
    let read = TimedFdReader::new(read).context("set up timed reader")?;
    let write = TimedFdWriter::new(conn).context("set up timed writerr")?;
    MgmtChannel::new_raw(read, write)
  }
}

impl<R,W> MgmtChannel<R,W> where R: Read, W: Write + Send {
  pub fn new_raw(read: R, write: W) -> Self  {
    let read = FrameReader::new(read);
    let write = FrameWriter::new(write);
    MgmtChannel { read, write }
  }

  pub fn read_inner_mut(&mut self) -> &mut R {
    self.read.inner_mut()
  }
}

impl ClientMgmtChannel {
  pub const PROGRESS: ProgressUpdateMode = PUM::Duplex;

  #[throws(AE)]
  pub fn cmd_withbulk<U,D>(&mut self, cmd: &MgmtCommand,
                           up: &mut U, down: &mut D,
                           progress: &mut dyn termprogress::Reporter)
                           -> MgmtResponse
  where U: Read + Send, D: Write,
  {
    use MgmtResponse::*;
    let mut wbulk = self.write
      .write_withbulk().context("start sending command")?
      .respond(&cmd).context("send command")?;
    let read = &mut self.read;

    let (resp, mut rbulk) = crossbeam_utils::thread::scope(|scope| {
      let thr = scope.spawn(move |_| {
        io::copy(up, &mut wbulk).context("copy")?;
        wbulk.finish().context("finish")?;
        Ok::<_,AE>(())
      });

      let (mut resp, mut rbulk) =
        read.read_withbulk()
        .context("failed to read response")?;

      while let MR::Progress(pi) = resp {
        resp = (&mut rbulk).read_rmp()?;
        progress.report(&pi);
      }

      let r = thr.join().expect("bulk data upload thread paniced");
      if let Err(e) = r {
        progress.clear();
        warn!("bulk data upload failed: {}", e);
      }
      Ok::<_,AE>((resp, rbulk))
    })
      .expect("bulk data upload thread panicked, not reaped")
      ?;

    progress.clear();
    match &resp {
      Progress(_) => panic!(),
      Fine | AccountsList{..} | GamesList{..} |
      Libraries(_) | LibraryItems(_) | Bundles{..} | Bundle{..} => { },
      SshKeys(..) | SshKeyAdded{..} | ThisConnAuthBy{..} => { },
      AlterGame { error: None, .. } => { },
      Error { error } => {
        Err(error.clone()).context(
          format!("got error response to: {:?}",&cmd)
        )?;
      },
      AlterGame { error: Some(error), ref responses } => {
        if let MgmtCommand::AlterGame { insns, .. } = &cmd {
          if responses.len() < insns.len() {
            Err(error.clone())
              .with_context(|| format!("{:?}", &insns[responses.len()]))
              .context("AlterGame insn failed")?;
          }
        }
        Err(error.clone()).context(format!(
          "game alterations failed (maybe partially); response to: {:?}",
          &cmd
        ))?;
      }
    };

    io::copy(&mut rbulk, down).context("copy bulk download")?;
    resp
  }

  #[throws(AE)]
  pub fn cmd(&mut self, cmd: &MgmtCommand) -> MgmtResponse {
    self.cmd_withbulk(cmd, &mut io::empty(), &mut io::sink(),
                      &mut termprogress::Null)?
  }

  pub fn for_game(self, game: InstanceName, how: MgmtGameUpdateMode)
                  -> MgmtChannelForGame {
    MgmtChannelForGame {
      chan: self,
      game, how
    }
  }
}

pub trait IoTryClone: Sized {
  fn try_clone(&self) -> io::Result<Self>;
}

impl IoTryClone for UnixStream {
  fn try_clone(&self) -> io::Result<UnixStream> { self.try_clone() }
}


#[derive(Debug)]
pub struct MgmtChannelForGame {
  pub chan: ClientMgmtChannel,
  pub game: InstanceName,
  pub how: MgmtGameUpdateMode,
}
deref_to_field_mut!{MgmtChannelForGame, ClientMgmtChannel, chan}

impl MgmtChannelForGame {
  #[throws(AE)]
  pub fn alter_game(&mut self, insns: Vec<MgmtGameInstruction>,
                f: Option<&mut dyn FnMut(&MgmtGameResponse) -> Result<(),AE>>)
                -> Vec<MgmtGameResponse> {
    let insns_len = insns.len();
    let cmd = MgmtCommand::AlterGame {
      game: self.game.clone(), how: self.how,
      insns
    };
    let responses = match self.cmd(&cmd)? {
      MgmtResponse::AlterGame { error: None, responses }
      if responses.len() == insns_len ||
         responses.iter().any(|r| matches!(r, MGR::InsnExpanded)) => {
        responses
      },
      wat => Err(anyhow!("unexpected AlterGame response: {:?} => {:?}",
                         &cmd, &wat))?,
    };
    if let Some(f) = f {
      for response in &responses {
        f(response)?;
      }
    }
    responses
  }

  #[throws(AE)]
  pub fn info(&mut self) -> MgmtGameResponseGameInfo {
    let resp = self.alter_game(vec![MGI::Info], None)?;
    match &resp[..] {
      [MGR::Info(info)] => info.clone(),
      x => throw!(anyhow!("unexpected response to game Info: {:?}", &x)),
    }
  }

  #[throws(AE)]
  pub fn has_player(&mut self, account: &AccountName)
                    -> Option<(PlayerId, MgmtPlayerInfo)>
  {
    let players = {
      let MgmtGameResponseGameInfo { players, .. } = self.info()?;
      players
    };

    players.into_iter().filter(
      |(_,mpi)| &mpi.account == account
    ).next()
  }

  #[throws(AE)]
  pub fn list_pieces(&mut self) -> (Vec<MgmtGamePieceInfo>, BTreeSet<String>) {
    let insns = vec![ MGI::ListPieces ];
    let mut responses = self.alter_game(insns, None)?;
    match responses.as_mut_slice() {
      [MGR::Pieces { pieces, pcaliases }] => return (
        mem::take(pieces),
        mem::take(pcaliases),
      ),
      wat => Err(anyhow!("ListPieces => {:?}", &wat))?,
    }
  }

  #[throws(AE)]
  pub fn list_items(&mut self, lib: Option<String>, pat: String)
                -> Vec<ItemEnquiryData> {
    let cmd = MgmtCommand::LibraryListByGlob {
      game: self.game.clone(),
      lib, pat,
    };
    let mut items = match self.cmd(&cmd)? {
      MgmtResponse::LibraryItems(items) => items,
      wat => Err(anyhow!("unexpected LibraryListByGlob response: {:?}",
                         &wat))?,
    };
    items.sort();
    items
  }

/*
  fn get_info(&mut self) -> Result<
      (MgmtGameResponseGameInfo, HashMap<String,PlayerId>
      ),AE>
  {
    let mut players = self.alter_game(
      vec![ MgmtGameInstruction::Info ],
      None,
    )?;
    let info = match players.pop() {
      Some(MgmtGameResponse::Info(info)) => info,
      wat => Err(anyhow!("GetGstate got {:?}", &wat))?,
    };
    let mut nick2id = HashMap::new();
    for (player, pstate) in info.players.iter() {
      use hash_map::Entry::*;
      match nick2id.entry(pstate.nick.clone()) {
        Occupied(oe) => Err(anyhow!("game has duplicate nick {:?}, {} {}",
                                    &pstate.nick, *oe.get(), player))?,
        Vacant(ve) => ve.insert(player),
      };
    }
    Ok((info, nick2id))
  }
*/
}
