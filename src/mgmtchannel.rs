// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;
use crate::commands::*;

#[derive(Debug,Error)]
pub enum MgmtChannelReadError {
  EOF,
  Parse(String),
  IO(#[from] io::Error),
}
display_as_debug!{MgmtChannelReadError}

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

pub struct MgmtChannel {
  pub read:  FrameReader<BufReader<Box<dyn Read >>>,
  pub write: FrameWriter<BufWriter<Box<dyn Write>>>,
}

impl Debug for MgmtChannel{ 
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    f.write_str("MgmtChannel{...}")?
  }
}

impl MgmtChannel {
  #[throws(AE)]
  pub fn connect(socket_path: &str) -> MgmtChannel {
    let unix = UnixStream::connect(socket_path)
      .with_context(||socket_path.to_owned())
      .context("connect to server")?; 
    let chan = MgmtChannel::new(unix)?;
    chan
  }

  #[throws(AE)]
  pub fn new<U: IoTryClone + Read + Write + 'static>(conn: U) -> MgmtChannel {
    let read = conn.try_clone().context("dup the command stream")?;
    let read = Box::new(read) as Box<dyn Read>;
    let read = BufReader::new(read);
    let read = FrameReader::new(read);
    let write = Box::new(conn) as Box<dyn Write>;
    let write = BufWriter::new(write);
    let write = FrameWriter::new(write);
    MgmtChannel { read, write }
  }

  #[throws(AE)]
  pub fn cmd(&mut self, cmd: &MgmtCommand) -> MgmtResponse {
    use MgmtResponse::*;
    self.write.write(&cmd).context("send command")?;
    let resp = self.read.read().context("read response")?;
    match &resp {
      Fine | AccountsList{..} | GamesList{..} | LibraryItems(_) => { },
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
              .context("AlterGame insn failed")
              .with_context(|| format!(" {:?}", &insns[responses.len()]))?;
          }
        }
        Err(error.clone()).context(format!(
          "game alterations failed (maybe partially); response to: {:?}",
          &cmd
        ))?;
      }
    };
    resp
  }

  #[throws(AE)]
  pub fn list_items(&mut self, pat: &shapelib::ItemSpec)
                -> Vec<shapelib::ItemEnquiryData> {
    let cmd = MgmtCommand::LibraryListByGlob { glob: pat.clone() };
    let mut items = match self.cmd(&cmd)? {
      MgmtResponse::LibraryItems(items) => items,
      wat => Err(anyhow!("unexpected LibraryListByGlob response: {:?}",
                         &wat))?,
    };
    items.sort();
    items
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
  pub chan: MgmtChannel,
  pub game: InstanceName,
  pub how: MgmtGameUpdateMode,
}
deref_to_field_mut!{MgmtChannelForGame, MgmtChannel, chan}

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
