// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use super::*;
use super::usebundles::*;

// todo: list-players

//---------- reset-game ----------

mod reset_game {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    table_file: Option<String>,
    game_spec: String,
    bundles: Vec<String>,
    bundles_only: bool,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.table_file).metavar("TABLE-SPEC[-TOML]")
      .add_option(&["--reset-table"],StoreOption,
                  "reset the players and access too");
    ap.refer(&mut sa.game_spec).required()
      .add_argument("GAME-SPEC",Store,
                    "game spec, as found in server, \
                     or local filename if it contains a '/')");
    ap.refer(&mut sa.bundles).required()
      .add_argument("BUNDLES",Collect,
                    "Bundle files to use.  If any are specified, \
                     all needed bundles must be specified, as any \
                     not mentioned will be cleared from the server.");
    let mut bundles_only = ap.refer(&mut sa.bundles_only);
    bundles_only.add_option(&["--bundles-only"],StoreTrue,
              "insist that server has only the specified BUNDLES \
               (clearing out the server if no BUNDLES were specified)");
    bundles_only.add_option(&["--bundles-at-least"],StoreFalse,
              "don't care if the server has additional bundles uploaded \
               earlier (default)");
    ap
  }

  fn call(SCCA{ ma, args,.. }:SCCA) -> Result<(),AE> {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let instance_name = ma.instance();
    let mut chan = ma.access_game()?;

    let reset_insn =
      if let Some(filename) = spec_arg_is_path(&args.game_spec) {
        let spec_toml = read_spec_from_path(
          filename, SpecRaw::<GameSpec>::new())?;
        MGI::ResetFromGameSpec { spec_toml }
      } else {
        MGI::ResetFromNamedSpec { spec: args.game_spec.clone() }
      };

    let mut insns = vec![];

    if let Some(table_file) = args.table_file {
      let table_spec = read_spec(&ma, &table_file, SpecParseToml::new())?;
      let game = chan.game.clone();
      chan.cmd(&MgmtCommand::CreateGame {
        game,
        insns: vec![],
      }).map(|_|()).or_else(|e| {
        if let Some(&MgmtError::AlreadyExists) = e.downcast_ref() {
          return Ok(())
        }
        Err(e)
      })?;

      insns.extend(setup_table(&ma, &instance_name, &table_spec)?);
    }

    if args.bundles_only || args.bundles.len() != 0 {
      let local = args.bundles.into_iter().map(|file| {
        BundleForUpload::prepare(file)
      }).collect::<Result<Vec<_>,_>>()?;

      let resp = chan.cmd(&MgmtCommand::ListBundles { game: ma.instance() })?;
      let remote = match resp {
        MR::Bundles { bundles } => bundles,
        x => throw!(anyhow!("unexpected response to ListBundles: {:?}",x)),
      };

      let bundles_only = args.bundles_only;
      match Itertools::zip_longest(
        local.iter().rev(),
        remote.iter().rev(),
      ).map(|eob| {
        use EitherOrBoth::*;
        use bundles::State::*;
        match eob {
          Right((id, remote)) => if bundles_only {
            Err(format!("server has additional bundle(s) eg {} {}",
                        id, remote))
          } else {
            Ok(())
          },
          Left(local) => {
            Err(format!("server is missing {} {} {}",
                        local.kind, local.hash, local.file))
          },
          Both(_local, (id, Uploading)) => {
            Err(format!("server has incomplete upload :{}", id))
          },
          Both(local, (id, Loaded(remote))) => {
            if (local.size,  local.hash) !=
               (remote.size, remote.hash) {
               Err(format!("server's {} does not match {}", id, &local.file))
            } else {
               Ok(())
            }
          }
        }
      }).find_map(Result::err).map_or_else(|| Ok(()), Err) {
        Ok(()) => {
          if ma.verbose >= 0 {
            eprintln!("Reusing server's existing bundles");
          }
        },
        Err(why) => {
          if ma.verbose >= 0 {
            eprintln!("Re-uploading bundles: {}", why);
          }
          if bundles_only {
            clear_game(&ma, &mut chan)?;
          }
          let progress = ma.progressbar()?;
          let mut progress = termprogress::Nest::new(local.len(), progress);
          for bundle in local {
            bundle.upload(&ma, &mut chan, &mut progress)?;
          }
        },
      }
    }

    insns.push(reset_insn);

    chan.alter_game(insns, None)?;

    if ma.verbose >= 0 {
      eprintln!("reset successful.");
    }
    Ok(())
  }

  inventory_subcmd!{
    "reset",
    "Reset the state of the game table",
  }
}

//---------- set-link ----------

mod set_link {

  use super::*;

  #[derive(Debug,Default)]
  struct Args {
    kind: Option<LinkKind>,
    url: Option<String>,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.kind)
      .add_argument("LINK-KIND",StoreOption,"link kind");
    ap.refer(&mut sa.url)
      .add_argument("URL",StoreOption,"url (or empty for none)");
    ap
  }

  #[throws(AE)]
  fn call(SCCA{ mut out, ma, args,.. }:SCCA) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = ma.access_game()?;

    match args.url {
      None => {
        let MgmtGameResponseGameInfo { links, .. } = chan.info()?;
        for (tk, v) in links {
          let v: Url = (&v).try_into().context("reparse sererr's UrlSpec")?;
          match args.kind {
            None => {
              writeln!(out, "{:<10} {}", tk, &v)?;
            }
            Some(wk) => {
              if wk == tk {
                writeln!(out, "{}", &v)?;
              }
            }
          }
        }
      },

      Some(url) => {
        let kind = args.kind.unwrap();
        chan.alter_game(vec![
          if url == "" {
            MGI::RemoveLink { kind }
          } else {
            MGI::SetLink { kind, url: UrlSpec(url) }
          }
        ], None)?;
      },
    }
  }

  inventory_subcmd!{
    "set-link",
    "Set one of the info links visible from within the game",
  }
}

//---------- join-game ----------

mod join_game {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    reset_access: bool,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.reset_access)
      .add_option(&["--reset"],StoreTrue,
                  "generate and deliver new player access token");
    ap
  }

  fn call(SCCA{ mut out, ma, args,.. }:SCCA) -> Result<(),AE> {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = ma.access_game()?;

    let mut insns = vec![];
    match chan.has_player(&ma.account)? {
      None => {
        let nick = ma.nick.clone()
          .unwrap_or_else(|| ma.account.default_nick());
        let details = MgmtPlayerDetails { nick: Some(nick) };
        insns.push(MGI::JoinGame { details });
      }
      Some((player, mpi)) => {
        writeln!(out, "already in game, as player #{} {:?}",
                 player.0.get_idx_version().0, &mpi.nick)?;
        let MgmtPlayerInfo { nick, account:_ } = mpi;
        if let Some(new_nick) = &ma.nick {
          if &nick != new_nick {
            writeln!(out, "changing nick to {:?}", &new_nick)?;
            let details = MgmtPlayerDetails { nick: ma.nick.clone() };
            insns.push(MGI::UpdatePlayer { player, details });
          }
        }
        if args.reset_access {
          writeln!(out, "resetting access token (invalidating other URLs)")?;
          insns.push(MGI::ResetPlayerAccess(player));
        } else {
          writeln!(out, "redelivering existing access token")?;
          insns.push(MGI::RedeliverPlayerAccess(player));
        }
      }
    };

    fn deliver(out: &mut CookedStdout, token: &AccessTokenReport) {
      for l in &token.lines {
        if l.contains(char::is_control) {
          writeln!(out, "Server token info contains control chars! {:?}", &l)
        } else {
          writeln!(out, " {}", &l)
        }.unwrap()
      }
    }

    for resp in chan.alter_game(insns, None)? {
      match resp {
        MGR::JoinGame { nick, player, token } => {
          writeln!(out, "joined game as player #{} {:?}",
                   player.0.get_idx_version().0,
                   &nick)?;
          deliver(&mut out, &token);
        }
        MGR::PlayerAccessToken(token) => {
          deliver(&mut out, &token);
        }
        MGR::Fine => {}
        _ => throw!(anyhow!("unexpected response to instruction(s)")),
      }
    }

    Ok(())
  }

  inventory_subcmd!{
    "join-game",
    "Join a game or reset access token (creating or updating account)",
  }
}

//---------- leave-game ----------

mod leave_game {
  use super::*;

  type Args = NoArgs;

  fn call(SCCA{ mut out, ma, args,.. }:SCCA) -> Result<(),AE> {
    let _args = parse_args::<Args,_>(args, &noargs, &ok_id, None);
    let mut chan = ma.access_game()?;

    let player = match chan.has_player(&ma.account)? {
      None => {
        writeln!(out, "this account is not a player in that game")?;
        exit(EXIT_NOTFOUND);
      }
      Some((player, _)) => player,
    };

    chan.alter_game(vec![MGI::LeaveGame(player)], None)?;

    Ok(())
  }

  inventory_subcmd!{
    "leave-game",
    "Leave a game",
  }
}

//---------- clear game ----------

#[throws(AE)]
fn clear_game(ma: &MainOpts, chan: &mut MgmtChannelForGame) {
  chan.alter_game(vec![MGI::ClearGame{ }], None)
    .context("clear table")?;
  chan.cmd(&MC::ClearBundles { game: ma.instance() })
    .context("clear bundles")?;
}

mod clear_game {
  use super::*;

  type Args = NoArgs;

  #[throws(AE)]
  fn call(SCCA{ ma, args,.. }:SCCA) {
    let _args = parse_args::<Args,_>(args, &noargs, &ok_id, None);
    let mut chan = ma.access_game()?;
    clear_game(&ma, &mut chan)?;
  }

  inventory_subcmd!{
    "clear-game",
    "clear the table and clear out all bundles",
  }
}

//---------- delete-game ----------

mod delete_game {
  use super::*;

  type Args = NoArgs;

  fn call(SCCA{ ma, args,.. }:SCCA) -> Result<(),AE> {
    let _args = parse_args::<Args,_>(args, &noargs, &ok_id, None);
    let mut chan = ma.access_game()?;
    let game = chan.game.clone();
    chan.cmd(&MC::DestroyGame { game })?;
    Ok(())
  }

  inventory_subcmd!{
    "delete-game",
    "Delete a game (throwing all the players out of it)",
  }
}
