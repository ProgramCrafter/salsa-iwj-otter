// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// todo: delete-account

use super::*;

//---------- list-games ----------

mod list_games {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    all: bool,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.all)
      .add_option(&["--all"],StoreTrue,
                  "user superuser access to list *all* games");
    ap
  }

  fn call(SCCA{ mut out, ma, args,.. }:SCCA) -> Result<(),AE> {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut conn = connect(&ma)?;
    let mut games = match conn.cmd(&MC::ListGames { all: Some(args.all) })? {
      MR::GamesList(g) => g,
      x => throw!(anyhow!("unexpected response to ListGames: {:?}", &x)),
    };
    games.sort();
    for g in games {
      writeln!(out, "{}", &g)?;
    }
    Ok(())
  }

  inventory_subcmd!{
    "list-games",
    "List games",
  }
}

//---------- list-accounts ----------

mod list_accounts {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    all: bool,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.all)
      .add_option(&["--all"],StoreTrue,
                  "user superuser access to list all accounts");
    ap
  }

  #[throws(AE)]
  fn call(SCCA{ mut out, ma, args,.. }:SCCA) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut conn = connect(&ma)?;
    let all = Some(args.all);
    let accounts = match conn.cmd(&MC::ListAccounts { all })? {
      MR::AccountsList(g) => g,
      x => throw!(anyhow!("unexpected response to ListAccounts: {:?}", &x)),
    };
    for a in accounts {
      writeln!(out, "{}", a)?;
    }
  }

  inventory_subcmd!{
    "list-accounts",
    "List accounts in your account scope",
  }
}
