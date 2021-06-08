// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use super::*;

//---------- mgmtchannel-proxy ----------

mod mgmtchannel_proxy {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    restrict: Option<sshkeys::KeySpec>,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.restrict).metavar("ID:NONCE")
      .add_option(&["--restrict-ssh"],StoreOption,
                  "restrict to access available to registred OpenSSH key \
                   (used in runes generated by server for authorized_keys)");
    ap
  }

  #[throws(AE)]
  fn call(SCCA{ out, ma, args,.. }:SCCA) {
    set_program_name("otter (remote)".into());

    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = connect_chan(&ma)?;

    if let Some(ref restrict) = args.restrict {
      chan.cmd(&MC::SetRestrictedSshScope { key: restrict.clone() })
        .context("specify authorisation")?;
    }

    let MgmtChannel { read, write } = chan;
    let mut read = read.into_stream()?;
    let mut write = write.into_stream()?;

    drop(out); // do our own stdout

    let tcmds = thread::spawn(move || {
      io_copy_interactive(&mut BufReader::new(io::stdin()), &mut write)
        .map_err(|e| match e {
          Left(re)  => AE::from(re).context("read cmds from stdin"),
          Right(we) => AE::from(we).context("forward cmds to servvr"),
        })
        .unwrap_or_else(|e| e.end_process(8));
      exit(0);
    });
    let tresps = thread::spawn(move || {
      io_copy_interactive(&mut read, &mut RawStdout::new())
        .map_err(|e| match e {
          Left(re)  => AE::from(re).context("read resps from server"),
          Right(we) => AE::from(we).context("forward cmds to stdout"),
        })
        .context("copy responses")
        .unwrap_or_else(|e| e.end_process(8));
      exit(0);
    });
    tcmds.join().expect("collect commands copy"); 
    tresps.join().expect("collect responses copy");
  }

  inventory_subcmd!{
    SSH_PROXY_SUBCMD,
    "connect to management channel and copy raw message data back and forth",
    suppress_selectaccount: true,
  }
}

//---------- set-ssh-keys ----------

mod set_ssh_keys {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    add: bool,
    allow_non_ssh: bool,
    remove_current: bool,
    keys: String,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.add)
      .add_option(&["--add"],StoreTrue,
                  "add keys, only (ie, leave all existing keys)");
    ap.refer(&mut sa.allow_non_ssh)
      .add_option(&["--allow-non-ssh-account"],StoreTrue,
                  "allow settings ssh key access for a non-ssh: account");
    ap.refer(&mut sa.remove_current)
      .add_option(&["--allow-remove-current"],StoreTrue,
                  "allow removing the key currently being used for access");
    ap.refer(&mut sa.keys).required()
      .add_argument("KEYS-FILE", Store,
                  "file of keys, in authorized_keys formaat \
                   (`-` means stdin)");
    ap
  }

  #[throws(AE)]
  fn call(SCCA{ ma, args,.. }:SCCA) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut conn = connect(&ma)?;

    if ! ma.account.subaccount.is_empty() {
      throw!(ME::NoSshKeysForSubaccount);
    }
    let is_ssh_account = matches!(ma.account.scope, AS::Ssh{..});
    if ! (args.allow_non_ssh || is_ssh_account) {
      throw!(anyhow!("not setting ssh keys for non-ssh: account; \
                      use --allow-non-ssh-account to override"));
    }

    conn.prep_access_account(&ma, false)?;

    use sshkeys::*;

    #[derive(Debug)]
    struct Currently {
      index: usize,
      mkr: MgmtKeyReport,
      thisconn_retain: bool,
    }

    #[derive(Debug,Default)]
    struct St {
      wanted: Option<(usize, AuthkeysLine)>,
      currently: Vec<Currently>,
    }
    type PubDataString = String;
    let mut states: HashMap<PubDataString, St> = default();

    // read wanted set

    let akf: Box<dyn Read> = if args.keys == "-" { Box::new(io::stdin()) }
    else {
      Box::new(File::open(&args.keys)
        .with_context(|| args.keys.clone())
        .context("KEYS-FILE")?)
    };
    let akf = BufReader::new(akf);

    for (l, lno) in akf.lines().zip(1..) {
      let l = l.context("read KEYS-FILE")?;
      let l = l.trim();
      if l.starts_with("#") || l == "" { continue }
      let l = AuthkeysLine(l.to_owned());
      let (pubdata, _comment) = l.parse()
        .with_context(|| format!("parse KEYS-FILE line {}", lno))?;
      let st = states.entry(pubdata.to_string()).or_insert_with(default);
      if let Some((lno0,_)) = st.wanted { throw!(
        anyhow!("KEYS-FILE has duplicate key, lines {} {}", lno0, lno)
      )}
      st.wanted = Some((lno, l));
    }

    // find the one we're using now

    let using = if args.remove_current { None } else {
      use MgmtResponse::ThisConnAuthBy as TCAB;
      use MgmtThisConnAuthBy as MTCAB;
      match conn.cmd(&MC::ThisConnAuthBy).context("find current auth")? {
        TCAB(MTCAB::Ssh { key }) => Some(key),
        TCAB(MTCAB::Local) => None,
        #[allow(unreachable_patterns)] TCAB(q) => throw!(anyhow!(
          "unexpected ThisConnAuthBy {:?}, \
           cannot check if we are removing the current authentication, \
           (and --allow-remove-current not specified)", q)),
        _ => throw!(anyhow!("unexpected response to ThisConnAuthBy")),
      }
    };

    // obtain current set

    for (index, mkr) in match conn.cmd(&MC::SshListKeys)
      .context("list existing keys")?
    {
      MR::SshKeys(report) => report,
      _ => throw!(anyhow!("unexpected response to SshListKeys")),
    }
      .into_iter().enumerate()
    {
      let pubdata_s = mkr.data.to_string();
      let st = states.entry(pubdata_s).or_insert_with(default);
      let thisconn_retain = Some(&mkr.key) == using.as_ref();
      st.currently.push(Currently { index, mkr, thisconn_retain });
    }

    // check we don't want to bail

    for st in states.values() {
      if st.wanted.is_none() {
        for c in &st.currently {
          if c.thisconn_retain {
            throw!(anyhow!(
              "refusing to remove currently-being-used key #{} {}",
              c.index, &c.mkr));
          }
        }
      }
    }

    // delete any with problems
    // doing this even for wanted keys prevents constant buildup
    // of problem keys

    for st in states.values() {
      for c in &st.currently {
        if c.mkr.problem.is_some() {
          conn.cmd(&MC::SshDeleteKey { index: c.index, id: c.mkr.key.id })
            .with_context(|| format!("delete broken key #{}", c.index))?;
        }
      }
    }

    // add new keys

    for st in states.values() {
      if_let!{ Some((lno, akl)) = &st.wanted; else continue };
      if st.currently.iter().any(|c| c.mkr.problem.is_none()) { continue }
      conn.cmd(&MC::SshAddKey { akl: akl.clone() })
        .with_context(|| format!("add key from KEYS-FILE line {}", lno))?;
    }

    // delete old keys

    for st in states.values() {
      if st.wanted.is_some() { continue }
      for c in &st.currently {
        if c.mkr.problem.is_some() { continue /* we deleted it already */ }
        conn.cmd(&MC::SshDeleteKey { index: c.index, id: c.mkr.key.id })
          .with_context(|| format!("delete old key #{}", c.index))?;
      }
    }
  }

  inventory_subcmd!{
    "set-ssh-keys",
    "set SSH keys for remote management access authentication",
  }
}

//---------- list-ssh-keys ----------

mod list_ssh_keys {
  use super::*;

  type Args = NoArgs;

  #[throws(AE)]
  fn call(SCCA{ mut out, ma, args,.. }:SCCA) {
    let _args = parse_args::<Args,_>(args, &noargs, &ok_id, None);
    let mut conn = connect(&ma)?;
    conn.prep_access_account(&ma, false)?;

    use sshkeys::*;

    // find the one we're using now

    let using = {
      use MgmtResponse::ThisConnAuthBy as TCAB;
      use MgmtThisConnAuthBy as MTCAB;
      match conn.cmd(&MC::ThisConnAuthBy).context("find current auth")? {
        TCAB(MTCAB::Ssh { key }) => Some(key),
        TCAB(_) => None,
        _ => throw!(anyhow!("unexpected response to ThisConnAuthBy")),
      }
    };

    // obtain current set

    for (_index, mkr) in match conn.cmd(&MC::SshListKeys)
      .context("list existing keys")?
    {
      MR::SshKeys(report) => report,
      _ => throw!(anyhow!("unexpected response to SshListKeys")),
    }
      .into_iter().enumerate()
    {
      let s = mkr.to_string();
      use unicode_width::UnicodeWidthChar;
      if s.chars().any(|c| c.width() == None /* control char */) {
        write!(&mut out, "# FUNKY! # {:?}", &s)?;
      } else {
        write!(&mut out, "{}", &s)?;
      }

      if Some(&mkr.key) == using.as_ref() {
        write!(&mut out, "# <- this connection!")?;
      }
      writeln!(&mut out, "")?;
    }

    out.flush()?;
  }

  inventory_subcmd!{
    "list-ssh-keys",
    "set SSH keys for remote management access authentication",
  }
}