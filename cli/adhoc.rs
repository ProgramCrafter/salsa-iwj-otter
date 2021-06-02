// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use super::*;

//---------- adhoc json/ron etc. ----------

#[derive(Debug,Copy,Clone)]
struct AdhocFormat(&'static str);

impl From<&'static Subcommand> for AdhocFormat {
  fn from(sc: &'static Subcommand) -> Self { AdhocFormat(
    sc.verb.rsplitn(2,'-').next().unwrap()
  )}
}

impl AdhocFormat {
  pub fn metavar(&self) -> String { self.0.to_uppercase() }
  pub fn name(&self) -> String { match self.0 {
    // special cases go here
    _ => return self.0.to_uppercase(),
  }/*.into()*/ }

  #[throws(AE)]
  pub fn parse<T>(&self, input: Vec<String>, what: &str) -> Vec<T>
  where T: DeserializeOwned
  {
    input.into_iter().enumerate().map(|(i,s)| match self.0 {
      "json" => serde_json::from_str(&s).map_err(AE::from),
      "ron"  => ron::de::   from_str(&s).map_err(AE::from),
      _ => panic!(),
    }
        .with_context(|| s.clone())
        .with_context(|| format!("parse {} (#{})", what, i))
    ).collect::<Result<Vec<T>,AE>>()?
  }

  #[throws(AE)]
  pub fn report<T>(&self, out: &mut CookedStdout, resp: T)
  where T: Serialize
  {
    writeln!(out, "{}", match self.0 {
      "json" => serde_json::to_string(&resp).map_err(AE::from),
      "ron"  => ron::ser::  to_string(&resp).map_err(AE::from),
      _ => panic!(),
    }
        .context("re-format response")?)?;
  }
}

//---------- adhoc command ----------

mod command_adhoc {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    cmds: Vec<String>,
  }

  fn subargs<'ap,'a:'ap,'m:'ap>(
    sa: &'a mut Args,
    ahf: AdhocFormat,
  ) -> ArgumentParser<'ap> {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.cmds).required()
      .add_argument(format!("{}-COMMAND", ahf.metavar()).leak(),
                    Collect,
                    format!("{}-encoded MgmtCommand", ahf.name())
                    .leak());
    ap
  }

  #[throws(AE)]
  fn call(SCCA{ mut out, ma, args,.. }:SCCA) {
    let ahf = ma.sc.into();

    let subargs: ApMaker<_> = &|sa| subargs(sa,ahf);
    let args = parse_args::<Args,_>(args, subargs, &ok_id, None);
    let mut conn = connect(&ma)?;

    let cmds: Vec<MgmtCommand> = ahf.parse(args.cmds, "cmd")?;
    for (i, cmd) in cmds.into_iter().enumerate() {
      let resp = conn.cmd(&cmd).with_context(|| format!("cmd #{}", i))?;
      ahf.report(&mut out, resp)?;
    }
  }

  inventory_subcmd!{
    "command-json",
    "run ad-hoc management command(s) (JSON)",
  }
  inventory_subcmd!{
    "command-ron",
    "run ad-hoc management command(s) (Rusty Object Notation)",
  }
}

//---------- alter game ----------

mod alter_game_adhoc {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    insns: Vec<String>,
  }

  fn subargs<'ap,'a:'ap,'m:'ap>(
    sa: &'a mut Args,
    ahf: AdhocFormat,
  ) -> ArgumentParser<'ap> {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.insns).required()
      .add_argument(format!("{}-INSN", ahf.metavar()).leak(),
                    Collect,
                    format!("{}-encoded MgmtGameInstruction", ahf.name())
                    .leak());
    ap
  }

  fn call(SCCA{ mut out, ma, args,.. }:SCCA) -> Result<(),AE> {
    let ahf = ma.sc.into();

    let subargs: ApMaker<_> = &|sa| subargs(sa,ahf);
    let args = parse_args::<Args,_>(args, subargs, &ok_id, None);
    let mut chan = ma.access_game()?;

    let insns: Vec<MgmtGameInstruction> = ahf.parse(args.insns, "insn")?;
    let resps = chan.alter_game(insns,None)?;
    for resp in resps {
      ahf.report(&mut out, resp)?;
    }

    Ok(())
  }

  inventory_subcmd!{
    "alter-game-json",
    "run an ad-hoc AlterGame command (JSON)",
  }
  inventory_subcmd!{
    "alter-game-ron",
    "run an ad-hoc AlterGame command (Rusty Object Notation)",
  }
}
