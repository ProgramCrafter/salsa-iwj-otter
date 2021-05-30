// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// management API implementation

use otter::imports::*;

use super::*;
use otter::commands::*;

use authproofs::*;

// ---------- newtypes, type aliases, basic definitions ----------

pub const IDLE_TIMEOUT:   Duration = Duration::from_secs(60);
pub const UPLOAD_TIMEOUT: Duration = Duration::from_secs(60);

use pwd::Passwd;
use std::os::unix::io::AsRawFd;
use std::os::unix::net::UnixListener;
use uds::UnixStreamExt;

type CSE = anyhow::Error;

type TP = TablePermission;

use MgmtResponse::Fine;

const USERLIST: &str = "/etc/userlist";
const CREATE_PIECES_MAX: u32 = 300;
const OVERALL_PIECES_MAX: usize = 100_000; // don't make not fit in i32

const DEFAULT_POS_START: Pos = PosC::new(20,20);
const DEFAULT_POS_DELTA: Pos = PosC::new(5,5);

pub struct CommandListener {
  listener: UnixListener,
}

struct CommandStream<'d> {
  chan: MgmtChannel,
  d: CommandStreamData<'d>,
}

struct CommandStreamData<'d> {
  desc: &'d str,
  account: Option<AccountSpecified>,
  authstate: AuthState,
}

type Euid = Result<Uid, ConnectionEuidDiscoverError>;

#[derive(Debug)]
enum AuthState {
  None { euid: Euid },
  Superuser { euid: Euid, auth: AuthorisationSuperuser },
  Ssh { key: sshkeys::KeySpec, auth: Authorisation<sshkeys::KeySpec> },
}

#[derive(Debug,Clone)]
struct AccountSpecified {
  notional_account: AccountName, // might not exist
  cooked: String, // account.to_string()
  auth: Authorisation<AccountName>, // but we did check permissions
}

enum PermissionCheckHow {
  Instance,
  InstanceOrOnlyAffectedAccount(AccountId),
  InstanceOrOnlyAffectedPlayer(PlayerId),
}

type PCH = PermissionCheckHow;

pub const TP_ACCESS_BUNDLES: &[TP] = &[
  TP::ViewNotSecret,
  TP::Play,
  TP::ChangePieces,
  TP::UploadBundles,
];

// ========== management API ==========

// ---------- management command implementations

//#[throws(CSE)]
fn execute_and_respond<W>(cs: &mut CommandStreamData, cmd: MgmtCommand,
                          bulk_upload: &mut ReadFrame<TimedFdReader>,
                          for_response: &mut FrameWriter<W>)
                          -> Result<(), CSE>
  where W: Write
{
  let mut bulk_download: Option<Box<dyn Read>> = None;
  let mut for_response = for_response
    .write_withbulk().context("start to respond")?;

  let mut cmd_s = log_enabled!(log::Level::Info)
    .as_some_from(|| format!("{:?}", &cmd))
    .unwrap_or_default();
  const MAX: usize = 200;
  if cmd_s.len() > MAX-3 {
    cmd_s.truncate(MAX-3);
    cmd_s += "..";
  }

  #[throws(MgmtError)]
  fn start_access_game(game: &InstanceName)
      -> (AccountsGuard, Unauthorised<InstanceRef, InstanceName>)
  {
    (
      AccountsGuard::lock(),
      Instance::lookup_by_name_unauth(&game)?,
    )
  }

  #[throws(MgmtError)]
  fn access_bundles<'ig,F,R>(
    cs: &mut CommandStreamData,
    ag: &AccountsGuard,
    gref: &'ig Unauthorised<InstanceRef, InstanceName>,
    perms: &[TablePermission],
    f: &mut F,
  ) -> (R, Authorisation<InstanceName>)
  where F: FnMut(
    &mut InstanceGuard<'ig>,
    BundlesGuard<'ig>,
  ) -> Result<R, MgmtError>
  {
    let bundles = gref.lock_bundles();
    let mut igu = gref.lock()?;
    let (mut ig, auth) = cs.check_acl(&ag, &mut igu, PCH::Instance, perms)?;
    let bundles = bundles.by(auth);
    let r = f(&mut ig, bundles)?;
    (r, auth)
  }

  #[throws(MgmtError)]
  fn start_access_ssh_keys(cs: &CommandStreamData)
      -> (AccountsGuard, AccountId, Authorisation<AccountScope>)
  {
    let ag = AccountsGuard::lock();
    let wanted = &cs.current_account()?.notional_account;
    let acctid = ag.check(wanted)?;
    let auth = authorise_scope_direct(cs, &ag, &wanted.scope)?;
    (ag, acctid, auth)
  }

  let resp = (|| Ok::<_,MgmtError>(match cmd {
    MC::Noop => Fine,

    MC::SetSuperuser(enable) => {
      let preserve_euid = match &cs.authstate {
        AuthState::None      { euid, .. } => euid,
        AuthState::Superuser { euid, .. } => euid,
        AuthState::Ssh { .. } => throw!(ME::AuthorisationError),
      }.clone();

      if !enable {
        cs.authstate = AuthState::None { euid: preserve_euid };
      } else {
        let ag = AccountsGuard::lock();
        let auth = authorise_scope_direct(cs, &ag, &AccountScope::Server)?;
        let auth = auth.therefore_ok();
        cs.authstate = AuthState::Superuser { euid: preserve_euid, auth };
      }
      Fine
    },
    MC::SetRestrictedSshScope { key } => {
      let good_uid = Some(config().ssh_proxy_uid);
      let auth = cs.authorised_uid(good_uid, Some("SetRestrictedScope"))
        .map_err(|_| ME::AuthorisationError)?;
      let auth = auth.therefore_ok();
      cs.authstate = AuthState::Ssh { key, auth };
      Fine
    },

    MC::CreateAccount(AccountDetails {
      account, nick, timezone, access, layout
    }) => {
      let mut ag = AccountsGuard::lock();
      let auth = authorise_for_account(cs, &ag, &account)?;
      let access = cs.accountrecord_from_spec(access)?
        .unwrap_or_else(|| AccessRecord::new_unset());
      let nick = nick.unwrap_or_else(|| account.to_string());
      let account = account.to_owned().into();
      let layout = layout.unwrap_or_default();
      let record = AccountRecord {
        account, nick, access,
        layout,
        timezone: timezone.unwrap_or_default(),
        ssh_keys: default(),
      };
      ag.insert_entry(record, auth)?;
      Fine
    }

    MC::UpdateAccount(AccountDetails {
      account, nick, timezone, access, layout
    }) => {
      let mut ag = AccountsGuard::lock();
      let mut games = games_lock();
      let auth = authorise_for_account(cs, &ag, &account)?;
      let access = cs.accountrecord_from_spec(access)?;
      ag.with_entry_mut(&mut games, &account, auth, access, |record, _acctid|{
        fn update_from<T>(spec: Option<T>, record: &mut T) {
          if let Some(new) = spec { *record = new; }
        }
        update_from(nick,                   &mut record.nick    );
        update_from(timezone,               &mut record.timezone);
        update_from(layout,                 &mut record.layout  );
        Fine
      })
        ?
        .map_err(|(e,_)|e) ?
    }

    MC::DeleteAccount(account) => {
      let mut ag = AccountsGuard::lock();
      let mut games = games_lock();
      let auth = authorise_for_account(cs, &ag, &account)?;
      ag.remove_entry(&mut games, &account, auth)?;
      Fine
    }

    MC::SelectAccount(wanted_account) => {
      let ag = AccountsGuard::lock();
      let auth = authorise_scope_direct(cs, &ag, &wanted_account.scope)?;
      cs.account = Some(AccountSpecified {
        cooked: wanted_account.to_string(),
        notional_account: wanted_account,
        auth: auth.therefore_ok(),
      });
      Fine
    }

    MC::CheckAccount => {
      let ag = AccountsGuard::lock();
      let _ok = ag.lookup(&cs.current_account()?.notional_account)?;
      Fine
    }

    MC::ListAccounts { all } => {
      let ag = AccountsGuard::lock();
      let names = if all == Some(true) {
        let auth = cs.superuser().ok_or(ME::AuthorisationError)?;
        ag.list_accounts_all(auth.into())
      } else {
        let AccountSpecified { notional_account, auth, .. } =
          cs.account.as_ref().ok_or(ME::SpecifyAccount)?;
        ag.list_accounts_scope(&notional_account.scope, *auth)
      };
      MR::AccountsList(names)
    }

    MC::CreateGame { game, insns } => {
      let mut ag = AccountsGuard::lock();
      let mut games = games_lock();
      let auth = authorise_by_account(cs, &ag, &game)?;

      let gs = otter::gamestate::GameState {
        table_colour: Html::lit("green").into(),
        table_size: DEFAULT_TABLE_SIZE,
        pieces: default(),
        players: default(),
        log: default(),
        gen: Generation(0),
        max_z: ZLevel::zero(),
        occults: default(),
      };

      let acl = default();
      let gref = Instance::new(game, gs, &mut games, acl, auth)?;
      let ig = gref.lock()?;
      let mut ig = Unauthorised::of(ig);

      let resp =
      execute_for_game(cs, &mut ag, &mut ig,
                       insns, MgmtGameUpdateMode::Bulk)
        .map_err(|e|{
          let ig = ig.by(Authorisation::authorise_any());
          let name = ig.name.clone();
          let InstanceGuard { c, .. } = ig;
          Instance::destroy_game(&mut games, c, auth)
            .unwrap_or_else(|e| warn!(
              "failed to tidy up failecd creation of {:?}: {:?}",
              &name, &e
            ));
          e
        })?;

      resp
    }

    MC::UploadBundle { game, size, hash, kind, progress } => {
      let (upload, auth) = {
        let (ag, gref) = start_access_game(&game)?;
        access_bundles(
          cs,&ag,&gref, &[TP::UploadBundles],
          &mut |mut ig, mut bundles: BundlesGuard<'_>| {
            bundles.start_upload(&mut ig, kind)
          }
        )?
      };
      bulk_upload.inner_mut().set_timeout(Some(UPLOAD_TIMEOUT));
      // If the timeout fires after the bulk data has all arrived, it
      // won't take effect, because: it only takes effect when we try
      // to read from the stresm, and after we have the data, we
      // won't read again until we go on to the next command - which
      // will have its own timeout.
      let uploaded = upload.bulk(bulk_upload, size,
                                 &hash, progress, &mut for_response)?;
      let bundle = {
        let gref = Instance::lookup_by_name(&game, auth)?;
        let mut bundles = gref.lock_bundles();
        let mut ig = gref.lock()?;
        bundles.finish_upload(&mut ig, uploaded)?
      };
      MR::Bundle { bundle }
    }
    MC::ListBundles { game } => {
      let (ag, gref) = start_access_game(&game)?;
      let (bundles,_auth) =
        access_bundles(
          cs,&ag,&gref,TP_ACCESS_BUNDLES,
          &mut |ig,_|{
            Ok(ig.bundle_list.clone())
          })?;
      MR::Bundles { bundles }
    }
    MC::DownloadBundle { game, id } => {
      let (ag, gref) = start_access_game(&game)?;
      access_bundles(
        cs,&ag,&gref,TP_ACCESS_BUNDLES,
        &mut |ig,_|{
          let f = id.open(&ig)?.ok_or_else(|| ME::BundleNotFound)?;
          bulk_download = Some(Box::new(f));
          Ok(())
        })?;
      Fine
    }
    MC::ClearBundles { game } => {
      let (ag, gref) = start_access_game(&game)?;
      access_bundles(
        cs,&ag,&gref, &[TP::ClearBundles],
        &mut |mut ig, mut bundles: BundlesGuard<'_>| {
          bundles.clear(&mut ig)
        })?;
      Fine
    }

    MC::ListGames { all } => {
      let ag = AccountsGuard::lock();
      let names = Instance::list_names(
        None, Authorisation::authorise_any());
      let auth_all = if all == Some(true) {
        let auth = cs.superuser().ok_or(ME::AuthorisationError)?.into();
        Some(auth)
      } else {
        None
      };
      let mut names = names.into_iter().map(|name| {
        let gref = Instance::lookup_by_name_unauth(&name)?;
        let mut igu = gref.lock_even_destroying();
        let _ig = if let Some(auth_all) = auth_all {
          igu.by_ref(auth_all)
        } else {
          cs.check_acl(&ag, &mut igu, PCH::Instance, &[TP::ShowInList])?.0
        };
        Ok::<_,ME>(name)
      }).filter(|ent| matches_doesnot!(
        ent,
        = Ok(_),
        ! Err(ME::GameNotFound) | Err(ME::AuthorisationError),
        = Err(_),
      ))
        .collect::<Result<Vec<_>,_>>() ?;
      names.sort_unstable();
      MR::GamesList(names)
    }

    MC::AlterGame { game, insns, how } => {
      let (mut ag, gref) = start_access_game(&game)?;
      let mut g = gref.lock()?;
      execute_for_game(cs, &mut ag, &mut g, insns, how)?
    }

    MC::DestroyGame { game } => {
      let mut ag = AccountsGuard::lock();
      let mut games = games_lock();
      let auth = authorise_by_account(cs, &mut ag, &game)?;
      let gref = Instance::lookup_by_name_locked(&games, &game, auth)?;
      let ig = gref.lock_even_destroying();
      Instance::destroy_game(&mut games, ig, auth)?;
      Fine
    }

    MC::LibraryListLibraries { game } => {
      let (ag, gref) = start_access_game(&game)?;
      let (libs, _auth) =
        access_bundles(
          cs,&ag,&gref, &[TP::UploadBundles],
          &mut |ig, _| Ok(
            ig.all_shapelibs()
              .iter()
              .map(|reg| reg.iter())
              .flatten()
              .map(|ll| ll.iter())
              .flatten()
              .map(|l| l.enquiry())
              .collect::<Vec<LibraryEnquiryData>>()
          )
        )?;
      MR::Libraries(libs)
    }

    MC::LibraryListByGlob { game, lib, pat } => {
      let (ag, gref) = start_access_game(&game)?;
      let (results, _auth) =
        access_bundles(
          cs,&ag,&gref, &[TP::UploadBundles],
          &mut |ig, _| {
            let regs = ig.all_shapelibs();
            let mut results: Vec<ItemEnquiryData> = default();
            let libss = if let Some(lib) = &lib {
              vec![regs.lib_name_lookup(lib)?]
            } else {
              regs.all_libs().collect()
            };
            for libs in libss {
              for lib in libs {
                results.extend(lib.list_glob(&pat)?);
              }
            }
            Ok(results)
          })?;
      MR::LibraryItems(results)
    }

    MC::SshListKeys => {
      let (ag, acctid, auth) = start_access_ssh_keys(&cs)?;
      let list = ag.sshkeys_report(acctid, auth)?;
      MR::SshKeys(list)
    }
    MC::SshAddKey { akl } => {
      let (mut ag, acctid, auth) = start_access_ssh_keys(&cs)?;
      let (index, id) = ag.sshkeys_add(acctid, akl, auth)?;
      MR::SshKey { index, id }
    }
    MC::SshDeleteKey { index, id } => {
      let (mut ag, acctid, auth) = start_access_ssh_keys(&cs)?;
      ag.sshkeys_remove(acctid, index, id, auth)?;
      MR::Fine
    }

    MC::LoadFakeRng(ents) => {
      let superuser = cs.superuser()
        .ok_or(ME::SuperuserAuthorisationRequired)?;
      config().game_rng.set_fake(ents, superuser)?;
      Fine
    }
  }))();

  let resp = match resp {
    Ok(resp) => {
      info!("command connection {}: executed {}",
            &cs.desc, cmd_s);
      resp
    }
    Err(error) => {
      info!("command connection {}: error {:?} from {}",
            &cs.desc, &error, cmd_s);
      MgmtResponse::Error { error }
    }
  };

  let mut wf = for_response.respond(&resp).context("respond")?;
  if let Some(mut bulk_download) = bulk_download {
    io::copy(&mut bulk_download, &mut wf).context("download")?;
  }
  wf.finish().context("flush")?;
  Ok(())
}

// ---------- game command implementations ----------



type ExecuteGameInsnResults<'igr, 'ig> = (
  ExecuteGameChangeUpdates,
  MgmtGameResponse,
  UnpreparedUpdates, // These happena after everything else
  Vec<MgmtGameInstruction>,
  &'igr mut InstanceGuard<'ig>,
);

//#[throws(ME)]
fn execute_game_insn<'cs, 'igr, 'ig: 'igr>(
  cs: &'cs CommandStreamData,
  ag: &'_ mut AccountsGuard,
  ig: &'igr mut Unauthorised<InstanceGuard<'ig>, InstanceName>,
  update: MgmtGameInstruction,
  who: &Html,
  to_permute: &mut ToRecalculate,
)
  -> Result<ExecuteGameInsnResults<'igr, 'ig> ,ME>
{
  type U = ExecuteGameChangeUpdates;
  use MgmtGameResponse::Fine;

  fn tz_from_str(s: &str) -> Timezone {
    Timezone::from_str(s).void_unwrap()
  }

  fn no_updates<'igr,'ig>(ig: &'igr mut InstanceGuard<'ig>,
                          mgr: MgmtGameResponse)
                          -> ExecuteGameInsnResults<'igr, 'ig> {
    (U{ pcs: vec![], log: vec![], raw: None }, mgr, None, vec![], ig)
  }

  #[throws(MgmtError)]
  fn readonly<'igr, 'ig: 'igr, 'cs,
              F: FnOnce(&InstanceGuard) -> Result<MgmtGameResponse,ME>,
              P: Into<PermSet<TablePermission>>>
  
    (
      cs: &'cs CommandStreamData,
      ag: &AccountsGuard,
      ig: &'igr mut Unauthorised<InstanceGuard<'ig>, InstanceName>,
      p: P,
      f: F
    ) -> ExecuteGameInsnResults<'igr, 'ig>
  {
    let (ig, _) = cs.check_acl(ag, ig, PCH::Instance, p)?;
    let resp = f(ig)?;
    (U{ pcs: vec![], log: vec![], raw: None }, resp, None, vec![], ig)
  }

  #[throws(MgmtError)]
  fn pieceid_lookup<'igr, 'ig: 'igr, 'cs, 
                    F: FnOnce(&PerPlayerIdMap) -> MGR>
    (
      cs: &'cs CommandStreamData,
      ig: &'igr mut Unauthorised<InstanceGuard<'ig>, InstanceName>,
      player: PlayerId,
      f: F,
    ) -> ExecuteGameInsnResults<'igr, 'ig>
  {
    let superuser = cs.superuser().ok_or(ME::SuperuserAuthorisationRequired)?;
    let ig = ig.by_mut(superuser.into());
    let gpl = ig.gs.players.byid(player)?;
    let resp = f(&gpl.idmap);
    no_updates(ig, resp)
  }

  #[throws(MgmtError)]
  fn some_synch_core(ig: &mut InstanceGuard<'_>) -> (Generation, MGR) {
    let mut buf = PrepareUpdatesBuffer::new(ig, None);
    let gen = buf.gen();
    drop(buf); // does updatenocc
    ig.save_game_now()?;
    // we handled the update ourselves, return no update info, just MGR
    (gen, MGR::Synch(gen))
  }

  #[throws(MgmtError)]
  fn update_links<'igr, 'ig: 'igr, 'cs,
               F: FnOnce(&mut Arc<LinksTable>) -> Result<Html,ME>>
    (
      cs: &'cs CommandStreamData,
      ag: &'_ mut AccountsGuard,
      ig: &'igr mut Unauthorised<InstanceGuard<'ig>, InstanceName>,
      f: F
    ) -> ExecuteGameInsnResults<'igr, 'ig>
  {
    let ig = cs.check_acl(ag, ig, PCH::Instance, &[TP::SetLinks])?.0;
    let log_html = f(&mut ig.links)?;
    let log = vec![LogEntry { html: log_html, }];
    (U{ log,
        pcs: vec![],
        raw: Some(vec![ PreparedUpdateEntry::SetLinks(ig.links.clone()) ])},
     Fine, None, vec![], ig)
  }

  #[throws(MgmtError)]
  fn reset_game<'igr, 'ig: 'igr, 'cs>(
    cs: &'cs CommandStreamData,
    ag: &'_ mut AccountsGuard,
    ig: &'igr mut Unauthorised<InstanceGuard<'ig>, InstanceName>,
    f: Box<dyn FnOnce(&mut InstanceGuard, &mut Vec<MgmtGameInstruction>)
                      -> Result<LogEntry, MgmtError> + '_>
  ) -> ExecuteGameInsnResults<'igr, 'ig>
  {
    let ig = cs.check_acl(&ag, ig, PCH::Instance, &[TP::ChangePieces])?.0;

    // Clear out old stuff
    let mut insns = vec![];
    for alias in ig.pcaliases.keys() {
      insns.push(MGI::DeletePieceAlias(alias.clone()));
    }
    for piece in ig.gs.pieces.keys() {
      insns.push(MGI::DeletePiece(piece));
    }

    let logent = f(ig, &mut insns)?;

    (U{ pcs: vec![],
        log: vec![ logent ],
        raw: None },
     MGR::InsnExpanded,
     None, insns, ig)
  }

  #[throws(MgmtError)]
  fn reset_game_from_spec<'igr, 'ig: 'igr, 'cs>(
    cs: &'cs CommandStreamData,
    ag: &'_ mut AccountsGuard,
    ig: &'igr mut Unauthorised<InstanceGuard<'ig>, InstanceName>,
    who: &'_ Html,
    get_spec_toml: Box<dyn FnOnce(&InstanceGuard)
                                  -> Result<String, MgmtError>>,
  ) -> ExecuteGameInsnResults<'igr, 'ig>
  {
    reset_game(cs,ag,ig, Box::new(|ig, insns|{
      let spec = get_spec_toml(ig)?;
      let spec: toml::Value = spec.parse()
        .map_err(|e: toml::de::Error| ME::TomlSyntaxError(e.to_string()))?;
      let GameSpec {
        pieces, table_size, table_colour, pcaliases,
      } = toml_de::from_value(&spec)
        .map_err(|e: toml_de::Error| ME::TomlStructureError(e.to_string()))?;

      // Define new stuff:
      for (alias, target) in pcaliases.into_iter() {
        insns.push(MGI::DefinePieceAlias{ alias, target });
      }
      insns.push(MGI::ClearLog);
      insns.push(MGI::SetTableSize(table_size));
      insns.push(MGI::SetTableColour(table_colour));
      for pspec in pieces.into_iter() {
        insns.push(MGI::AddPieces(pspec));
      }
      let html = hformat!("{} reset the game", &who);
      Ok(LogEntry { html })
    }))?
  }

  impl<'cs> CommandStreamData<'cs> {
    #[throws(MgmtError)]
    fn check_acl_manip_player_access<'igr, 'ig: 'igr>(
      &self,
      ag: &AccountsGuard,
      ig: &'igr mut Unauthorised<InstanceGuard<'ig>, InstanceName>,
      player: PlayerId,
      perm: TablePermission,
    ) -> (&'igr mut InstanceGuard<'ig>, Authorisation<AccountName>) {
      let (ig, auth) = self.check_acl(ag, ig,
                            PCH::InstanceOrOnlyAffectedPlayer(player),
                                      &[perm])?;
      fn auth_map(n: &InstanceName) -> &AccountName { &n.account }
//      let auth_map = |n: &InstanceName| -> &AccountName { &n.account };
      let auth = auth.map(auth_map);
      (ig, auth)
    }
  }

  let y = match update {
    MGI::Noop { } => readonly(cs,ag,ig, &[TP::TestExistence], |_| Ok(Fine))?,

    MGI::SetTableSize(size) => {
      let ig = cs.check_acl(ag, ig, PCH::Instance, &[TP::ChangePieces])?.0;
      for p in ig.gs.pieces.values() {
        p.pos.clamped(size)?;
      }
      ig.gs.table_size = size;
      (U{ pcs: vec![],
          log: vec![ LogEntry {
            html: hformat!("{} resized the table to {}x{}",
                           who, size.x(), size.y()),
          }],
          raw: Some(vec![ PreparedUpdateEntry::SetTableSize(size) ]) },
       Fine, None, vec![], ig)
    }

    MGI::SetTableColour(colour) => {
      let ig = cs.check_acl(ag, ig, PCH::Instance, &[TP::ChangePieces])?.0;
      let colour: Colour = (&colour).try_into().map_err(|e| SpE::from(e))?;
      ig.gs.table_colour = colour.clone();
      (U{ pcs: vec![],
          log: vec![ LogEntry {
            html: hformat!("{} recoloured the tabletop to {}",
                           &who, &colour),
          }],
          raw: Some(vec![ PreparedUpdateEntry::SetTableColour(colour) ]) },
       Fine, None, vec![], ig)
    }

    MGI::JoinGame {
      details: MgmtPlayerDetails { nick },
    } => {
      let account = &cs.current_account()?.notional_account;
      let (arecord, acctid) = ag.lookup(account)?;
      let (ig, auth) = cs.check_acl(ag, ig, PCH::Instance, &[TP::Play])?;
      let nick = nick.ok_or(ME::MustSpecifyNick)?;
      let logentry = LogEntry {
        html: hformat!("{} [{}] joined the game", nick, account),
      };
      let timezone = &arecord.timezone;
      let tz = tz_from_str(&timezone);
      let gpl = GPlayer {
        nick: nick.to_string(),
        layout: arecord.layout,
        idmap: default(),
        moveheld: default(),
        movehist: default(),
      };
      let ipl = IPlayer {
        acctid,
        tz,
        tokens_revealed: default(),
      };
      let (player, update, logentry) =
        ig.player_new(gpl, ipl, arecord.account.clone(), logentry)?;

      let atr = ig.player_access_reset(ag, player, auth.therefore_ok())?;

      (U{ pcs: vec![],
          log: vec![ logentry ],
          raw: Some(vec![ update ] )},
       MGR::JoinGame { nick, player, token: atr },
       None, vec![], ig)
    },

    MGI::DeletePieceAlias(alias) => {
      let ig = cs.check_acl(&ag, ig, PCH::Instance, &[TP::ChangePieces])?.0;
      ig.pcaliases.remove(&alias);
      no_updates(ig, MGR::Fine)
    },
    MGI::DefinePieceAlias { alias, target } => {
      let ig = cs.check_acl(&ag, ig, PCH::Instance, &[TP::ChangePieces])?.0;
      ig.pcaliases.insert(alias, target);
      no_updates(ig, MGR::Fine)
    },

    MGI::ClearGame { } => {
      reset_game(cs,ag,ig, Box::new(|_ig, _insns|{
        let html = hformat!("{} cleared out the game", &who);
        Ok(LogEntry { html })
      }))?
    }

    MGI::ResetFromNamedSpec { spec } => {
      reset_game_from_spec(cs,ag,ig,who, Box::new(move |ig| {
        let data = bundles::load_spec_to_read(ig,&spec)?;
        Ok::<_,ME>(data)
      }))?
    }

    MGI::ResetFromGameSpec { spec_toml: spec } => {
      reset_game_from_spec(cs,ag,ig,who, Box::new(|_| Ok::<_,ME>(spec)))?
    }

    MGI::InsnMark(token) => {
      let (ig, _) = cs.check_acl(&ag,ig,PCH::Instance, &[TP::TestExistence])?;
      no_updates(ig, MGR::InsnMark(token))
    }

    MGI::Synch => {
      let (ig, _) = cs.check_acl(&ag, ig, PCH::Instance, &[TP::Play])?;
      let (_gen, mgr) = some_synch_core(ig)?;
      no_updates(ig, mgr)
    }

    MGI::SynchLog => {
      let superuser = cs.superuser()
        .ok_or(ME::SuperuserAuthorisationRequired)?;
      let ig = ig.by_mut(superuser.into());
      let (gen, mgr) = some_synch_core(ig)?;
      let log = LogEntry { html: synch_logentry(gen) };
      (U{ pcs: vec![], log: vec![log], raw: None }, mgr, None, vec![], ig)
    },

    MGI::PieceIdLookupFwd { player, piece } => {
      pieceid_lookup(
        cs, ig, player,
        |ppidm| MGR::VisiblePieceId(ppidm.fwd(piece))
      )?
    },
    MGI::PieceIdLookupRev { player, vpid } => {
      pieceid_lookup(
        cs, ig, player,
        |ppidm| MGR::InternalPieceId(ppidm.rev(vpid))
      )?
    },

    MGI::ListPieces => readonly(cs,ag,ig, &[TP::ViewNotSecret], |ig|{
      let ioccults = &ig.ioccults;
      let pieces = ig.gs.pieces.iter().filter_map(
        |(piece,gpc)| (|| Ok::<_,MgmtError>(if_chain!{
          let &GPiece { pos, face, .. } = gpc;
          if let Some(ipc) = ig.ipieces.get(piece);
          let unocc = gpc.fully_visible_to_everyone();
          let visible = if let Some(y) = unocc {
            // todo: something more sophisticated would be nice
            let pri = PieceRenderInstructions::new_visible(
              // visible id is internal one here
              VisiblePieceId(piece.data())
            );
            let bbox = ipc.show(y).bbox_approx()?;
            let desc_html = pri.describe(ioccults,&ig.gs.occults, gpc, ipc);
            Some(MgmtGamePieceVisibleInfo {
              pos, face, desc_html, bbox
            })
          } else {
            None
          };
          let itemname = if let Some(unocc) = unocc {
            ipc.show(unocc).itemname().to_string()
          } else {
            "occulted-item".to_string()
          };
          let itemname = itemname.try_into().map_err(
            |e| internal_error_bydebug(&e)
          )?;
          then {
            Some(MgmtGamePieceInfo {
              piece,
              itemname,
              visible,
            })
          } else {
            None
          }
        }))().transpose()
      ).collect::<Result<Vec<_>,_>>()?;
      let pcaliases = ig.pcaliases.keys().cloned().collect();
      Ok(MGR::Pieces { pieces, pcaliases })
    })?,

    MGI::UpdatePlayer {
      player,
      details: MgmtPlayerDetails { nick },
    } => {
      let ig = cs.check_acl_modify_player(ag, ig, player,
                                          &[TP::ModifyOtherPlayer])?.0;
      let mut log = vec![];
      if let Some(new_nick) = nick {
        ig.check_new_nick(&new_nick)?;
        let gpl = ig.gs.players.byid_mut(player)?;
        log.push(LogEntry {
          html: hformat!("{} changed {}'s nick to {}",
                         &who, gpl.nick, new_nick),
        });
        gpl.nick = new_nick;
      }
      (U{ log,
          pcs: vec![],
          raw: None},
       Fine, None, vec![], ig)
    },

    MGI::Info => readonly(cs,ag,ig, &[TP::ViewNotSecret], |ig|{
      let players = ig.gs.players.iter().map(
        |(player, gpl)| {
          let nick = gpl.nick.clone();
          if_chain! {
            if let Some(ipr) = ig.iplayers.get(player);
            let ipl = &ipr.ipl;
            if let Ok((arecord, _)) = ag.lookup(ipl.acctid);
            then { Ok((player, MgmtPlayerInfo {
              account: (*arecord.account).clone(),
              nick,
            })) }
            else {
              throw!(InternalError::PartialPlayerData)
            }
          }
        }
      ).collect::<Result<SecondarySlotMap<_,_>,ME>>()?;
      let table_size = ig.gs.table_size;
      let links = ig.links.iter().filter_map(
        |(k,v)|
        Some((k.clone(), UrlSpec(v.as_ref()?.clone())))
      ).collect();
      let info = MgmtGameResponseGameInfo { table_size, players, links };
      Ok(MGR::Info(info))
    })?,

    MGI::SetLinks(mut spec_links) =>  {
      update_links(cs,ag,ig, |ig_links|{
        let mut new_links: LinksTable = default();
        // todo want a FromIterator impl
        for (k,v) in spec_links.drain() {
          let url: Url = (&v).try_into()?;
          new_links[k] = Some(url.into());
        }
        let new_links = Arc::new(new_links);
        *ig_links = new_links.clone();
        Ok(
          hformat!("{} set the links to off-server game resources",
                   who)
        )
      })?
    }

    MGI::SetLink { kind, url } =>  {
      update_links(cs,ag,ig, |ig_links|{
        let mut new_links: LinksTable = (**ig_links).clone();
        let url: Url = (&url).try_into()?;
        let show: Html = (kind, url.as_str()).to_html();
        new_links[kind] = Some(url.into());
        let new_links = Arc::new(new_links);
        *ig_links = new_links.clone();
        Ok(
          hformat!("{} set the link {}",
                  who, show)
        )
      })?
    }

    MGI::RemoveLink { kind } =>  {
      update_links(cs,ag,ig, |ig_links|{
        let mut new_links: LinksTable = (**ig_links).clone();
        new_links[kind] = None;
        let new_links = Arc::new(new_links);
        *ig_links = new_links.clone();
        Ok(
          hformat!("{} removed the link {}",
                  who, &kind)
        )
      })?
    }

    MGI::ResetPlayerAccess(player) => {
      let (ig, auth) = cs.check_acl_manip_player_access
        (ag, ig, player, TP::ResetOthersAccess)?;

      let token = ig.player_access_reset(ag, player, auth)?;
      no_updates(ig, MGR::PlayerAccessToken(token))
    }

    MGI::RedeliverPlayerAccess(player) => {
      let (ig, auth) = cs.check_acl_manip_player_access
        (ag, ig, player, TP::RedeliverOthersAccess)?;

      let token = ig.player_access_redeliver(ag, player, auth)?;
      no_updates(ig, MGR::PlayerAccessToken(token))
    },

    MGI::LeaveGame(player) => {
      let account = &cs.current_account()?.notional_account;
      let (ig, _auth) = cs.check_acl_manip_player_access
        (ag, ig, player, TP::ModifyOtherPlayer)?;

      let got = ig.players_remove(&[player].iter().cloned().collect())?;

      let (gpl, ipl, update) = got.into_iter().next()
        .ok_or(PlayerNotFound)?;

      let html =
        hformat!("{} [{}] left the game [{}]"
                ,
                (|| Some(gpl?.nick))()
                .unwrap_or("<partial data!>".into())
                ,
                (||{
                  let (record, _) = ag.lookup(ipl?.acctid).ok()?;
                  Some(record.account.to_string())
                })()
                .unwrap_or("<account deleted>".into())
                ,
                &account
                )
        ;

      (U{ pcs: vec![],
          log: vec![ LogEntry { html }],
          raw: Some(vec![ update ]) },
       Fine, None, vec![], ig)
    },

    MGI::DeletePiece(piece) => {
      let (ig_g, modperm, _) = cs.check_acl_modify_pieces(ag, ig)?;
      let ig = &mut **ig_g;
      let _ipc = ig.ipieces.as_mut(modperm)
        .get(piece).ok_or(ME::PieceNotFound)?;
      let gs = &mut ig.gs;
      let gpc = gs.pieces.as_mut(modperm).get_mut(piece);
      let mut xupdates = vec![];
      if let Some(gpc) = gpc {
        gpc.occult.passive_delete_hook(&mut gs.occults, piece);
        if gpc.occult.is_active() {
          drop(gpc);
          xupdates.append(
            &mut
              remove_occultation(
                &mut gs.gen.unique_gen(),
                &mut gs.players,
                &mut gs.pieces,
                &mut gs.occults,
                &mut ig.ipieces,
                &mut ig.ioccults,
                to_permute,
                piece)?
          );
        }
      }
      let ioccults = &ig.ioccults;
      let gpc = gs.pieces.as_mut(modperm).remove(piece);
      let ipc = ig.ipieces.as_mut(modperm).remove(piece).unwrap();
      let desc_html = if let Some(gpc) = &gpc {
        let pri = PieceRenderInstructions::new_visible(default());
        pri.describe(ioccults,&gs.occults, gpc, &ipc)
      } else {
        "<piece partially missing from game state>".to_html()
      };
      if let Some(gpc) = gpc {
        ipc.p.into_inner().delete_hook(&gpc, gs);
      }
      if let Some(occilk) = ipc.occilk {
        ig.ioccults.ilks.dispose(occilk);
      }
      (U{ pcs: vec![(piece, PieceUpdateOp::Delete())],
          log: vec![ LogEntry {
            html: hformat!("A piece {} was removed from the game",
                          desc_html),
          }],
          raw: None },
       Fine,
       if xupdates.len() != 0 {
         Some(
           Box::new(move |prepub: &mut PrepareUpdatesBuffer|
                    prepub.piece_updates(xupdates, &None))
             as SomeUnpreparedUpdates
         )
       } else { None },
       vec![],
       ig_g)
    },

    MGI::AddPieces(PiecesSpec{ pos,posd,count,face,pinned,angle,info }) => {
      let (ig_g, modperm, _) = cs.check_acl_modify_pieces(ag, ig)?;
      let ig = &mut **ig_g;
      let gs = &mut ig.gs;
      let implicit: u32 = info.count(&ig.pcaliases)?
        .try_into().map_err(
          |_| SpE::InternalError(format!("implicit item count out of range"))
        )?;
      let count: Box<dyn ExactSizeIterator<Item=u32>> = match count {
        Some(explicit) if implicit == 1 => {
          Box::new((0..explicit).map(|_| 0))
        },
        Some(explicit) if implicit != explicit => {
          throw!(SpecError::InconsistentPieceCount)
        },
        None | Some(_) => {
          Box::new(0..implicit)
        },
      };

      let count_len = count.len();
      if count_len > CREATE_PIECES_MAX as usize { throw!(ME::LimitExceeded) }
      if gs.pieces.len() + count_len > OVERALL_PIECES_MAX {
        throw!(ME::LimitExceeded)
      }
      let posd = posd.unwrap_or(DEFAULT_POS_DELTA);

      let mut updates = Vec::with_capacity(count_len);
      let mut pos = pos.unwrap_or(DEFAULT_POS_START);
      let mut z = gs.max_z.z.clone_mut();
      for piece_i in count {
        let gs = &mut ig.gs;
        let face = face.unwrap_or_default();
        let mut gpc = GPiece {
          held: None,
          zlevel: ZLevel { z: z.increment()?, zg: gs.gen },
          lastclient: default(),
          occult: default(),
          gen_before_lastclient: Generation(0),
          pinned: pinned.unwrap_or(false),
          angle,
          gen: gs.gen,
          pos, face,
          xdata: None,
          moveable: default(),
          rotateable: true,
        };
        let PieceSpecLoaded { p, loaded_via_alias, occultable } =
          info.load(piece_i as usize, &mut gpc, &ig, SpecDepth::zero())?;
        if p.nfaces() <= face.into() {
          throw!(SpecError::FaceNotFound);
        }
        let gs = &mut ig.gs;
        gpc.pos.clamped(gs.table_size)?;
        if gpc.zlevel > gs.max_z { gs.max_z = gpc.zlevel.clone() }
        let piece = gs.pieces.as_mut(modperm).insert(gpc);
        let p = IPieceTraitObj::new(p);
        (||{
          let ilks = &mut ig.ioccults.ilks;
          let occilk = occultable.map(|(ilkname, p_occ)| {
            ilks.create(ilkname, OccultIlkData { p_occ })
          });
          ig.ipieces.as_mut(modperm).insert(piece, IPiece {
            p, occilk, loaded_via_alias,
          });
          updates.push((piece, PieceUpdateOp::Insert(())));
        })(); // <- no ?, infallible (to avoid leaking ilk)
        pos = (pos + posd)?;
      }

      (U{ pcs: updates,
          log: vec![ LogEntry {
            html: hformat!("{} added {} pieces", who, count_len),
          }],
          raw: None },
       Fine, None, vec![], ig_g)
    },

    MGI::ClearLog => {
      let (ig, _) = cs.check_acl(&ag, ig, PCH::Instance, &[TP::Super])?;
      for gpl in ig.gs.players.values_mut() {
        gpl.movehist.clear();
      }
      ig.gs.log.clear();
      for ipr in ig.iplayers.values_mut() {
        // todo: do this only if there are no hidden pieces?
        let tr = &mut ipr.ipl.tokens_revealed;
        let latest = tr.values()
          .map(|trv| trv.latest)
          .max();
        if let Some(latest) = latest {
          tr.retain(|_k, v| v.latest >= latest);
        }
      }
      let raw = Some(vec![ PUE::MoveHistClear ]);
      (U{ pcs: vec![ ],
          log: vec![ LogEntry {
            html: hformat!("{} cleared the log history", who),
          } ],
          raw },
       Fine, None, vec![], ig)
    },

    MGI::SetACL { acl } => {
      let (ig, _) = cs.check_acl(&ag, ig, PCH::Instance, &[TP::Super])?;
      ig.acl = acl.into();
      let mut log = vec![ LogEntry {
        html: hformat!("{} set the table access control list",
                       who),
      } ];

      #[throws(InternalError)]
      fn remove_old_players(ag: &AccountsGuard, ig: &mut InstanceGuard,
                            who: &Html,
                            log: &mut Vec<LogEntry>)
                            -> Vec<PreparedUpdateEntry>
      {
        let owner_account = ig.name.account.to_string();
        let eacl = EffectiveACL {
          owner_account: Some(&owner_account),
          acl: &ig.acl,
        };

        let mut remove = HashSet::new();
        for (player, ipr) in &ig.iplayers {
          if_chain! {
            let acctid = ipr.ipl.acctid;
            if let Ok((record,_)) = ag.lookup(acctid);
            let perm = &[TP::Play];
            if let Ok(_) = eacl.check(&record.account.to_string(),
                                      perm.into());
            then {
              /* ok */
            }
            else {
              remove.insert(player);
            }
          };
        };

        let mut updates = Vec::new();
        for (gpl, _ipl, update) in ig.players_remove(&remove)? {
          let show = if let Some(gpl) = gpl {
            htmlescape::encode_minimal(&gpl.nick)
          } else {
            "<i>partial data?!</i>".to_string()
          };
          log.push(LogEntry {
            html: hformat!("{} removed a player {}", who, show),
          });
          updates.push(update);
        }

        updates
      }

      let updates = remove_old_players(&ag, ig, who, &mut log)?;

      (U{ pcs: vec![ ],
          log,
          raw: Some(updates) },
       Fine, None, vec![], ig)
    },
  };
  Ok(y)
}

// ---------- how to execute game commands & handle their updates ----------

#[throws(ME)]
fn execute_for_game<'cs, 'igr, 'ig: 'igr>(
  cs: &'cs CommandStreamData,
  ag: &mut AccountsGuard,
  igu: &'igr mut Unauthorised<InstanceGuard<'ig>, InstanceName>,
  insns: Vec<MgmtGameInstruction>,
  how: MgmtGameUpdateMode) -> MgmtResponse
{
  let (ok, uu) = ToRecalculate::with(|mut to_permute| {
    let r = (||{

  let who = if_chain! {
    let account = &cs.current_account()?.notional_account;
    let ig = igu.by_ref(Authorisation::authorise_any());
    if let Ok((_, acctid)) = ag.lookup(account);
    if let Some((player,_)) = ig.iplayers.iter()
      .filter(|(_,ipr)| ipr.ipl.acctid == acctid)
      .next();
    if let Some(gpl) = ig.gs.players.get(player);
    then { hformat!("{} [{}]",
                        gpl.nick,
                        account) }
    else { hformat!("[{}]",
                        account) }
  };

  let mut responses = Vec::with_capacity(insns.len());

  struct St {
    uh: UpdateHandler,
    auth: Authorisation<InstanceName>,
    have_deleted: bool,
  }
  impl St {
    #[throws(ME)]
    fn flush<'igr, 'ig>(&mut self,
             ig: &'igr mut InstanceGuard<'ig>,
             how: MgmtGameUpdateMode,
             who: &Html) {
      mem::replace(
        &mut self.uh,
        UpdateHandler::from_how(how),
      )
        .complete(ig, &who)?;
    }
    #[throws(ME)]
    fn flushu<'igr, 'ig>(&mut self,
             igu: &'igr mut Unauthorised<InstanceGuard<'ig>, InstanceName>,
             how: MgmtGameUpdateMode,
             who: &Html) {
      let ig = igu.by_mut(self.auth);
      self.flush(ig, how, who)?;
    }
  }
  let mut uh_auth: Option<St> = None;
  let flush_uh = |st: &mut St, igu: &'_ mut _| st.flushu(igu, how, &who);
  let mut insns: VecDeque<_> = insns.into();
  let res = (||{
    while let Some(insn) = insns.pop_front() {
      trace_dbg!("exeucting game insns", insn);

      if_chain!{
        if let MGI::AddPieces{..} = &insn;
        if let Some(ref mut st) = &mut uh_auth;
        if st.have_deleted == true;
        then {
          // This makes sure that all the updates we have queued up
          // talking about the old PieceId, will be Prepared (ie, the
          // vpid lookup done) before we reuse the slot and render the
          // vpid lookup impossible.
          flush_uh(st,igu)?;
        }
      }
      let was_delete = matches!(&insn, MGI::DeletePiece(..));

      let (updates, resp, unprepared, expand, ig) =
        execute_game_insn(cs, ag, igu, insn, &who,
                          &mut to_permute)?;
      let st = uh_auth.get_or_insert_with(||{
        let auth = Authorisation::authorised(&*ig.name);
        let uh = UpdateHandler::from_how(how);
        St { uh, auth, have_deleted: false }
      });
      st.have_deleted |= was_delete;
      st.uh.accumulate(ig, updates)?;
      if matches!(&resp, MGR::InsnExpanded) {
        let mut expand: VecDeque<_> = expand.into();
        expand.append(&mut insns);
        insns = expand;
      } else {
        assert!(expand.is_empty())
      }
      responses.push(resp);
      if let Some(unprepared) = unprepared {
        st.flush(ig,how,&who)?;
        let mut prepub = PrepareUpdatesBuffer::new(ig, None);
        unprepared(&mut prepub);
        prepub.finish();
      }
    }
    if let Some(ref mut st) = uh_auth {
      flush_uh(st,igu)?;
    }
    Ok(None)
  })();
  if let Some(mut st) = uh_auth {
    flush_uh(&mut st, igu)?;
    igu.by_mut(st.auth).save_game_now()?;
  }
  Ok::<_,ME>(MgmtResponse::AlterGame {
    responses,
    error: res.unwrap_or_else(Some),
  })

    })();
    (r, {
      let ig = igu.by_mut(Authorisation::authorise_any());
      let g = &mut **ig;
      let gs = &mut g.gs;
      to_permute.implement(&mut gs.players,
                           &mut gs.pieces,
                           &mut gs.occults,
                           &g.ipieces)
    })
  });

  if let Some(uu) = uu {
    let mut ig = igu.by_mut(Authorisation::authorise_any());
    let mut prepub = PrepareUpdatesBuffer::new(&mut ig, None);
    uu(&mut prepub);
    prepub.finish();
  }

  ok?
}

#[derive(Debug,Default)]
struct UpdateHandlerBulk {
  pieces: HashMap<PieceId, PieceUpdateOp<(),()>>,
  logs: bool,
  raw: Vec<PreparedUpdateEntry>,
}

#[derive(Debug)]
enum UpdateHandler {
  Bulk(UpdateHandlerBulk),
  Online,
}

impl UpdateHandler {
  fn from_how(how: MgmtGameUpdateMode) -> Self {
    use UpdateHandler::*;
    match how {
      MgmtGameUpdateMode::Bulk => Bulk(default()),
      MgmtGameUpdateMode::Online => Online,
    }
  }

  #[throws(SVGProcessingError)]
  fn accumulate(&mut self, g: &mut Instance,
                updates: ExecuteGameChangeUpdates) {
    let mut raw = updates.raw.unwrap_or_default();
    use UpdateHandler::*;
    match self {
      Bulk(bulk) => {
        for (upiece, uuop) in updates.pcs {
          use PieceUpdateOp::*;
          let oe = bulk.pieces.get(&upiece);
          let ne = match (oe, uuop) {
            ( None               , e        ) => Some( e          ),
            ( Some( Insert(()) ) , Delete() ) => None,
            ( Some( Insert(()) ) , _        ) => Some( Insert(()) ),
            ( Some( Delete(  ) ) , _        ) => Some( Modify(()) ),
            ( _                  , _        ) => Some( Modify(()) ),
          };
          trace_dbg!("accumulate", upiece, oe, uuop, ne);
          match ne {
            Some(ne) => { bulk.pieces.insert(upiece, ne); },
            None     => { bulk.pieces.remove(&upiece);    },
          };
        }
        bulk.logs |= updates.log.len() != 0;
        bulk.raw.append(&mut raw);
      }
      Online => {
        let estimate = updates.pcs.len() + updates.log.len();
        let mut buf = PrepareUpdatesBuffer::new(g, Some(estimate));
        for (upiece, uuop) in updates.pcs {
          buf.piece_update(upiece, &None, PUOs::Simple(uuop));
        }
        buf.log_updates(updates.log);
        buf.raw_updates(raw);
      }
    }
  }

  #[throws(SVGProcessingError)]
  fn complete(self, g: &mut InstanceGuard, who: &Html) {
    use UpdateHandler::*;
    match self {
      Bulk(bulk) => {
        let mut buf = PrepareUpdatesBuffer::new(g, None);
        for (upiece, uuop) in bulk.pieces {
          buf.piece_update(upiece, &None, PUOs::Simple(uuop));
        }

        if bulk.logs {
          buf.log_updates(vec![LogEntry {
            html: hformat!("{} (re)configured the game", who),
          }]);
        }

        buf.raw_updates(bulk.raw);
      }
      Online => {}
    }
  }
}

// ========== general implementation ==========

// ---------- core listener implementation ----------

impl CommandStream<'_> {
  #[throws(CSE)]
  pub fn mainloop(mut self) {
    loop {
      use MgmtChannelReadError::*;
      match {
        self.chan.read.inner_mut().set_timeout(Some(IDLE_TIMEOUT));
        let r = self.chan.read.read_withbulk::<MgmtCommand>();
        r
      } {
        Ok((cmd, mut rbulk)) => {
          execute_and_respond(&mut self.d, cmd, &mut rbulk,
                              &mut self.chan.write)?;
        },
        Err(EOF) => break,
        Err(IO(e)) if e.kind() == ErrorKind::TimedOut => {
          info!("{}: idle timeout reading command stream", &self.d.desc);
          self.write_error(ME::IdleTimeout)?;
          break;
        }
        Err(IO(e)) => Err(e).context("read command stream")?,
        Err(Parse(s)) => self.write_error(ME::CommandParseFailed(s))?,
      }
    }
  }

  #[throws(CSE)]
  pub fn write_error(&mut self, error: MgmtError) {
    let resp = MgmtResponse::Error { error };
    self.chan.write.write(&resp).context("swrite error to command stream")?;
  }
}

impl CommandStreamData<'_> {
  #[throws(MgmtError)]
  fn current_account(&self) -> &AccountSpecified {
    self.account.as_ref().ok_or(ME::SpecifyAccount)?
  }
}

impl CommandListener {
  #[throws(StartupError)]
  pub fn new() -> Self {
    let path = &config().command_socket;
    match fs::remove_file(path) {
      Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(()),
      r => r,
    }
    .with_context(|| format!("remove socket {:?} before we bind", &path))?;
    let listener = UnixListener::bind(path)
      .with_context(|| format!("bind command socket {:?}", &path))?;

    fs::set_permissions(path, unix::fs::PermissionsExt::from_mode(0o666))
      .with_context(|| format!("chmod sommand socket {:?}", &path))?;

    CommandListener { listener }
  }

  #[throws(StartupError)]
  pub fn spawn(mut self) {
    thread::spawn(move ||{
      loop {
        self.accept_one().unwrap_or_else(
          |e| error!("accept/spawn failed: {:?}", e)
        );
      }
    })
  }

  #[throws(CSE)]
  fn accept_one(&mut self) {
    let (conn, _caller) = self.listener.accept().context("accept")?;
    let mut desc = format!("{:>5}", conn.as_raw_fd());
    info!("command connection {}: accepted", &desc);
    thread::spawn(move||{
      match (||{
        let euid = conn.initial_peer_credentials()
          .map(|creds| creds.euid())
          .map_err(|e| ConnectionEuidDiscoverError(format!("{}", e)));

        #[derive(Error,Debug)]
        struct EuidLookupError(String);
        display_as_debug!{EuidLookupError}
        impl<E> From<&E> for EuidLookupError where E: Display {
          fn from(e: &E) -> Self { EuidLookupError(format!("{}", e)) }
        }

        let user_desc: String = (||{
          let euid = euid.clone()?;
          let pwent = Passwd::from_uid(euid);
          let show_username =
            pwent.map_or_else(|| format!("<euid {}>", euid),
                              |p| p.name);
          <Result<_,AE>>::Ok(show_username)
        })().unwrap_or_else(|e| format!("<error: {}>", e));
        write!(&mut desc, " user={}", user_desc)?;

        let chan = MgmtChannel::new(conn)?;

        let d = CommandStreamData {
          account: None, desc: &desc,
          authstate: AuthState::None { euid: euid.map(Uid::from_raw) },
        };
        let cs = CommandStream { chan, d };
        cs.mainloop()?;
        
        <Result<_,StartupError>>::Ok(())
      })() {
        Ok(()) => info!("command connection {}: disconnected", &desc),
        Err(e) => warn!("command connection {}: error: {:?}", &desc, e),
      }
    });
  }
}

//---------- authorisation ----------

#[derive(Debug,Error,Clone)]
#[error("connection euid lookup failed (at connection initiation): {0}")]
pub struct ConnectionEuidDiscoverError(String);

impl From<ConnectionEuidDiscoverError> for AuthorisationError {
  fn from(e: ConnectionEuidDiscoverError) -> AuthorisationError {
    AuthorisationError(format!("{}", e))
  }
}

impl CommandStreamData<'_> {
  #[throws(AuthorisationError)]
  fn authorised_uid(&self, wanted: Option<Uid>, xinfo: Option<&str>)
                    -> Authorisation<Uid> {
    let &client_euid = match &self.authstate {
      AuthState::Superuser { euid, .. } => euid,
      AuthState::None      { euid, .. } => euid,
      AuthState::Ssh { .. } => throw!(anyhow!(
        "{}: cannot authorise by uid as ,now in AuthState::Ssh")),
    }.as_ref().map_err(|e| e.clone())?;
    let server_uid = Uid::current();
    if client_euid.is_root() ||
       client_euid == server_uid ||
       Some(client_euid) == wanted
    {
      return Authorisation::authorised(&client_euid);
    }
    throw!(anyhow!("{}: euid mismatch: client={:?} server={:?} wanted={:?}{}",
                   &self.desc, client_euid, server_uid, wanted,
                   xinfo.unwrap_or("")));
  }

  fn map_auth_err(&self, ae: AuthorisationError) -> MgmtError {
    warn!("command connection {}: authorisation error: {}",
          self.desc, ae.0);
    MgmtError::AuthorisationError
  }
}

impl CommandStreamData<'_> {
  pub fn superuser(&self) -> Option<AuthorisationSuperuser> {
    match self.authstate {
      AuthState::Superuser { auth, .. } => Some(auth),
      _ => None
    }
  }
  pub fn is_superuser<T:Serialize>(&self) -> Option<Authorisation<T>> {
    self.superuser().map(Into::into)
  }

  #[throws(MgmtError)]
  pub fn check_acl_modify_player<'igr, 'ig: 'igr,
                                 P: Into<PermSet<TablePermission>>>(
    &self,
    ag: &AccountsGuard,
    ig: &'igr mut Unauthorised<InstanceGuard<'ig>, InstanceName>,
    player: PlayerId,
    p: P,
  ) -> (&'igr mut InstanceGuard<'ig>,
        Authorisation<InstanceName>)
  {
    let ipl_unauth = {
      let ig = ig.by_ref(Authorisation::authorise_any());
      ig.iplayers.byid(player)?
    };
    let how = PCH::InstanceOrOnlyAffectedAccount(ipl_unauth.ipl.acctid);
    let (ig, auth) = self.check_acl(ag, ig, how, p)?;
    (ig, auth)
  }

  #[throws(MgmtError)]
  pub fn check_acl_modify_pieces<'igr, 'ig: 'igr>(
    &self,
    ag: &AccountsGuard,
    ig: &'igr mut Unauthorised<InstanceGuard<'ig>, InstanceName>,
  ) -> (
    &'igr mut InstanceGuard<'ig>,
    ModifyingPieces,
    Authorisation<InstanceName>,
  ) {
    let p = &[TP::ChangePieces];
    let (ig, auth) = self.check_acl(ag, ig, PCH::Instance, p)?;
    let modperm = ig.modify_pieces();
    (ig, modperm, auth)
  }

  #[throws(MgmtError)]
  pub fn check_acl<'igr, 'ig: 'igr, P: Into<PermSet<TablePermission>>>(
    &self,
    ag: &AccountsGuard,
    ig: &'igr mut Unauthorised<InstanceGuard<'ig>, InstanceName>,
    how: PermissionCheckHow,
    p: P,
  ) -> (&'igr mut InstanceGuard<'ig>, Authorisation<InstanceName>) {
    #[throws(MgmtError)]
    fn get_auth(cs: &CommandStreamData,
                ag: &AccountsGuard,
                ig: &mut Unauthorised<InstanceGuard, InstanceName>,
                how: PermissionCheckHow,
                p: PermSet<TablePermission>)
                -> Authorisation<InstanceName>
    {
      if let Some(superuser) = cs.superuser() {
        return superuser.into();
      }

      let current_account = cs.current_account()?;
      let (_subject_record, subject_acctid) =
        ag.lookup(&current_account.notional_account)?;

      let subject_is = |object_acctid: AccountId|{
        if subject_acctid == object_acctid {
          let auth: Authorisation<InstanceName>
            = Authorisation::authorise_any();
          return Some(auth);
        }
        return None;
      };

      if let Some(auth) = match how {
        PCH::InstanceOrOnlyAffectedAccount(object_acctid) => {
          subject_is(object_acctid)
        },
        PCH::InstanceOrOnlyAffectedPlayer(object_player) => {
          if_chain!{
            if let Some(object_ipr) =
              ig.by_ref(Authorisation::authorise_any()).iplayers
              .get(object_player);
            then { subject_is(object_ipr.ipl.acctid) }
            else { None }
          }
        }
        PCH::Instance => None,
      } {
        return auth;
      }

      let auth = {
        let subject = &current_account.cooked;
        let (acl, owner) = {
          let ig = ig.by_ref(Authorisation::authorise_any());
          (&ig.acl, &ig.name.account)
        };
        let owner_account = owner.to_string();
        let owner_account = Some(owner_account.as_str());
        let eacl = EffectiveACL { owner_account, acl };
        eacl.check(subject, p)?
      };
      auth
    }

    let auth = get_auth(self, ag, ig, how, p.into())?;
    (ig.by_mut(auth), auth)
  }

  #[throws(MgmtError)]
  fn accountrecord_from_spec(&self, spec: Option<Box<dyn PlayerAccessSpec>>)
                             -> Option<AccessRecord> {
    spec
      .map(|spec| AccessRecord::from_spec(spec, self.superuser()))
      .transpose()?
  }
}

#[throws(MgmtError)]
fn authorise_for_account(
  cs: &CommandStreamData,
  _accounts: &AccountsGuard,
  wanted: &AccountName,
) -> Authorisation<AccountName> {
  if let Some(y) = cs.is_superuser() {
    return y;
  }

  let currently = &cs.current_account()?;
  if &currently.notional_account != wanted {
    throw!(MgmtError::AuthorisationError)
  }
  currently.auth
}

#[throws(MgmtError)]
fn authorise_by_account(cs: &CommandStreamData, ag: &AccountsGuard,
                        wanted: &InstanceName)
                        -> Authorisation<InstanceName> {
  let current = cs.current_account()?;
  ag.check(&current.notional_account)?;

  if let Some(y) = cs.superuser() {
    return y.therefore_ok();
  }

  if &current.notional_account == &wanted.account {
    current.auth.map(
      // Not executed, exists as a proof.
      // we need this Box::leak because map wants us to return a ref
      // borrowing from the incoming subject, which would imply narrowing
      // of scope and of course we are widening scope here.  We're
      // saying that the account can access all its games.
      |account: &AccountName| Box::leak(Box::new(InstanceName {
        account: account.clone(),
        game: wanted.game.clone(),
      }))
    )
  } else {
    throw!(ME::AuthorisationError);
  }    
}

#[throws(MgmtError)]
fn authorise_scope_direct(cs: &CommandStreamData, ag: &AccountsGuard,
                          wanted: &AccountScope)
                          -> Authorisation<AccountScope> {
  // Usually, use authorise_by_account
  do_authorise_scope(cs, ag, wanted)
    .map_err(|e| cs.map_auth_err(e))?
}

#[throws(AuthorisationError)]
fn do_authorise_scope(cs: &CommandStreamData, ag: &AccountsGuard,
                      wanted: &AccountScope)
                      -> Authorisation<AccountScope> {
  match &cs.authstate {
    &AuthState::Superuser { auth, .. } => return auth.into(),

    &AuthState::Ssh { ref key, auth } => {
      let wanted_base_account = AccountName {
        scope: wanted.clone(),
        subaccount: default(),
      };
      if_chain!{
        if let Ok::<_,AccountNotFound>
          ((record, _acctid)) = ag.lookup(&wanted_base_account);
        if let
          Some(auth) = record.ssh_keys.check(ag, &key, auth);
        then { return Ok(auth) }
        else { throw!(AuthorisationError("ssh key not authorised".into())); }
      }
    },

    _ => {},
  }

  match &wanted {

    AccountScope::Server => {
      let y: Authorisation<Uid> = {
        cs.authorised_uid(None,None)?
      };
      y.therefore_ok()
    }

    AccountScope::Ssh{..} => {
      // Should have been dealt with earlier, when we checked authstate.
      throw!(AuthorisationError(
        "account must be accessed via ssh proxy"
          .into()));
    }

    AccountScope::Unix { user: wanted } => {
      struct InUserList;

      let y: Authorisation<(Passwd,Uid,InUserList)> = {

        struct AuthorisedIf { authorised_for: Option<Uid> }

        const SERVER_ONLY: (AuthorisedIf, Authorisation<InUserList>) = (
          AuthorisedIf { authorised_for: None },
          Authorisation::authorised(&InUserList),
        );

        let pwent = Passwd::from_name(&wanted)
          .map_err(
            |e| anyhow!("looking up requested username {:?}: {:?}",
                        &wanted, &e)
          )?
          .ok_or_else(
            || AuthorisationError(format!(
              "requested username {:?} not found", &wanted
            ))
          )?;
        let pwent_ok = Authorisation::authorised(&pwent);

        let ((uid, in_userlist_ok), xinfo) = (||{ <Result<_,AE>>::Ok({
          let allowed = BufReader::new(match File::open(USERLIST) {
            Err(e) if e.kind() == ErrorKind::NotFound => {
              return Ok((
                SERVER_ONLY,
                Some(format!(" user list {} does not exist", USERLIST))
              ))
            },
            r => r,            
          }?);
          
          allowed
            .lines()
            .filter_map(|le| match le {
              Ok(l) if l.trim() == wanted => Some(
                Ok((
                  (AuthorisedIf{ authorised_for: Some(
                    Uid::from_raw(pwent.uid)
                  )},
                   Authorisation::authorised(&InUserList),
                  ),
                  None
                ))
              ),
              Ok(_) => None,
              Err(e) => Some(<Result<_,AE>>::Err(e.into())),
            })
            .next()
            .unwrap_or_else(
              || Ok((
                SERVER_ONLY,
                Some(format!(" requested username {:?} not in {}",
                             &wanted, USERLIST)),
              ))
            )?
        })})()?;

        let info = xinfo.as_deref();
        let uid_ok = cs.authorised_uid(uid.authorised_for, info)?;
      
        (pwent_ok, uid_ok, in_userlist_ok).combine()
      };
      y.therefore_ok()
    },

  }
}
