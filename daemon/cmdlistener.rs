// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// management API implementation

use otter::imports::*;

use super::*;
use otter::commands::*;

use authproofs::*;

// ---------- newtypes, type aliases, basic definitions ----------

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

const DEFAULT_POS_START: Pos = PosC([20,20]);
const DEFAULT_POS_DELTA: Pos = PosC([5,5]);

pub struct CommandListener {
  listener: UnixListener,
}

struct CommandStream<'d> {
  euid: Result<Uid, ConnectionEuidDiscoverEerror>,
  desc: &'d str,
  account: Option<AccountSpecified>,
  superuser: Option<AuthorisationSuperuser>,
  chan: MgmtChannel,
}

#[derive(Debug,Clone)]
struct AccountSpecified {
  notional_account: AccountName, // might not exist
  cooked: String, // account.to_string()
  auth: Authorisation<AccountName>,
}

enum PermissionCheckHow {
  Instance,
  InstanceOrOnlyAffectedAccount(AccountId),
  InstanceOrOnlyAffectedPlayer(PlayerId),
}

type PCH = PermissionCheckHow;

// ========== management API ==========

// ---------- management command implementations

#[throws(ME)]
fn execute(cs: &mut CommandStream, cmd: MgmtCommand) -> MgmtResponse {
  match cmd {
    MC::Noop => Fine,

    MC::SetSuperuser(enable) => {
      if !enable {
        cs.superuser = None;
      } else {
        let auth = authorise_scope_direct(cs, &AccountScope::Server)?;
        cs.superuser = Some(auth.therefore_ok());
      }
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
        timezone: timezone.unwrap_or_default(),
        layout,
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
      let auth = authorise_scope_direct(cs, &wanted_account.scope)?;
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

    MC::CreateGame { game, insns } => {
      let mut ag = AccountsGuard::lock();
      let mut games = games_lock();
      let auth = authorise_by_account(cs, &ag, &game)?;

      let gs = otter::gamestate::GameState {
        table_colour: Html::lit("green"),
        table_size: DEFAULT_TABLE_SIZE,
        pieces: default(),
        players: default(),
        log: default(),
        gen: Generation(0),
        max_z: default(),
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

    MC::ListGames { all } => {
      let (scope, auth) = if all == Some(true) {
        let auth = authorise_scope_direct(cs, &AS::Server)?;
        (None, auth.therefore_ok())
      } else {
        let AccountSpecified { notional_account, auth, .. } =
          cs.account.as_ref().ok_or(ME::SpecifyAccount)?;
        (Some(notional_account), *auth)
      };
      let mut games = Instance::list_names(scope, auth);
      games.sort_unstable();
      MR::GamesList(games)
    }

    MC::AlterGame { game, insns, how } => {
      let mut ag = AccountsGuard::lock();
      let gref = Instance::lookup_by_name_unauth(&game)?;
      let mut g = gref.lock()?;
      execute_for_game(cs, &mut ag, &mut g, insns, how)?
    }

    MC::DestroyGame { game } => {
      let mut ag = AccountsGuard::lock();
      let mut games = games_lock();
      let auth = authorise_by_account(cs, &mut ag, &game)?;
      let gref = Instance::lookup_by_name_locked(&games, &game, auth)?;
      let ig = gref.lock_even_poisoned();
      Instance::destroy_game(&mut games, ig, auth)?;
      Fine
    }

    MC::LibraryListByGlob { glob: spec } => {
      let lib = shapelib::libs_lookup(&spec.lib)?;
      let results = lib.list_glob(&spec.item)?;
      MR::LibraryItems(results)
    }

    MC::LoadFakeRng(ents) => {
      let superuser = cs.superuser
        .ok_or(ME::SuperuserAuthorisationRequired)?;
      config().game_rng.set_fake(ents, superuser)?;
      Fine
    }
  }
}

// ---------- game command implementations ----------

type ExecuteGameInsnResults<'igr, 'ig> = (
  ExecuteGameChangeUpdates,
  MgmtGameResponse,
  Option<Void>,
  &'igr mut InstanceGuard<'ig>,
);

//#[throws(ME)]
fn execute_game_insn<'cs, 'igr, 'ig: 'igr>(
  cs: &'cs CommandStream,
  ag: &'_ mut AccountsGuard,
  ig: &'igr mut Unauthorised<InstanceGuard<'ig>, InstanceName>,
  update: MgmtGameInstruction,
  who: &Html,
  _to_permute: &mut ToPermute,
)
  -> Result<ExecuteGameInsnResults<'igr, 'ig> ,ME>
{
  type U = ExecuteGameChangeUpdates;
  use MgmtGameResponse::Fine;

  fn tz_from_str(s: &str) -> Timezone {
    match Timezone::from_str(s) {
      Ok(tz) => tz,
      Err(x) => match x { },
    }
  }

  #[throws(MgmtError)]
  fn readonly<'igr, 'ig: 'igr, 'cs,
              F: FnOnce(&InstanceGuard) -> Result<MgmtGameResponse,ME>,
              P: Into<PermSet<TablePermission>>>
  
    (
      cs: &'cs CommandStream,
      ag: &AccountsGuard,
      ig: &'igr mut Unauthorised<InstanceGuard<'ig>, InstanceName>,
      p: P,
      f: F
    ) -> ExecuteGameInsnResults<'igr, 'ig>
  {
    let (ig, _) = cs.check_acl(ag, ig, PCH::Instance, p)?;
    let resp = f(ig)?;
    (U{ pcs: vec![], log: vec![], raw: None }, resp, None, ig)
  }

  #[throws(MgmtError)]
  fn update_links<'igr, 'ig: 'igr, 'cs,
               F: FnOnce(&mut Arc<LinksTable>) -> Result<Html,ME>>
    (
      cs: &'cs CommandStream,
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
     Fine, None, ig)
  }

  impl<'cs> CommandStream<'cs> {
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
    MGI::Noop { } => readonly(cs,ag,ig, &[], |_| Ok(Fine))?,

    MGI::SetTableSize(size) => {
      let ig = cs.check_acl(ag, ig, PCH::Instance, &[TP::ChangePieces])?.0;
      ig.gs.table_size = size;
      (U{ pcs: vec![],
          log: vec![ LogEntry {
            html: Html(format!("{} resized the table to {}x{}",
                               &who.0, size.0[0], size.0[1])),
          }],
          raw: Some(vec![ PreparedUpdateEntry::SetTableSize(size) ]) },
       Fine, None, ig)
    }

    MGI::SetTableColour(colour) => {
      let ig = cs.check_acl(ag, ig, PCH::Instance, &[TP::ChangePieces])?.0;
      let colour: Colour = (&colour).try_into()?;
      ig.gs.table_colour = colour.clone();
      (U{ pcs: vec![],
          log: vec![ LogEntry {
            html: Html(format!("{} recoloured the tabletop to {}",
                               &who.0, &colour.0)),
          }],
          raw: Some(vec![ PreparedUpdateEntry::SetTableColour(colour) ]) },
       Fine, None, ig)
    }

    MGI::JoinGame {
      details: MgmtPlayerDetails { nick },
    } => {
      let account = &cs.current_account()?.notional_account;
      let (arecord, acctid) = ag.lookup(account)?;
      let (ig, auth) = cs.check_acl(ag, ig, PCH::Instance, &[TP::Play])?;
      let nick = nick.ok_or(ME::MustSpecifyNick)?;
      let logentry = LogEntry {
        html: Html(format!("{} [{}] joined the game", &nick, &account)),
      };
      let timezone = &arecord.timezone;
      let tz = tz_from_str(&timezone);
      let gpl = GPlayer {
        nick: nick.to_string(),
        layout: arecord.layout,
        idmap: default(),
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
       None, ig)
    },

    MGI::Synch => {
      let (mut ig, _) = cs.check_acl(&ag, ig, PCH::Instance, &[TP::Play])?;
      let mut buf = PrepareUpdatesBuffer::new(&mut ig, None, None);
      let gen = buf.gen();
      drop(buf); // does update
      (U{ pcs: vec![], // we handled the update ourselves,
          log: vec![], // return no update info
          raw: None },
       MGR::Synch(gen),
       None, ig)
    },

    MGI::ListPieces => readonly(cs,ag,ig, &[TP::ViewNotSecret], |ig|{
      let ioccults = &ig.ioccults;
      let pieces = ig.gs.pieces.iter().filter_map(
        |(piece,gpc)| (|| Ok::<_,MgmtError>(if_chain!{
          let &GPiece { pos, face, .. } = gpc;
          if let Some(ipc) = ig.ipieces.get(piece);
          let visible = if ! piece_at_all_occulted(gpc) {
            // todo: something more sophisticated would be nice
            let pri = PieceRenderInstructions::new_visible(
              // visible id is internal one here
              VisiblePieceId(piece.data())
            );
            let bbox = ipc.p.bbox_approx()?;
            let desc_html = pri.describe(ioccults, gpc, ipc);
            Some(MgmtGamePieceVisibleInfo {
              pos, face, desc_html, bbox
            })
          } else {
            None
          };
          let itemname = ipc.p.itemname().to_string();
          then {
            Some(MgmtGamePieceInfo {
              piece, itemname,
              visible
            })
          } else {
            None
          }
        }))().transpose()
      ).collect::<Result<Vec<_>,_>>()?;
      Ok(MGR::Pieces(pieces))
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
          html: Html(format!("{} changed {}'s nick to {}",
                             &who.0,
                             htmlescape::encode_minimal(&gpl.nick),
                             htmlescape::encode_minimal(&new_nick))),
        });
        gpl.nick = new_nick;
      }
      (U{ log,
          pcs: vec![],
          raw: None},
       Fine, None, ig)
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
          new_links[k] = Some(url.into_string());
        }
        let new_links = Arc::new(new_links);
        *ig_links = new_links.clone();
        Ok(Html(
          format!("{} set the links to off-server game resources",
                  &who.0)
        ))
      })?
    }

    MGI::SetLink { kind, url } =>  {
      update_links(cs,ag,ig, |ig_links|{
        let mut new_links: LinksTable = (**ig_links).clone();
        let url: Url = (&url).try_into()?;
        let show: Html = (kind, url.as_str()).into();
        new_links[kind] = Some(url.into_string());
        let new_links = Arc::new(new_links);
        *ig_links = new_links.clone();
        Ok(Html(
          format!("{} set the link {}",
                  &who.0, &show.0)
        ))
      })?
    }

    MGI::RemoveLink { kind } =>  {
      update_links(cs,ag,ig, |ig_links|{
        let mut new_links: LinksTable = (**ig_links).clone();
        new_links[kind] = None;
        let new_links = Arc::new(new_links);
        *ig_links = new_links.clone();
        Ok(Html(
          format!("{} removed the link {}",
                  &who.0, &kind)
        ))
      })?
    }

    MGI::ResetPlayerAccess(player) => {
      let (ig, auth) = cs.check_acl_manip_player_access
        (ag, ig, player, TP::ResetOthersAccess)?;

      let token = ig.player_access_reset(ag, player, auth)?;
      (U{ pcs: vec![],
          log: vec![],
          raw: None },
       MGR::PlayerAccessToken(token), None, ig)
    }

    MGI::RedeliverPlayerAccess(player) => {
      let (ig, auth) = cs.check_acl_manip_player_access
        (ag, ig, player, TP::RedeliverOthersAccess)?;

      let token = ig.player_access_redeliver(ag, player, auth)?;
      (U{ pcs: vec![],
          log: vec![],
          raw: None },
       MGR::PlayerAccessToken(token), None, ig)
    },

    MGI::LeaveGame(player) => {
      let account = &cs.current_account()?.notional_account;
      let (ig, _auth) = cs.check_acl_manip_player_access
        (ag, ig, player, TP::ModifyOtherPlayer)?;

      let got = ig.players_remove(&[player].iter().cloned().collect())?;

      let (gpl, ipl, update) = got.into_iter().next()
        .ok_or(PlayerNotFound)?;

      let html = Html(
        format!("{} [{}] left the game [{}]"
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
      );

      (U{ pcs: vec![],
          log: vec![ LogEntry { html }],
          raw: Some(vec![ update ]) },
       Fine, None, ig)
    },

    MGI::DeletePiece(piece) => {
      let (ig_g, modperm, _) = cs.check_acl_modify_pieces(ag, ig)?;
      let ig = &mut **ig_g;
      let ipc = ig.ipieces.as_mut(modperm)
        .remove(piece).ok_or(ME::PieceNotFound)?;
      let ioccults = &ig.ioccults;
      let gs = &mut ig.gs;
      let gpc = gs.pieces.as_mut(modperm).remove(piece);
      let desc_html = if let Some(gpc) = &gpc {
        let pri = PieceRenderInstructions::new_visible(default());
        pri.describe(ioccults, gpc, &ipc)
      } else {
        Html::lit("<piece partially missing from game state!>")
      };
      if let Some(gpc) = gpc { ipc.p.delete_hook(&gpc, gs); }
      if let Some(occilk) = ipc.occilk { ig.ioccults.ilks.dispose(occilk); }
      (U{ pcs: vec![(piece, PieceUpdateOp::Delete())],
          log: vec![ LogEntry {
            html: Html(format!("A piece {} was removed from the game",
                          desc_html.0)),
          }],
          raw: None },
       Fine, None, ig_g)
    },

    MGI::AddPieces(PiecesSpec{ pos,posd,count,face,pinned,angle,info }) => {
      let (ig_g, modperm, _) = cs.check_acl_modify_pieces(ag, ig)?;
      let ig = &mut **ig_g;
      let gs = &mut ig.gs;
      let implicit: u32 = info.count()
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
      let mut z = gs.max_z.clone_mut();
      for piece_i in count {
        let PieceSpecLoaded { p, occultable } = info.load(piece_i as usize)?;
        let ilks = &mut ig.ioccults.ilks;
        let occilk = occultable.map(|(ilkname, p_occ)| {
          ilks.insert(ilkname, OccultIlkData { p_occ })
        });
        let face = face.unwrap_or_default();
        if p.nfaces() <= face.into() {
          throw!(SpecError::FaceNotFound);
        }
        let gpc = GPiece {
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
        };
        gpc.pos.clamped(gs.table_size).map_err(|_| SpecError::PosOffTable)?;
        if gpc.zlevel.z > gs.max_z { gs.max_z = gpc.zlevel.z.clone() }
        let piece = gs.pieces.as_mut(modperm).insert(gpc);
        ig.ipieces.as_mut(modperm).insert(piece, IPiece { p, occilk });
        updates.push((piece, PieceUpdateOp::Insert(())));
        pos = (pos + posd)?;
      }

      (U{ pcs: updates,
          log: vec![ LogEntry {
            html: Html(format!("{} added {} pieces", &who.0, count_len)),
          }],
          raw: None },
       Fine, None, ig_g)
    },

    MGI::ClearLog => {
      let (ig, _) = cs.check_acl(&ag, ig, PCH::Instance, &[TP::Super])?;
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
      (U{ pcs: vec![ ],
          log: vec![ LogEntry {
            html: Html(format!("{} cleared the log", &who.0)),
          } ],
          raw: None },
       Fine, None, ig)
    },

    MGI::SetACL { acl } => {
      let (ig, _) = cs.check_acl(&ag, ig, PCH::Instance, &[TP::Super])?;
      ig.acl = acl.into();
      let mut log = vec![ LogEntry {
        html: Html(format!("{} set the table access control list",
                           &who.0)),
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
            html: Html(format!("{} removed a player {}", &who.0, &show)),
          });
          updates.push(update);
        }

        updates
      }

      let updates = remove_old_players(&ag, ig, who, &mut log)?;

      (U{ pcs: vec![ ],
          log,
          raw: Some(updates) },
       Fine, None, ig)
    },
  };
  Ok(y)
}

// ---------- how to execute game commands & handle their updates ----------

#[throws(ME)]
fn execute_for_game<'cs, 'igr, 'ig: 'igr>(
  cs: &'cs CommandStream,
  ag: &mut AccountsGuard,
  igu: &'igr mut Unauthorised<InstanceGuard<'ig>, InstanceName>,
  mut insns: Vec<MgmtGameInstruction>,
  how: MgmtGameUpdateMode) -> MgmtResponse
{
  ToPermute::with(|mut to_permute| {
    let r = (||{

  let mut uh = UpdateHandler::from_how(how);
  let mut responses = Vec::with_capacity(insns.len());
  let mut auth = None;
  let who = if_chain! {
    let account = &cs.current_account()?.notional_account;
    let ig = igu.by_ref(Authorisation::authorise_any());
    if let Ok((_, acctid)) = ag.lookup(account);
    if let Some((player,_)) = ig.iplayers.iter()
      .filter(|(_,ipr)| ipr.ipl.acctid == acctid)
      .next();
    if let Some(gpl) = ig.gs.players.get(player);
    then { Html(format!("{} [{}]",
                        &htmlescape::encode_minimal(&gpl.nick),
                        &account)) }
    else { Html(format!("[{}]",
                        &account)) }
  };
  let res = (||{
    for insn in insns.drain(0..) {
      let (updates, resp, for_prepub, ig) =
        execute_game_insn(cs, ag, igu, insn, &who, &mut to_permute)?;
      uh.accumulate(ig, updates)?;
      responses.push(resp);
      match for_prepub { None => (), Some(x) => match x { }};
      auth = Some(Authorisation::authorised(&*ig.name));
    }
    if let Some(auth) = auth { uh.complete(igu.by_mut(auth), &who)?; }
    Ok(None)
  })();
  if let Some(auth) = auth {
    igu.by_mut(auth).save_game_now()?;
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
  })?
}

#[derive(Debug,Default)]
struct UpdateHandlerBulk {
  pieces: slotmap::SparseSecondaryMap<PieceId, PieceUpdateOp<(),()>>,
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
          let ne = match (bulk.pieces.get(upiece), uuop) {
            ( None               , e        ) => Some( e          ),
            ( Some( Insert(()) ) , Delete() ) => None,
            ( Some( Insert(()) ) , _        ) => Some( Insert(()) ),
            ( Some( Delete(  ) ) , _        ) => Some( Modify(()) ),
            ( _                  , _        ) => Some( Modify(()) ),
          };
          match ne {
            Some(ne) => { bulk.pieces.insert(upiece, ne); },
            None     => { bulk.pieces.remove(upiece);     },
          };
        }
        bulk.logs |= updates.log.len() != 0;
        bulk.raw.append(&mut raw);
      }
      Online => {
        let estimate = updates.pcs.len() + updates.log.len();
        let mut buf = PrepareUpdatesBuffer::new(g, None, Some(estimate));
        for (upiece, uuop) in updates.pcs {
          buf.piece_update(upiece, PUOs::Simple(uuop));
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
        let mut buf = PrepareUpdatesBuffer::new(g, None, None);
        for (upiece, uuop) in bulk.pieces {
          buf.piece_update(upiece, PUOs::Simple(uuop));
        }

        if bulk.logs {
          buf.log_updates(vec![LogEntry {
            html: Html(format!("{} (re)configured the game", &who.0)),
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
      let resp = match self.chan.read::<MgmtCommand>() {
        Ok(cmd) => {
          let mut cmd_s = log_enabled!(log::Level::Info)
            .as_some_from(|| format!("{:?}", &cmd))
            .unwrap_or_default();
          const MAX: usize = 200;
          if cmd_s.len() > MAX-3 {
            cmd_s.truncate(MAX-3);
            cmd_s += "..";
          }
          match execute(&mut self, cmd) {
            Ok(resp) => {
              info!("command connection {}: executed {}",
                    &self.desc, cmd_s);
              resp
            }
            Err(error) => {
              info!("command connection {}: error {:?} from {}",
                    &self.desc, &error, cmd_s);
              MgmtResponse::Error { error }
            }
          }
        }
        Err(EOF) => break,
        Err(IO(e)) => Err(e).context("read command stream")?,
        Err(Parse(s)) => MgmtResponse::Error { error: ME::ParseFailed(s) },
      };
      self.chan.write(&resp).context("swrite command stream")?;
    }
  }

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
          .map_err(|e| ConnectionEuidDiscoverEerror(format!("{}", e)));

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

        let cs = CommandStream {
          account: None, desc: &desc,
          chan, euid: euid.map(Uid::from_raw),
          superuser: None,
        };
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
pub struct ConnectionEuidDiscoverEerror(String);

impl From<ConnectionEuidDiscoverEerror> for AuthorisationError {
  fn from(e: ConnectionEuidDiscoverEerror) -> AuthorisationError {
    AuthorisationError(format!("{}", e))
  }
}

impl CommandStream<'_> {
  #[throws(AuthorisationError)]
  fn authorised_uid(&self, wanted: Option<Uid>, xinfo: Option<&str>)
                    -> Authorisation<Uid> {
    let client_euid = *self.euid.as_ref().map_err(|e| e.clone())?;
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

impl CommandStream<'_> {
  pub fn is_superuser<T:Serialize>(&self) -> Option<Authorisation<T>> {
    self.superuser.map(Into::into)
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
    fn get_auth(cs: &CommandStream,
                ag: &AccountsGuard,
                ig: &mut Unauthorised<InstanceGuard, InstanceName>,
                how: PermissionCheckHow,
                p: PermSet<TablePermission>)
                -> Authorisation<InstanceName>
    {
      if let Some(superuser) = cs.superuser {
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
      .map(|spec| AccessRecord::from_spec(spec, self.superuser))
      .transpose()?
  }
}

#[throws(MgmtError)]
fn authorise_for_account(
  cs: &CommandStream,
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
fn authorise_by_account(cs: &CommandStream, ag: &AccountsGuard,
                        wanted: &InstanceName)
                        -> Authorisation<InstanceName> {
  let account = &wanted.account;
  ag.check(account)?;
  authorise_for_account(cs, ag, account)?
    .therefore_ok()
}

#[throws(MgmtError)]
fn authorise_scope_direct(cs: &CommandStream, wanted: &AccountScope)
                          -> Authorisation<AccountScope> {
  // Usually, use authorise_by_account
  do_authorise_scope(cs, wanted)
    .map_err(|e| cs.map_auth_err(e))?
}

#[throws(AuthorisationError)]
fn do_authorise_scope(cs: &CommandStream, wanted: &AccountScope)
                   -> Authorisation<AccountScope> {
  if let Some(y) = cs.is_superuser() { return y }

  match &wanted {

    AccountScope::Server => {
      let y: Authorisation<Uid> = {
        cs.authorised_uid(None,None)?
      };
      y.therefore_ok()
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
