// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use super::*;

//---------- upload-bundle ----------

pub const PROGFRAC_PREP:   f32 = 0.20;
pub const PROGFRAC_HASH:   f32 = 0.10;
pub const PROGFRAC_UPLOAD: f32 = 0.70;

#[derive(Debug)]
pub struct BundleForUpload {
  pub file: String,
  pub f: BufReader<File>,
  pub size: usize,
  pub hash: bundles::Hash,
  pub kind: bundles::Kind,
}

impl BundleForUpload {
  #[throws(AE)]
  pub fn prepare(file: String, progress: &mut termprogress::Nest) -> Self {
    progress.start_phase(PROGFRAC_PREP, None, "inspecting".into());

    let mut walk = WalkDir::new(&file)
      .same_file_system(true)
      .follow_links(true)
      .sort_by_file_name()
      .into_iter()
      .peekable();

    let peek = walk.peek().ok_or_else(|| anyhow!("unable to inspect"))?;
    if_let!{
      Ok(peek) = peek;
      else Err(
        walk.next().unwrap().unwrap_err()
      ).context("inspect")?
    };
    if ! peek.file_type().is_dir() {
      let f = File::open(&file).context("open")?;
      Self::prepare_open_file(&file, progress, f)?
    } else {
      let entries = walk.collect::<Result<Vec<_>,_>>()?;
      Self::prepare_from_dir(&file, progress, entries)?
    }
  }

  #[throws(AE)]
  pub fn prepare_open_file(file: &str, progress: &mut termprogress::Nest,
                           f: File) -> Self {
    progress.start_phase(PROGFRAC_HASH, None, "hashing".into());

    let size = f
      .metadata().context("fstat bundle file")?
      .len()
      .try_into().map_err(|_| anyhow!("bundle file far too large"))?;
    let mut f = BufReader::new(f);
    let hash = bundles::DigestWrite::of(&mut f)
      .context("read bundle file (for hash)")?;
    let hash = bundles::Hash(hash.into());
    let kind = bundles::Kind::only();
    f.rewind().context("rewind bundle file")?;
    BundleForUpload { file: file.to_owned(), f, size, hash, kind }
  }

  #[throws(AE)]
  pub fn upload(self, ma: &MainOpts, chan: &mut MgmtChannelForGame,
            progress: &mut dyn termprogress::Reporter)
            -> bundles::Id {
    let BundleForUpload { mut f, size, hash, kind,.. } = self;
    let cmd = MC::UploadBundle {
      size,
      game: ma.instance(),
      hash, kind,
      progress: MgmtChannel::PROGRESS,
    };
    let resp = chan.cmd_withbulk(&cmd, &mut f, &mut io::sink(),
                                 &mut *progress)?;
    if_let!{ MR::Bundle { bundle } = resp;
             else throw!(anyhow!("unexpected {:?}", &resp)) };
    progress.clear();
    bundle
  }

  #[throws(AE)]
  pub fn prepare_from_dir(dir: &str, progress: &mut termprogress::Nest,
                          entries: Vec<walkdir::DirEntry>) -> Self {
    let zipfile = tempfile::tempfile().context("create tmp zipfile")?;
    let zipfile = BufWriter::new(zipfile);
    let mut zipfile = zipfile::ZipWriter::new(zipfile);

    let n_entries = entries.len();
    for (entry_i, ent) in entries.into_iter().enumerate() {
      if ent.file_type().is_dir() { continue }

      let tail = {
        let comps = ent.path().components().rev()
          .take(ent.depth())
          .collect_vec();
        comps.into_iter().rev().collect::<PathBuf>()
      };

      let tail = tail.to_str()
        .ok_or_else(|| anyhow!("non-UTF-8 path in bundle {:?}", tail))?;

      progress.report(&ProgressInfo {
        phase: progress::Count {
          desc: "building zip".into(),
          value: progress::Value::Exact { i: 0, n: 1 },
        },
        item: progress::Count {
          desc: tail.into(),
          value: progress::Value::Exact { i: entry_i, n: n_entries },
        }
      });

      (||{
        let mut f = File::open(ent.path())
          .context("start to read")?;

        let metadata = f.metadata().context("fstat")?;

        let perms = metadata.permissions().mode() & 0o700;
        let perms = perms * 0o111;

        // Can't copy timestamp due to lack of useful support in
        // the `zip` crate.

        let options = zipfile::write::FileOptions::default()
          .compression_level(Some(1))
          .unix_permissions(perms);

        let () = zipfile.start_file(tail, options) // yoy no typestate!
          .context("start to write")?;

        io::copy(&mut f, &mut zipfile)
          .context("copy data")?;

        Ok::<_,AE>(())
      })()
        .with_context(|| format!("{:?}", tail))
        .context("add member")?;
    }

    let zipfile = zipfile.finish().context("finish zipfile")?;
    let mut zipfile = zipfile.into_inner()
      .map_err(|e| e.into_error()).context("flush zipfile")?;
    zipfile.rewind().context("rewind zipfile")?;

    Self::prepare_open_file(dir,progress, zipfile)?
  }
}

mod upload_bundle {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    bundle_file: String,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.bundle_file).required()
      .add_argument("BUNDLE",Store,"bundle file");
    ap
  }

  #[throws(AE)]
  fn call(SCCA{ mut out, ma, args,.. }:SCCA) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = ma.access_game()?;
    let progress = ma.progressbar()?;
    let mut progress = termprogress::Nest::new(progress);

    let for_upload = BundleForUpload::prepare(args.bundle_file,
                                              &mut progress)?;
    let bundle = for_upload.upload(&ma, &mut chan, &mut progress)?;
    writeln!(out, "{}", bundle)?;
  }

  inventory_subcmd!{
    "upload-bundle",
    "Upload a bundle",
  }
}

//---------- list-bundles ----------

mod list_bundles {
  use super::*;

  type Args = NoArgs;

  #[throws(AE)]
  fn call(SCCA{ mut out, ma, args,.. }:SCCA) {
    let _args = parse_args::<Args,_>(args, &noargs, &ok_id, None);
    let mut chan = ma.access_game()?;
    let resp = chan.cmd(&MC::ListBundles {
      game: ma.instance(),
    })?;
    if_let!{ MR::Bundles { bundles } = resp;
             else throw!(anyhow!("unexpected {:?}", &resp)) };
    for (id, state) in bundles {
      writeln!(out, "{} {}", id, &state)?;
    }
  }

  inventory_subcmd!{
    "list-bundles",
    "List bundles",
  }
}

//---------- download-bundle ----------

mod download_bundle {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    index: bundles::Index,
    output: Option<PathBuf>,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.index).required()
      .add_argument("INDEX",Store,"bundle number");
    ap.refer(&mut sa.output).metavar("OUTPUT")
      .add_option(&["-o","--output"],StoreOption,
                  "write output to OUTPUT (rather than NNNNN.zip");
    ap
  }

  #[throws(AE)]
  fn call(SCCA{ out, ma, args,.. }:SCCA) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = ma.access_game()?;
    let kind = bundles::Kind::only();
    let id = bundles::Id { kind, index: args.index };
    let path = args.output.unwrap_or_else(|| id.to_string().into());

    let (f, path_tmp): (Box<dyn Write>, _) =
      if path.as_os_str().as_bytes() == b"-"
    {
      drop(out);
      (Box::new(RawStdout::new()), None)
    } else {
      let tmp = {
        let mut w = path.as_os_str().to_owned();
        w.push(".tmp");
        PathBuf::from(w)
      };
      let f = fs::File::create(&tmp)
        .with_context(|| tmp.to_debug()).context("create temporary")?;
      (Box::new(f), Some((path, tmp)))
    };
    let mut f = BufWriter::new(f);
    let cmd = MC::DownloadBundle {
      game: ma.instance(),
      id,
    };
    chan.cmd_withbulk(&cmd, &mut io::empty(), &mut f,
                      &mut termprogress::Null)
      .context("download bundle")?;
    f.flush().context("flush bundle file")?;
    if let Some((path, tmp)) = path_tmp {
      fs::rename(&tmp,&path)
        .with_context(|| path.to_debug()).context("rename after download")?;
    }
  }

  inventory_subcmd!{
    "download-bundle",
    "download bundle",
  }
}
