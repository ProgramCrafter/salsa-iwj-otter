Developing
==========

Otter is mostly written in Rust.

The web UI frontend is written in Typescript, with a small amount of
Rust support code delivered via WebAssembly.

The HTML and SVG skeleton is in a templating language called Tera,
which is a jinja-like but from the Rust community.


Ad-hoc testing, playing about, etc.
-----------------------------------

In one shell:

```
     target/debug/daemon-otter server-test.toml
```

The server does not daemonise, and the default config there makes it
quite verbose.  So, in another shell:

```
    target/debug/otter                                               \
        --account server: --config server-test.toml --spec-dir=specs \
        --game test server::test reset --reset-table demo

    target/debug/otter                                               \
        --account server: --config server-test.toml --spec-dir=specs \
        --game server::test join-game
```

The URL printed can then be visited in a local browser.


Resetting/restoring things after tests, updating server, etc.
-------------------------------------------------------------

After the server is updated, you can just `^C` and restart it.  Games
are constantly saved (although there is an up-to-1s lag on the most
frequently updated game state).

If you want to clear out the server state, delete the files `[ag]-*`
and `accounts`.  NB that you should do this with the server not
running, because the server has most of that information in memory and
will like to write it out again.

If you update Typescript (JS code) you will need to rerun `make` to
rebuild the JS output.

Apart from that, if you update JS or WASM code or Tera templates, you
do not need to restart the server - it will pick up changes
automatically.

When testing, you do not need to `make bundled-sources` more than
once, at the beginning.  So don't, because it's slow.  But you
definitely should run it for every update if you make a deployment for
other people to use.  Otherwise you might be running a privately
modified server without offering your users its source code.  See
LICENCE.

If you Do Something to the output from cargo, you should `rm stamp/*`,
since the `Makefile` won't notice, otherwise, that the relevant cargo
rune(s) need to be re-run.  Needlessly deleting all the stamp files
wastes only a handful of seconds (on my stupidly fast laptop).
Deleting the stamp files is not needed if you simply edit Rust source
files.


Adding shapes (pieces, cards, etc.) to the builtin library
----------------------------------------------------------

(Consider making an uploadable game bundle instead.)

Otter's builtin shape libraries use SVGs.  The sources for the SVGs
are all in the otter source tree, in the `library/` directory.

Each shape is listed in one of the ``library/*.toml`` files, in a
`files` entry.  (Most of) the syntax and semantics of this file are
documented in the Rustdoc documentation for the module
`otter::shapelib_toml`.  If you run `make -j8 shapelib` it will print
out a `file://` url for these docs.
XXX

You can preview the shapes, including any changes you make, without a
whole game server, by running `make -j8 shapelib`, and looking at
`templates/shapelib.html`.  As above, this make rune will print the
`file://` url for you.

See the build instructions for information about how to install the
tools you will need.

Some of these SVGs were scraped from Wikimedia.  The scraper machinery
can perhaps be adapted to scrape SVGs from elsewhere.

You can also add your own SVGs in the `library/edited/` directory.  If
you do that, please make sure to include the actual source code there
too.  If you copied or adapted an SVG from somewhere, note the details
of where you got it (eg, in a git commit message).

Contributions should be via git branch, eg a merge request on Salsa:
[https://salsa.debian.org/iwj/otter](https://salsa.debian.org/iwj/otter)

NB that shapes must come with a licence compatible with CC-BY-SA 4.0
or GNU AGPLv3+.  See `LICENCE` for more information about copyright status.


Navigating the otter source code
--------------------------------

* `src/`

  The main Rust source code.  This is mixture of code used only or
  mainly by the server and code used by the `otter` command line
  utility; these aren't split up in a wholly principled way.  In Rust
  terms this is a "library crate".

* `src/bin/*.rs`

  Support executables, including in particular the command line
  utility `otter` which is used to set up and join games.

* `daemon/`

  The Otter server.  This is a simple binary crate.  Much
  functionality belonging primarily, or only, to the server is in
  `src/`, simply because it was easier not to disentangle it.
  Anything that needs Actix (the web framework) is in `daemon/`.
  Generally, we try to keep async confined to there too.

* `base/`

  Code shared by the host and the WebAssembly.  Notably, the Z
  coordinate handling, but also a a few other minor functions needed
  by both client and server.  To avoid duplication they are written
  once in Rust and compiled twice - once for the host and once for
  WebAssembly for use in the client.  This crate is kept fairly small
  to keeep the WebAssembly binary small (currently, ~140kby).

* `wasm/`

  WebAssembly/Rust bindings for the items in `base/`.  Produces the
  single wasm file for use by the JavaScript, and corresponding
  Typescript annotations etc.

* `templates/script.ts`

  The main Typescript (typed Javascript) code.  Otter's web
  compatibility target is the earliest browser versions that properly
  support WebAssembly.

* `templates/session.tera`, `macros.tera`, etc.

  Tera templates generating the main HTML screen.  These templates are
  filled in from structs in the Rust source code.  The main files are
  `session.tera` (portrait), `landscape.tera`, and `macros.tera`
  (common), and their rendering uses an instance of
  `SessionRenderContext` from `src/session.rs`.

* `nwtemplates/`

  "Non-web templates".  Tera templates for things other than web
  pages.  Currently this includes the server's outgoing emails.  These
  are in a separate directory to distinguish them from the templates
  for the actix webserver's Tera instance, which loads everything
  in `templates/*.tera`.

* `apitest/`

  Tests of the server which use its APIs - specifically, the
  management socket (also used by the `otter` command line tool) and
  the web API, but which do not use any JavaScript.

  These are not standard Rust `#[test]` tests because they need to
  reinvoke themselves via `bwrap` for test isolation reasons, and
  because their dependencies are extensive and not properly capturable
  in Cargo.  They are run by `make check`.

  The file `apitest/apitest.rs` also contains code which is reused by
  the WebDriver tests.

* `wdriver/`

  WebDriver-based end-to-end tests.  Each `wdt-*.rs` is one test
  utility.  `wdriver.rs` is the library for these, and contains most
  of the heavy lifting.

  The tests produce a single portmanteau binary to reduce compile
  times.  You run it with `target/debug/wdriver --test=wdt-something`.

* `jstest/`

  Node.js--based unit-end tests.
  XXX explain

* `specs/`.  The table and game specs, as used directly by `otter`.

* `library/`: The shape libraries.

  The program `./media-scraper` (which is not run by the `Makefile`)
  reads `library/*.toml` for instructions and generates `files.make`
  fragments.  These fragments arrange to run `./usvg-processor` which
  launders SVGs through `usvg`.

  The shape libraries have a different, more relaxed, copyright
  licence.

* `webassembly-types`: A git-subtree of "WebAssembly Types".


Automatic in-browser tests (`wdriver`)
--------------------------------------

`make check` runs all the tests; `make wdt` runs only the webdriver
(in-browser) tests.  You can run an individual test with a rune like
this:

```
  OTTER_TEST_LOG=otter_webdriver_tests=trace CARGO_MANIFEST_DIR=~ian/Rustup/Game/server time target/debug/wdriver --test=wdt-simple --geckodriver-args=
```

(This rune has some example logging options in it, for you to change
if you like. You can omit the `CARGO_MANIFEST_DIR` for an in-tree
non-privsep build.)  After a test has run, you can find screenshots,
etc. in `tmp/wdt-simple` or whatever.  You can restart the same game
server setup as the test used, with the state left by the test, with a
rune like this:

```
  target/debug/daemon-otter tmp/wdt-simple/server-config.toml
```
and then play with it at these urls:
```
  http://localhost:8000/?kmqAKPwK4TfReFjMor8MJhdRPBcwIBpe
  http://localhost:8000/?ccg9kzoTh758QrVE1xMY7BQWB36dNJTx
```

(Yes, those are fixed game access links, hardcoded by the tests.
You can bookmark them.)
