OTTER - ONLINE TABLE TOP ENVIRONMENT RENDERER
=============================================

Otter is an online "table simulator" intended to be suitable for board
games and similar.

It is accessed from a web browser running JavaScript.  The server runs
on a convenationl Unix host.  Currently, joining a game requires a
unix shell account on the server.

I expect it to be used with a concurrent voice chat.

The game server does not currently have a built-in text chat system.
The game organiser can use the game server to distribute (and update)
voice chat and info links.

Right now Otter is in an alpha state.


JOINING A GAME
==============

In the simplest case:
```
  otter join-game unix:<user>::<game-name>
```
e.g.
```
  otter join-game unix:ijackson::test
```

See `otter --help` for further options, including setting your nick.

Currently when a new player joins a game (with the `otter` command),
all the other players must reload the page.


CREATING A GAME
===============

```
otter reset --reset-table local-users :test demo
                         /^^^^^^^^^^^  ^^^\ ^^^^'~ game spec
                         `table spec       game name
```

Here `local-users` refers to the file `local-users.table.spec` in the
Otter specs directory (`/volatile/Otter/specs` on chiark).  The table
spec file handles access control (and some other global properties)
This particular file says that all local shell account users may join
the game.

`:test` is the game name.  It starts with a colon, which means
implicitly `unix:<whoami>::test`.  Other people have to name the game
with the full name, with all three colons in it.

`demo` refers to the file `demo.game.spec`.  The "game spec" says what
shape table is and what pieces there are.  This is a simple demo game.
There is also `penultima` which is a work-in-progress set of pieces
suitable for fairy chess etc.

See `otter --help` for some more options.

Currently, resetting a game (or otherwise adding or removing pieces)
will mean all the players will get errors until they reload the page.


MAKING YOUR OWN GAME
====================

If you want to use existing piece shapes that Otter already knows
about, you can do this by providing a `<something>.game.toml` file.
The format of these files is a TOML document representing a GameSpec
as found in `src/spec.rs` in the Otter source code.

todo: use rustdoc to provide this somewhere.


ADDING SHAPES
=============

Otter uses SVGs.  The sources for the SVGs are all in the otter source
tree, in the `library/` directory.

Each shape is listed in one of the `library/*.toml` files, in a
`files` entry.  (Most of) the syntax and semantics of this file are
documented in the Rustdoc documentation for the module
`otter::shapelib_toml`.  If you run `make -j8 shapelib` it will print
out a `file://` url for these docs.

You can preview the shapes, including any changes you make, without a
whole game server, by running `make -j8 shapelib`, and looking at
`templates/shapelib.html`.  As above, this make rune will print the
`file://` url for you.  (See BUILDING AND TESTING for information
about how to install the tools you will need.)

Some of these SVGs were scraped from Wikimedia.  The scraper machinery
can perhaps be adapted to scrape SVGs from elsewhere.

You can also add your own SVGs in the library/edited/ directory.
If you do that, please make sure to include the actual source code.
If you copied or adapted an SVG from somewhere, provide details.

Contributions should be via git branch, eg a merge request on Salsa:
[https://salsa.debian.org/iwj/otter](https://salsa.debian.org/iwj/otter)

NB that shapes must come with a licence compatible with CC-BY-SA 4.0.
See `LICENCE` for more information about copyright status.


BUILDING AND TESTING
====================

You will need at least 6000 megabytes of disk space, or more, and a
good internet connection.  Your computer will be compiling a lot of
code.

These instructions have been tested on Debian buster.


Setup
-----

1. 
```
     sudo apt install build-essential cpio git curl     \
                      pkg-config libssl-dev             \
                      node-typescript inkscape
```

2. Install Rust.  This is most easily done with [rustup](https://rustup.rs)):

```
     curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

   and then follow the instructions about your `PATH`.  If this rune
   alarms you, see below about Rust privsep.

3. Switch your Rust install to use Rust Nightly and add the WASM
   target:

```
     rustup default nightly
     rustup target add wasm32-unknown-unknown
```

   Unfortunately, it is possible that the Rust nightly you find when
   you run this is missing some pieces.  The following is known to
   work (with otter from the time of writing):
```
     rustup default nightly-2021-01-26
```

4. Install the `usvg` SVG launderer, which we need for shape libraries

```
     cargo install usvg
```

   This will put it in `~/.cargo/bin`, which you presumably have on
   your `PATH` (or the above `rustup` and `cargo` runes wouldn't work).


** If you just want to edit and preview the shape libraries
   (ie the piece shapes) you can stop here **


5. Install some more build tools:

```
     cargo install bundle-sources
     git clone https://github.com/ijackson/wasm-pack.git -b cargo-opts
     cd wasm-pack
     cargo install
```

wasm-pack upstream haven't reviewed my merge request, so you need my
version.  NB that wasm-pack will itself download and install more
stuff when it is run by the Otter Makefile.


Build
-----

```
     git clone https://salsa.debian.org/iwj/otter
     cd otter
     make -j8 all bundled-sources
```

Or if you just want to edit the piece libraries:

```
    make -j8 shapelib
```
And then open `./templates/shapelib.html` in your browser


Ad-hoc tests
------------

In one shell:

```
     target/debug/daemon-otter server-test.toml
```

The server does not daemonise, and the default config there makes it
quite verbose.  So, in another shell:

```
    target/debug/otter                                               \
        --account server: --config server-test.toml --spec-dir=specs \
        reset --reset-table test server::test demo

    target/debug/otter                                               \
        --account server: --config server-test.toml --spec-dir=specs \
        join-game server::test
```

The URL printed can then be visited in a local browser.


Resetting/restoring things after tests, updating server, etc.
-------------------------------------------------------------

After the server is updated, you can just `^C` and restart it.  Games
are constantly saved (although there is an up-to-1s lag on the most
frequently udpated game state).

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
since the `Makefile` won't notice, otherwise, that, the relevant cargo
rune(s) need to be re-run.  Needlessly deleting all the stamp files
wastes only a handful of seconds (on my stupidly fast laptop).


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

  The Otter server.  This is a simple binary crare.  Much
  functionality belonging primarily, or only, to the server, is in
  `src/`, simply because it was easier not to disentangle it.
  Anything that needs Rocket (the web framework) is in `daemon/`.

* `zcoord/`

  Code shared by the host and the WebAssembly.  Notably, the Z
  coordinate handling, but also a a few other minor functions needed
  by both client and server.  To avoid duplicating them are written
  once in Rust and compiled twice - once for the host and once for
  WebAssembly for use in the client.  This crate is kept minimal to
  keeep the WebAssembly binary small.

* `wasm/`

  WebAssembly/Rust bindings for the items in `zcoord/`.  Produces the
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

  "Non-web templataes".  Tera templates for things other than web
  pages.  Currently this includes the server's outgoing emails.  These
  have to be in a separate directory because Rocket likes to load
  everything applicable it finds in its own `templates/` directory.
  These are used via `src/nwtemplates.rs`.

* `wdriver.rs`, `wdriver/`

  WebDriver-based end-to-end tests.  Each `wdt-*.rs` is one test
  utility.  `wdriver.rs` (in the top level to evade Cargo's
  dur-brained search rules) is the library for these, and contains
  most of the heavy lifting.

  These are not standard Rust `#[test]` tests because they need to
  reinvoke themselves via `bwrap` for test isolation reasons, and
  because their dependencies are extensive and not properly capturable
  in Cargo.  They are run by `make check`.

* `library/`: The shape libraries.

  The program `./media-scraper` (which is not run by the `Makefile`)
  reads `library/*.toml` for instructions and generates `files.make`
  fragments.  These fragments arrange to run `./usvg-processor` which
  launders SVGs through `usvg`.  `usvg-processor`.

  The shape libraries have a different, more relaxed, copyright
  licence.


Automatic in-browser tests
--------------------------

* `apt install firefox`

* `https://github.com/mozilla/geckodriver/releases/tag/v0.28.0`
  download appropriate tarball, put "geckodriver" on PATH

`make check` runs all the tests; `make wdt` runs only those tests.  You can run
an individual test with a rune like this:

```
  OTTER_TEST_LOG=otter_webdriver_tests=trace CARGO_MANIFEST_DIR=~ian/Rustup/Game/server time target/debug/wdt-simple --geckodriver-args=
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
and then play with it at this url:
```
  http://localhost:8000/?kmqAKPwK4TfReFjMor8MJhdRPBcwIBpe
```


Rust, cargo, curl|bash-ware; privsep
------------------------------------

If you follow the above instructions you will have downloaded and
executed - and, therefore, trusted:

 * Various Debian packages - safe
 * Rustup (the Rust downloader/installer) - this is pretty safe
 * Rust itself - again, pretty safe
 * Otter itself - well, I wrote this; up to you.
 * My branch of wasm-pack - I haven't audited what I started with.
 * 300 transitive dependencies of otter (from crates.io)
 * 50 transitive dependencies of bundle-sources
 * the transitive dependencies of resvg
 * god knows how many transitive dependencies of wasm-pack
 * a geckodriver binary directly from mozilla
 * whatever wasm-pack downloads at runtime (mostly(?) via cargo)

You will have trusted the integrity of the following:

 * The Debian archive (via its apt keyring) (very good)
 * Rustup's and Rust's TLS keyholders (good, I think)
 * The HTTP TLS cabal (sigh)
 * github (pretty good in practice)
 * whatever mozilla do to make binaries, in particular geckodriver
 * crates.io (extremely poor traceability)
 * the project management of hundreds of random crates.io libraries

If this makes you uncomfortable, as it should, you may wish to
consider running everything in a separate shell account, or a VM or
container of some kind.

(I have a not-properly-released tool called "nailing-cargo" which
makes it possible to do most things in my main account but run the
Rust stuff in a separate less-privileged account.  There is support
for this in the Makefile.  But if you want to run *everything* in the
lesser account, you don't need to bother with that.)


Dependencies - apologia
-----------------------

 * Rust Nightly

   This is needed almost solely because Rocket needs it.  Rocket is
   the web framework I am using.  The next version of Rocket (0.5.x),
   which is in development, will not need Nightly, but it will also be
   a serious compatibility break.  The existing Rocket (0.4.x) will
   almost certainly never be ported to Stable Rust.  When Rocket 0.5.x
   is out, porting Otter to it will go on my list - but it won't be
   trivial.  Sorry.

 * The many dependencies of Otter

   These are partly because Rocket is a large piece of software with
   much functionality.  But also because I favoured my own programming
   convenience and in some cases was experimenting with different
   approaches.  In practice, it seems to me that once I'm using Rocket
   and WASM utilities and resvg and so on, there is not that much to
   be gained by trying to prune the dependencies of the otter package
   itself.

 * wasm-pack

   This is a wrapper program for various utilities for manipulating
   WebAssembly files, and their Typescript and Javascript glue, etc.
   It likes to run cargo and do god knows what.  I'm not sure it's
   buying me much over whatever things it runs, so ideally it would be
   best to replace this with calls to the underlying utilities and
   libraries.  But there are some wrinkles, for example, some version
   coupling requirements that wasm-pack takes care of.  And to be
   honest, I'm not sure precisely what it does and understanding that
   would be a necessary first step to reproducing it in the Makefile.

 * bundle-rust-sources

   This is mine, but it needs to be properly released.

 * geckodriver (for the automated in-browser tests)

   This is done with a protocol called "WebDriver" which is a
   cross-browser way to puppet a browser.  There is a thing called
   "geckodriver" which converts that to a firefox-specific protocol
   for the same purpose, called "Marionette".  (In practice all this
   seems to have lots of bugs and misfeatures.)

   AFAICT the usual approach for using geckodriver to have it *bind to
   a fixed TCP port accessible to all local programs*.  My wrapper
   tooling arranges to run this in an ephemeral $HOME and a private
   network namespace.

   AFAICT the only practical way to get geckodriver is to download the
   binary.  I got mine here:
     https://github.com/mozilla/geckodriver/releases/tag/v0.28.0 You
   You just dump the binary on your PATH.


Final weirdness
---------------

 * For running on chiark I build with the Rust target
   `x86_64-unknown-linux-musl` which on my system is configured to
   produce a completely statically linked bionary.  I have this in my
   `~/.cargo/config` (in the lesser privsep account):

```
[target.x86_64-unknown-linux-musl]
rustflags = ["-C", "target-feature=+crt-static"]
# ^ from https://stackoverflow.com/questions/31770604/how-to-generate-statically-linked-executables
```
