OTTER - ONLINE TABLE TOP ENVIRONMENT RENDERER
=============================================

Otter is an online "table simulator" intended to be suitable for board
games and similar.

It is accessed from a web browser running JavaScript.  The server runs
on a convenationl Unix host.  Currently, joining a game requires a
unix shell account on the server.

The game does not have a built-in text chat system, nor any other
communication other than via moving the game pieces.  I expect it to
be used with a concurrent voice chat, or perhaps a concurrent text
chat program.  Right now the arrangements for the chat must be agreed
by the players without help from the Otter server.

Right now Otter is in an alpha state.


JOINING A GAME
==============

In the simplest case:
  otter join-game unix:<user>::<game-name>
e.g.
  otter join-game unix:ijackson::test

See otter --help for further options, including setting your nick.

Currently when a new player joins a game (with the `otter` command),
all the other players must reload the page.


CREATING A GAME
===============

otter reset --reset-table local-users :test demo
                         /^^^^^^^^^^^  ^^^\ ^^^^'~ game spec
                         `table spec       game name

Here "local-users" refers to the file "local-users.table.spec" in the
Otter specs directory (/volatile/Otter/specs on chiark).  The table
spec file handles access control (and some other global properties)
This particular file says that all local shell account users may join
the game.

":test" is the game name.  It starts with a colon, which means
implicitly "unix:<whoami>::test".  Other people have to name the game
with the full name, with all three colons in it.

"demo" refers to the file "demo.game.spec".  The "game spec" says what
shape table is and what pieces there are.  This is a simple demo game.
There is also "penultima" which is a work-in-progress set of pieces
suitable for fairy chess etc.

See otter --help for some more options.

Currently, resetting a game (or otherwise adding or removing pieces)
will mean all the players will get errors until they reload the page.


MAKING YOUR OWN GAME
====================

If you want to use existing piece shapes that Otter already knows
about, you can do this by providing a game.spec.toml file.  The format
of these files is a TOML document representing a GameSpec as found in
src/spec.rs in the Otter source code.

todo: use rustdoc to provide this somewhere.


ADDING SHAPES
=============

Otter uses SVGs.  The sources for the SVGs are all in the otter source
tree, in the library/ directory.

Unfortunately the mechanisms here are not yet particularly well
documented.

Some of these SVGs were scraped from Wikimedia.  The scraper machinery
can perhaps be adapted to scrape SVGs from elsewhere.

You can also add your own SVGs in the library/edited/ directory.
If you do that, please make sure to include the actual source code.
If you copied or adapted an SVG from somewhere, provide details.

Contributions should be via git branch, eg a merge request on Salsa:
  https://salsa.debian.org/iwj/otter

NB that shapes must come with a licence compatible with CC-BY-SA 4.0.
See LICENCE for more information about copyright status.


BUILDING AND TESTING
====================

You will need about 5500 megabytes of disk space, and a good internet
connection.  Your computer will be compiling a lot of code.

These instructions have been tested on Debian buster.

Setup
-----

1. 
     sudo apt install build-essential cpio git curl     \
                      pkg-config libssl-dev             \
                      node-typescript

2. Install Rust.  This is most easily done with rustup:

```
     curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

   and then follow the instructions about your PATH.  If this rune
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

     rustup default nightly-2020-11-09

4. Install some build tools:

```
     cargo install usvg
     cargo install bundle-sources
```

   This will put them in `~/.cargo/bin`, which you presumably have on
   your PATH (or the above `rustup` and `cargo` runes wouldn't work).

5. Install the version of wasm-pack with the option I need (upstream
   haven't reviewed my merge request):

```
     git clone https://github.com/ijackson/wasm-pack.git -b cargo-opts
     cd wasm-pack
     cargo install
```

  NB that wasm-pack will itself download and install more stuff when
  it is run by the otter Makefile.


Build
-----

```
     git clone https://salsa.debian.org/iwj/otter
     cd otter
     make -j8 all bundled-sources
```


Test
----

In one shell:

```
     target/debug/daemon-otter server-test.toml
```

The server does not daemonise, and the default config there makes it
quite verboxe.  So, in another shell:

```
    target/debug/otter \
        --account server: --config server-test.toml --spec-dir=specs \
        reset --reset-table test server::test demo

    target/debug/otter \
        --account server: --config server-test.toml --spec-dir=specs \
        join-game server::test
```

The URL printed can then be visited in a local browser.


Resetting/restoring things after tests, updating server, etc.
-------------------------------------------------------------

After the server is updated, you can just ^C and restart it.  Games
are constantly saved (although there is a 1s lag on the most
frequently udpated game state).

If you want to clear out the server state, delete the files `[ag]-*`
and `accounts`.  NB that you should do this with the server not
running, because the server has most of that information in memory and
will like to write it out again.

If you update Typescript (JS code) you will need to rerun make to
rebuild the JS output.

Apart from that, if you update JS or WASM code or Tera templates, you
do not need to restart the server - it will pick up changes
automatically.

When testing, you do not need to `make bundled-sources' more than
once, at the beginning.  So don't, because it's slow.  But you
definitely should run it for every update if you make a deployment for
other people to use.  Otherwise you might be running a privately
modified server without offering your users its source code.  See
LICENCE.


Navigating the otter source code
--------------------------------

* `src/*.rs`

  The main Rust source code.  This is mixture of code used only by the
  server and code used by the `otter` command line utility; these
  aren't split up.  In Rust terms this is a "library crate".

* `src/bin/*.rs`: The actual executables.

* `wasm/*.rs`: We ship WebAssembly bindings for a few things, mostly
  to avoid having to write the code twice - once in Rust and once in
  Typescript.

* `zcoord/*.rs`: Code shared by the host and the WebAssembly.
  Notably, the Z coordinate handling, but also a
  string-timestamp-handling function.  This is a separate library
  crate so that we don't have to compile Rocket for WASM...

* templates/script.ts

  The main Typescript (typed Javascript) code.  Otter's web
  targets is earliest browser versions that support WebAssembly.

* `templates/session.tera`, `macros.tera`, etc.

  Tera templates generating the main HTML screen.  These templates are
  filled in from structs in the Rust source code.  In the case of
  `session.tera` and `macros.tera`, the rendering uses an instance of
  `SessionRenderContext` from `src/session.rs`.

* `library/`

  The shape libraries.  The program `./media-scraper` (which is not
  run by the `Makefile`) reads `library/*.toml` for instructions and
  generates `files.make` fragments.  These fragments arrange to run
  `./usvg-processor` which launders SVGs through `usvg`.
  `usvg-processor`.


Rust, cargo, curl|bash-ware; privsep
------------------------------------

If you follow the above instructions you will have downloaded and
executed - and, therefore, trusted:

 * Various Debian packages - safe
 * Rustup (the Rust downloader/installer) - this is pretty safe
 * Rust itself - again, pretty safe
 * Otter itself - well, I wrote this; up to you.
 * My branch of wasm-pack - I haven't audited what I started with.

 * 236 transitive dependencies of otter (from crates.io)
 * 50 transitive dependencies of bundle-sources
 * the transitive dependencies of resvg
 * god knows how many transitive dependencies of wasm-pack
 * whatever wasm-pack downloads at runtime (mostly(?) via cargo)

You will have trusted the integrity of the following:

 * The Debian archive (via its apt keyring) (very good)
 * Rustup's and Rust's TLS keyholders (good, I think)
 * The HTTP TLS cabal (sigh)
 * github (pretty good in practice)
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
   almost certainly never be ported to Stable Rust.  Sorry.

 * The many dependencies of Otter

   These are partly because Rocket is a large piece of software with
   much functionality.  But also because I favoured my own programming
   convenience and in some cases was experimenting with different
   approaches.  In practice, it seems to me that once I'm using Rocket
   and WASM utilities and resvg and so on, there is not that much to
   be gained by trying to prune the dependencies of the otter package
   itself.

 * wasm-pack

   This is a wrapper a program for manipulating WebAssembly files.  It
   likes to run cargo and do god knows what.  I'm not sure it's buying
   me much over whatever things it runs, so ideally it would be best
   to replace this with calls to the underlying utilities and
   libraries.  But there are some wrinkles, for example, some version
   coupling requirements that wasm-pack takes care of.  And to be
   honest, I'm not sure precisely what it does so reproducing that in
   the Makefile would be nontrivial.

 * bundle-rust-sources

   This needs to be properly released.


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
