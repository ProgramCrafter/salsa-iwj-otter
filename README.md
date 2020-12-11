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

Adding shapes
-------------

Otter uses SVGs.  The sources for the SVGs are all in the otter source
tree, in the library/ directory.

Some of these SVGs were scraped from Wikimedia.  The scraper machinery
can perhaps be adapted to scrape SVGs from elsewhere.

You can also add your own SVGs in the library/edited/ directory.
If you do that, please make sure to include the actual source code.
If you copied or adapted an SVG from somewhere, provide details.

Contributions should be via git branch, eg a merge request on Salsa:
  https://salsa.debian.org/iwj/otter



BUILDING AND TESTING
====================

These instructions have been tested on Debian buster.

Setup
-----

You will need about 6Gby of disk space.

1. 
   sudo apt install build-essential git curl pkg-config libssl-dev node-typescript

2. Install Rust.  This is most easily done with rustup:

   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

and then follow the instructions about your PATH.  If this rune alarms
you, see below about Rust privsep.

3. Switch your Rust install to use Rust Nightly and add the WASM
target:

   rustup default nightly
   rustup target add wasm32-unknown-unknown

4. Install some build tools:

   cargo install usvg
   cargo install bundle-sources

This will put them in ~/.cargo/bin, which the rustup curl rune above
will have arranged to put on your PATH.

5. Install the version of wasm-pack with the option I need, which
upstream haven't taken (or refused) the MR for:

  git clone https://github.com/ijackson/wasm-pack.git -b cargo-opts
  cd wasm-pack
  cargo install

NB that wasm-pack will itself download and install more stuff when it
is run by the otter Makefile.


Build
-----

  git clone https://salsa.debian.org/iwj/otter
  cd otter
  make -j8



   

You will want to start with the git branch
  

You must use "make" to build the whole thing, although you can run
"cargo build" to build the 

You cannot build it just with `cargo`, you must use `make`.

You will also need various other utilities and dependencies.  See
below.  On my own laptop deployment is done with `make deploy` which
copies all the relevant sources into the `bundled-sources` directory,
which is accessible via the Otter web UI.  See the code in `Makefile`.

Dependencies
------------

 * Rust Nightly (sorry)

 * `cargo` and a willingness to let it download all the dependencies
   and run them, from crates.io.  You can use my `Cargo.lock.example`
   if you like.  I use a privsep scheme to avoid running stuff from
   cargo in my main account on my laptop.  Using the not properly
   released `nailing-cargo` program.

 * `tsc`, Microsoft's Typescript compiler.  The version in Debian
   buster will do.

 * `wasm-pack`, a program for manipulating WebAssembly files.  This
   too likes to run cargo and do god knows what.

 * `resvg`, a program for manipulating SVG files.

 * `bundle-rust-sources`, an un-released Rust package for publishing
   source code of Rust projects.

Weirdnesses:

 * For running on chiark I build with the Rust target
   `x86_64-unknown-linux-musl` which on my system is configured to
   produce a completely statically linked bionary:

```
[target.x86_64-unknown-linux-musl]
rustflags = ["-C", "target-feature=+crt-static"]
# ^ from https://stackoverflow.com/questions/31770604/how-to-generate-statically-linked-executables
```
