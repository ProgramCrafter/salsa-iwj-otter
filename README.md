OTTER - ONLINE TABLE TOP ENVIRONMENT RENDERER
=============================================

Otter is an online "table simulator" intended to be suitable for board
games and similar.

It is accessed from a web browser running JavaScript.  The server runs
on a convenationl Unix host.  Currently, joining a game requires a
unix shell account on the server.

Right now Otter is in an alpha state.


BUILDING
========

Otter is not so easy to build.  You will want to start with the git
branch
  https://salsa.debian.org/iwj/otter

You cannot build it just with `cargo`, you must use `make`.

You will also need various other utilities and dependencies - in some
cases, un-released dependencies or locally patched versions.  See
`Cargo.nail` and `Makefile`.  On my own laptop deployment is done with
`make deploy` which copies all the relevant sources into the
`bundled-sources` directory, which is accessible via the Otter web UI.

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

 * `Cargo.nail` contains a list of sibling directories of my Otter
   source tree, which on my machine is called `server`.  For several
   of these I sent patches upstream which have generally been
   accepted, but I need to tidy this up to switch to the upstream
   version.

 * The Rocket dependency in `Cargo.toml` is completely mad due
   to Cargo being awful.  I am hoping I can switch to an upstream
   Rocket now.

 * For running on chiark I build with the Rust target
   `x86_64-unknown-linux-musl` which on my system is configured to
   produce a completely statically linked bionary:

```
[target.x86_64-unknown-linux-musl]
rustflags = ["-C", "target-feature=+crt-static"]
# ^ from https://stackoverflow.com/questions/31770604/how-to-generate-statically-linked-executables
```
