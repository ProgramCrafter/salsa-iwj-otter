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
