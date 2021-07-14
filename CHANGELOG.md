Version 0.8.0 - UNRELEASED
==========================

Bugfixes
--------

 * Complete overhaul of Z coordinate (stacking order) handling, including
   reworked Z lowering algorithm (`b` key function).  Changes and
   consequences include:
    - On page load, use player's view of the Z coordinate for sort order (Z
      display order) of occulted pieces, not actual Z (!)  resulting in
      incoherent client state and arbitrary behaviours eg:
    - Now we no longer ever raise a piece when user asks to lower (!!)
    - Occulters (hands and decks) more uniformly try to be low down.
    - e.g. user will no longer accidentally put card under their hand (!!)
    - Do not derange relative ordering of multiple lowered pieces.
    - Fix a Rust panic in Z coordinate handling which might occur in some
      attempts to lower pieces.
    - When the user asks to raise but nothing can be raised, log a message.

 * Substantial overhaul of error handling for problems detected when
   processing client API requests:
    - It should no longer be possible for the user to cause JS exceptions or
      other lossage merely by asking to do things which it happens that the
      server (unbeknownst to user or client script.ts) won't permit.  Report
      these errors in the user-facing client log window, instead.
    - New discussion of inapplicable-api-op etc. error handling in
      `PROTOCOL.md`, and implementation of the new scheme.
    - Principled client decisions about what to show the user.

 * Do not re-raise things during drag if the user explicitly lowered
   them with `l`.

 * Fix JS exception if user selected multiple pieces which used the same key
   for different purposes.  (Even transiently, for example by selecting
   multiple hand repositories and claiming them all - you'd briefly have `C`
   for claim for some, and `C` for unclaim for others.)

New features
------------

 * New general `t` keystroke to bring a piece to the top of the
   stackingorder.

 * Player hands now show the count of contained pieces.

 * New loosely synchronised approach to "regrab", to enable a player
   to release a piece (eg, a card they have just drawn) and
   immediately, asynchronously, regrab it, while the server is sending
   an update (eg resulting from an occultation status change).  I.e.:
     - You can now draw a card into your hand in Mao and immediately regrasp
       it, planning to decide whether to play it - and you can do the regrasp
       without waiting for the server to show you the card.  Your regrasp and
       the server update run concurrently.

Other user-facing changes
-------------------------

 * `vatikan` game spec: Make the two hands at the bottom bigger.
 * Deck card count uses a monospaced font.

Internal and development/test changes
--------------------------------

 * Demo game: Add a label to the test hand.
 * Dependencies on tera templating engine slightly rationalised.
 * New jstest facility for running for-browser JS in a nodejs
   environment with some cheesy mockups.  (New `otter-nodejs-tests`
   crate; involves running `wasm-bindgen` twice.)
 * Test Z lowering algorithm with new test facility.
 * Maekfile: avoid rebuilding the otter cli over and over again,
   by touching it when we rebuild it.
 * Webdriver tests: pass window size arguments to firefox so we
   get a window big enough for the provided game specs.  Involves
   wrapper script for `firefox`.  Also Xvfb server screen size.
 * Test handling of UI actions which server decides (unpredictably
   from client POV) cannot be performed.
 * Fix some bogus links in internal docs.
 * Add some internal documentation for zcoord module.
 * Improve defensive programming and testing in zcoord module.
 * Add more tests of zcoord module.
 * Tidy up some leftover comments etc.
 * Webdriver tests: check that there were no JS exceptions.
 * Improved debug output in various places (when enabled or in tests).
 * Testing of inapplicable-api-op error handling.
 * Typo and formatting fixes in `PROTOCOL.md`.
 * Abolish the incoherent `OnlineError` error type and replace it
   with `Inapplicable` and `Fatal`.
 * New "loose" update (api op) concept to support new regrab feature.
 * Test new regrab feature, and adjust tests to cope with regrab
   feature's somewhat different reporting of simultaneous-drag-attempt.
 * Promote some debugging support to otter-base.
 * Fixes to `cargo doc` build.
 * Abolish old incoherent and ununused `conflict_expected` in client JS.
 * Tests do a consistency check of the Z coordinates vs the in-SVG
   subelement stacking order.

Version 0.7.1 - 2021-06-09
==========================

Bugfixes
--------

 * `make shapelib` builds, and reports a URL for, the actual
   documentation for shape library layout and spec syntax, not to an
   obsolete location.

Installation and deployment changes
-----------------------------------

 * Better documentation of how to install just the command line
   game management client (eg for use via ssh).

 * Update Rocket dependency in Cargo.lock to one which builds with
   recent Nightly Rust.

 * Update usvg dependency in Cargo.toml to a version with revised
   less-insane command line parsing, and adjust all calls to it.

 * Updated other dependencies.

 * Work around a cargo bug which breaks the docs build:
    https://github.com/rust-lang/cargo/issues/9564

Installation and deployment changes
-----------------------------------

 * Improvements to release checklist and release script.  Notably,
   add a checklist item for checking the build on recent Nightly.


Version 0.7.0 - 2021-06-08
==========================

New features
------------

 * Game spec files can now be processed with Tera for templating.

 * ssh-based access for `otter` cli, including appropriately-
   restricted access via authorized_keys in the server role account,
   and a mechanism for self-service public key management.

 * `otter` cli can now read a preferences file
   (`~/.config/otter/prefs.toml` by default) to allow pre-configuring
   commonly-required command line options like `--game` and `--ssh`.
   See `otter --help` for documentation.

 * Mao and Penultima slightly updated (and, those game specs now
   use the new templating feature).

Bugfixes
--------

 * UI: When multiple pieces are selected and are to be raised (whether
   by explicit request, or because of drag distance), their relative
   ordering is preserved.  So dragging a group of pieces no longer
   scrambles their z order.

 * UI: When the SSE connection auto-reconnects, do not declare it an
   error and suggest to the user that reloading might help.  Instead,
   simply hope that it is going to be OK (since it generally is).  If
   the SSE connection declares failure (state 2), report a scarier
   message.

 * Docs: Fixes to some documentation errors (including a fix
   contributed by Simon Tatham).

 * CLI etc.: Improve a number of error messages; in particular,
   replace several "debug prints" of error messages with proper
   formatting, and print a better program kname.

 * CLI: Much better handling of stdout write errors.

Installation and deployment changes
-----------------------------------

 * The `otter` command line utility is now in its own Rust package
   `otter-cli`.

 * Better logging by server of events on command connections.

Internal and development changes
--------------------------------

 * make-release now polls crates.io's github view to work around
   cargo/crates.io bug https://github.com/rust-lang/cargo/issues/9507
   and has a release checklist in it.

 * Some internal renamings for clarity, especially regarding
   Authorisation proof tokens.

 * Reworked the types involved in management command channels, to
   use a different stacking of read/write adapters.  In particular,
   a new `childio` facility for handling conversations with a child
   process (in support of using `ssh` as a command conn transport).

 * `otter` cli no longer uses println!.  Instead we have a special
   wrapper for stdout which handles the errors and buffering for us.

 * `otter` cli subcommand dispatch made nicer and the program's code
   broken out into multiple files.

 * Internal apitest case code restructured somewhat.


Version 0.6.0 - 2021-05-23
==========================

New features
------------

 * Support uploading bundles of game materials, so games can be played
   that are not playable with Otter's provided piece libraries.

 * New `vatikan.game.toml`, suitable for many variants of
   Manipulation Rummy.

 * New `private.table.toml` and `same-scope.table.toml` for
   less-public games.

Command line usage changes, etc.
--------------------------------

 * otter(1) now takes the game name using a `--game` (`-g`) global
   option rather than a per-subcommand positional argument.

 * Library listing, piece identification, and so on, changed,
   including changes to `otter library-add` and `otter library-list`.
   Specifically, per-game libraries mean that `library-list` now
   needs the `--game` option.

 * otter(1) and otterlib(!) now honour `OTTER_CLI_LOG`
   (in Rust env_logger format).

 * otter(1) no longer acts on change_directory server config
   setting (but still resolves paths in config relative to that dir).

 * Much better reporting of errors, especially from otter(1).
   Previously even straightforward errors would result in a controlled
   `panic`; now a prettier message is printed.

 * Default table size is now 300,200 (like `penultima` and `mao`; was
   400,200).

  * otter(1) can be used to issue adhoc management commands supplied
    on the command line in JSON or RON format.

Bugfixes
--------

 * Fix a serious bug with acl handling which might allow players who
   can access a game more access than intended.

 * Occult ilks are properly sorted out during piece load and game
   save/load.  Previously a game with occulted pieces might not be
   reloadable.

 * Test suite might previously fail with EBADF due to off-by-one error
   in fd cleanup routiine.

Documentation
-------------

 * Document uploadable bundle format.

n * Document game and piece spec format.

 * Document shape library catalogue format (previously this was done
   with rustdoc annotations on Rust structs used with serde, which
   produced incomplete and very hard to understand information).
 
 * De-emphasize docs for for amending the builtin shape libraries;
   suggest making bundles instead.

 * Examples, of game specs, shape libs, buncdles.

Installation and deployment changes
-----------------------------------

 * usvg is now built shipped by the otter build system.

 * server-config.toml can specify the path to `usvg`, `libexec`,
   etc.

Compatibility notes
-------------------

 * The otter(1) command line interface has changed and many common
   operations need to be specified differently.

 * The management API protocol has completely changed.  Old otter(1)
   clients will not work.

 * Savefiles from older versions of Otter are not loadable.

Internal and development changes
--------------------------------

 * Change CLI to server management wire protocol to binary-framed
   MessagePack (was newline-framed JSON).

 * Update dependencies.

 * Build system and test suite improvements and fixes.

 * Move game spec parsing from client to server.

 * Better error messages, especially from otter(1)

 * Tests have better error reporting and output capturing.

 * Various substantial refactorings to tests.

 * Test the supplied game specs.

 * Test game save/load.

 * Use the sphinx feature sphinx.ext.autosectionlabel for xrefs.

 * Server management channel now has an idle timout.  Should not be
   relevant with the supplied otter(1) client.

 * Much internal refactoring, new macros etc., to support the other
   work.

 * Reset game (game spec parsing and implemnetation) is now
   done in the server rather than the client.
 
 * Improvements and bugfixes to `make-release` and `update-version`
   scripts.  Fix anomalous (but working) `Cargo.toml` version
   dependency syntax.

Version 0.5.1 - 2021-04-19
==========================

This was the first public release.

[comment]: # Local variables:
[comment]: # mode: text
[comment]: # End:
