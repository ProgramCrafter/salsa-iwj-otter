Version 1.1.0 - 2022-05-26
==========================

Bundle format
-------------

Introduced bundle format versions, and bundle format version `2`.
Bundle format version `1` is still supported.  There are significant
changes and improvements in version `2`, available by writing
`format=2`.  See "Bundle compatibility" in the documentation.

 * Handling of piece size and angle specification overhauled.
 * New and better template substitution syntax.
 * Fixed piece angle specification in game specs.

Breaking change to otterlib CLI program
---------------------------------------

 * otterlib (the library preview utility, not normally needed for bundle
   development) now needs to know the nwtemplates directory (so it can
   properly process etc. magic library items).

New features
------------

 * Dice, via the `Die` piece type.
 * Currency (money, and similar things), via the `Currency` piece type.
 * Shape libraries can provide canned "magic" items
   (which are then based on the library's own image(s).
 * `stack_pos` parameter for `PickupDeck` pieces, specifying where the
   "neat pickup deck" should appear, so it doesn't have to central.
 * otter CLI client: Allow uploading a directory directly as a
   bundle zipfile (helpful for bundle development).
 * Include piece description in some log messages.

Fixes and improvements
----------------------

 * Do not randomly panic when trying to print truncated debug output
   for non-ASCII piece HTML.
 * Set default fonts for SVG processing: in bundle SVGs, at all (so
   that text is not elided); in builtin libraries, more widely
   (without effect on the existing pieces).
 * Notice errors during JS startup, and display them.
 * Correct sense of pin/unpin log message.
 * Shape library previews: fix (and improve) the HTML (from otterlib).
 * Fix a tiny rendering error in arrowheads.
 * Test suite: `childio`: work around a Linux kernel race bug: you can
   sometimes reap a child and then write to its pipe without EPIPE.
 * Build system: run miri with +nightly, so that tests work when stable
   Rust is the default.  Document that these tests require nightly.
 * Correct an error message about JSON serialisation.
 * Remove some direct stderr debug prints from production code.
 * Log message colourisation on a tty now leaves the logs more readable.
 * Some improvements to bundle/shapelib loading error messages.

Docs
----

 * Dcouemnt the semver stability promises.
 * Publish the docs to an Otter-version-specific directory, so that
   old docs versions are retained.
 * Fix some formatting errors and typos.
 * Correct some leftover todos.
 * Clarify extra field syntax in shape library catalogue `files` entries.

Protocol and model enhancements
-------------------------------

 * `multigrab`: pieces that can capture the number of pieces the user
   says to grab, and instead handle the whole multigrab operation.
 * `fastsplit`: pieces that can be split and merged during play
   without having to rewrite the aux save file (containing SVG images).
 * Non-mixing OccultIlk, for pieces which always remain distinguishable.
 * Special rendering instructions, for pieces whose display involves
   bespoke code in the TypeScript.
 * OpOutcomeThunk system to allow piece implementations to be reinvoked
   with an ability to make much wider mutable borrows.

Build system, internal changes, etc.
------------------------------

 * Update to Rust 2021.
 * Break out an additional `otter-support` crate (to try to
   improve incremental build times).
 * Use `ambassador` macro crate to help delegate trait impls.
 * When formatting the Rust docs, pass `--document-private-items`.
   (The Rust doc tree is for Otter developers.)
 * Rationalised and sanitised the item import (`use`) practices.
 * Refactoring in the test suite to support new tests.
 * Fake time facility for testing.
 * Concrete protocol: more Quiet PieceUpdateOps.
 * Work around inkscape extension fail-to-save bug (seen in recolour).
 * Reorganised and shuffled about `Outline` types.
 * Avoid saving various things to savefiles that aren't needed
   (mostly `None`s).
 * Add some internal documentation about `ModifyingPieces`.
 * Code motion to better organise shapelib.rs and utils.rs
 * Centralised some UI properties in ui.rs.
 * Made some type names more consistent (eg `GOccults`, `Catalogue`).
 * Renamed some PacketFrame types etc. from MgmtChannel.
 * Other minor improvements.


Version 1.0.0 - 2022-04-02
==========================

Significant changes
-------------------

 * Compile on stable Rust.
 * Uses actix-web rather than Rocket (should be similar in overall
   functionality but there may well be behavioural changes).
 * Always listen on localhost:8000 by default regardless of `debug` config.
   But, support explicit configuration of listening addresses.
 * Do not sometimes leak (in game log) piece identity of face-down cards.

Minor improvements
------------------

 * Add short crate-level docs with reference to website etc.
 * Fix inconsequential misacceptance of some odd-length hex byte strings.
 * Fix some almost-incosequential short write bugs (might manifest as races).
 * Typo fix in one error message.

Build system
------------

 * Turn on integer overflow checks for release builds.
 * Don't override environment's SPHINXBUILD or NAILING_CARGO.
 * Update dependency versions.
 * Skip some unnecessaary miri runs of certain tests.
 * Support for controlling the Rust version via RUST_VERSION.
 * `make deploy` MUSL build is in release mode by default.
 * Document that build now requires 6G rather than 10G.

Internal improvements
---------------------

 * Drop wee-alloc as allocator from WASM.
 * Many internal code style improvements prompted by rust-clippy.
 * Remove some unused internal bits and bobs.
 * Require layout parameter in session URL loads.
 * Miscellanous Other cleanups.


Version 0.7.3 - 2021-07-25
==========================

Build system bugfixes
---------------------

 * `cargo` publication in `make-release` uses better pre-clean
   approach.  (Removes some junk from caago packages.)

drop our own directory from cargo.nail

Version 0.7.2 - 2021-07-25
==========================

Bugfixes
--------

 * Complete overhaul of Z coordinate (stacking order) handling, including
   reworked Z lowering algorithm (`b` key function).  Changes and
   consequences include:

    - On page load, use player's view of the Z coordinate for sort order (Z
      display order) of occulted pieces, not actual Z (!)  avoiding
      incoherent client state and arbitrary behaviours eg:
    - Now we no longer ever raise a piece when user asks to lower (!!)
    - Occulters (hands and decks) more uniformly try to be low down.
    - e.g. user will no longer accidentally put card under their hand (!!)
    - Do not derange relative ordering of multiple lowered pieces.
    - Fix a logic errors and panics in Z coordinate handling which
      might prevent some attmepts to lower (or, mayby, raise) pieces.
    - When the user asks to raise but nothing can be raised, log a message.
    - Automatically lower occulting pieces when they are enabled, and
      pieces which are being pinned (insofar as reasonable); this
      significantly reduces the probability of mysterious trouble
      lowering a piece ("would disturb a piece held by another player").

 * Substantial overhaul of error handling for problems detected when
   processing client API requests:

    - It should no longer be possible for the user to cause JS exceptions or
      other lossage merely by asking to do things which it happens that the
      server (unbeknownst to user or client script.ts) won't permit.  Report
      these errors in the user-facing client log window, instead.
    - New discussion of inapplicable-api-op etc. error handling in
      `PROTOCOL.md`, and implementation of the new scheme.
    - Principled client decisions about what to show the user - which
      things are errors, which things are to be rported as conflicts, and
      generally what to put in the user-facing message log.

 * Do not re-raise things during drag if the user explicitly lowered
   them with `b` (since the user grasped them).

 * Fix JS exception if user selected multiple pieces which used the same key
   for different purposes.  (Even transiently, for example by selecting
   multiple hand repositories and claiming them all - you'd briefly have `C`
   for claim for some, and `C` for unclaim for others, triggering the bug.)

New features
------------

 * New general `t` keystroke to bring a piece to the top of the
   stacking order.

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

 * `vatikan` game spec: Make the two hand repositoriess at the bottom bigger.
 * Deck card count uses a monospaced font.
 * Demo game: Add a label to the test hand.

Internal and development/test changes
-------------------------------------

New tests (and new checks in existing tests):

 * Test Z lowering algorithm with new test facility.
 * Test handling of UI actions which server decides (unpredictably
   from client POV) cannot be performed.
 * Add more tests of the zcoord module.
 * Webdriver tests: check that there were no JS exceptions.
 * Add in-browser testing of inapplicable-api-op error handling.
 * Test new regrab feature, and adjust tests to cope with regrab
   feature's somewhat different reporting of simultaneous-drag-attempts.
 * Tests do a consistency check of the Z coordinates vs the in-SVG
   subelement stacking order.

Testing, supporting changes:

 * New jstest facility for running for-browser JS in a nodejs
   environment with some cheesy mockups.  (New `otter-nodejs-tests`
   crate; involves a separate invocation of `wasm-bindgen`.)
 * Webdriver tests: pass window size arguments to firefox so we
   get a window big enough for the provided game specs.  Involves
   wrapper script for `firefox`.  Also Xvfb server screen size.

Internal and protocol improvements:

 * New "loose" update (api op) concept to support new regrab feature.
 * Fix a wdriver synch race which could lead to spurious tests failures.
 * Improve defensive programming in the zcoord module.
 * Abolish the incoherent `OnlineError` error type and replace it
   with `Inapplicable` and `Fatal`.
 * Abolish old incoherent and ununused `conflict_expected` in client JS.
 * Improved debug output in various places (when enabled or in tests).
 * Cleaned up a few flabby idioms in some tests.
 * Promote some debugging support to otter-base.

Internal docs improvements:

 * Fix some bogus links in internal docs.
 * Add some more internal documentation to the zcoord module.
 * Tidy up some leftover comments etc.
 * Typo and formatting fixes in `PROTOCOL.md`.
 * Fixes to `cargo doc` build.

Build system and dependencies:

 * Update many dependencies to new versions.
 * Tested build with Rust nightly-2021-07-15.
 * Dependencies on tera templating engine slightly rationalised.
 * Maekfile: avoid rebuilding the otter cli over and over again,
   by touching it when we rebuild it.


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
