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

 * Document game and piece spec format.

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
