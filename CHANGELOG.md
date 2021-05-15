Version UNRELEASED
==================

 * New "vatikan.game.toml", suitable for many variants of
   Manipulation Rummy.
 * New "private.table.toml" and "same-scope.table.toml" for
   less-public games.
 * otter(1) and otterlib(!) now honour OTTER_CLI_LOG
   (in Rust env_logger format).
 * otter(1) no longer acts on change_directory server config
   setting (but still resolves paths in config relative to that dir).
 * Change CLI to server management wire protocol to binary-framed
   MessagePack (was newline-framed JSON).
 * Build system and test suite improvements and fixes.
 * Per-game libraries; library-list now takes a game name.

 * Move game spec parsing from client to server
 * Server-side bundles
 * Better error messages, especially from otter(1)

Version 0.5.1
=============

This was the first public release.

[comment]: # Local variables:
[comment]: # mode: text
[comment]: # End:
