Advanced usage
==============

Adding pieces to a game in progress
-----------------------------------

Use the `otter library-list` and `otter library-add` commands, you can
add pieces to a game while it is in progress.

You may want to refer to the shape library preview ("Shapelib" link in
the game UI).


Defining your own game
----------------------

If you want to use existing piece shapes that Otter already knows
about, you can do this by providing a `<something>.game.toml` file.

The format of these files is a TOML document representing a `GameSpec`
as found in `src/spec.rs` in the Otter source code.  Unfortunately
this is not yet documented.


Adding shapes (pieces, cards, etc.)
-----------------------------------

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
