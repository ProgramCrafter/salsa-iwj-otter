Building
========

Otter is mostly written in Rust.  The web UI frontend is written in
Typescript.  The shape libraries are SVGs and need SVG manipulation
libraries.  The main documentation is done with Sphinx.

You will need at least 10G of disk space, or more, and a good internet
connection.  Your computer will be compiling a lot of code.

These instructions were once tested on Debian 11 "bullseye" and Debian
10 "buster".  Otter won't build on Debian 9 "stretch" because the
Typescript compiler (tsc) is too old.  tsc 3.3.3333 is known to work.
Ubuntu 20.04LTS "focal" should work.

Setup
-----

1. Install the packaged build dependencies::

     sudo apt install build-essential cpio git curl zip        \
                      pkg-config libssl-dev                    \
                      node-typescript inkscape                 \
                      netpbm imagemagick libtoml-parser-perl   \
                      potrace                                  \
                      python3-sphinx python3-recommonmark

   And for running the tests (``make check``) you will need::

     sudo apt install bubblewrap xvfb moreutils firefox-esr


2. Install Rust.  This is most easily done with rustup_::

     curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

   and then follow the instructions about your ``PATH``.  If this rune
   alarms you, see below about Rust privsep.

.. _rustup: https://rustup.rs

3. Switch your Rust install to use Rust Nightly and add the WASM
   target::

     rustup default nightly
     rustup target add wasm32-unknown-unknown
     rustup component add miri # for the tests

   Unfortunately, it is possible that the Rust Nightly you find when
   you run this is missing some pieces, or is not compatible with the
   version of Otter you have.  The following is known to work with
   Otter 0.7.1::

     rustup default nightly-2022-03-19 # downloads 1bfe40d11 2022-03-18

   **If you just want to run the otter cli client over ssh to an existing server, or edit and preview the shape libraries (ie the piece shapes) you can stop here.**

4. For the tests, install the webdriver binary from Mozilla.  Visit

     https://github.com/mozilla/geckodriver/releases/tag/v0.28.0

   and just dump the resulting ``geckodriver`` binary on your ``PATH``.


Full build
----------

::

     git clone https://salsa.debian.org/iwj/otter
     cd otter
     make -j8 all bundled-sources && echo it worked

Expect to see ``it worked`` at the end.  If you don't see that, it
failed, and the error message is buried in the scrollback.

Build just the command line game management client `otter`
..........................................................

::
     cargo build -p otter-cli

and then copy ``target/debug/otter`` onto your path.

Alternatively, you can let ``cargo`` directly install and download the
latest released version:

::
     cargo install otter-cli

Build just the shape library preview
....................................

::

    make -j8 shapelib

And then open ``./templates/shapelib.html`` in your browser


curl|bash-ware; privsep
-----------------------

**If you are not the kind of person to worry about computer security -
especially your software supply chains - you can skip this part.**

If you follow the above instructions, you will have downloaded and
executed - and, therefore, trusted:

 * Various Debian packages - safe
 * Rustup (the Rust downloader/installer) - this is pretty safe
 * Rust itself - again, pretty safe
 * Otter itself - well, I wrote this; up to you.
 * 475 transitive dependencies of otter (from crates.io)
 * a geckodriver binary directly from mozilla

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


Apologia
........

Rust Nightly
````````````

This was needed almost solely because Rocket needs it.
Hopefully we will compile on stable shortly.

The many dependencies of Otter
``````````````````````````````

These are partly because actix is a large piece of software with
much functionality.  But also because I favoured my own programming
convenience and in some cases was experimenting with different
approaches.  In practice, it seems to me that once I'm using Actix
and WASM and resvg and so on, there is not that much to be gained
by trying to prune the dependencies of the otter package itself.

geckodriver (for the automated in-browser tests)
````````````````````````````````````````````````

This is done with a protocol called "WebDriver" which is a
cross-browser way to puppet a browser.  There is a thing called
"geckodriver" which converts that to a firefox-specific protocol
for the same purpose, called "Marionette".  (In practice all this
seems to have lots of bugs and misfeatures.)

AFAICT the usual approach for using geckodriver is to have it *bind to
a fixed TCP port accessible to all local programs*.  My wrapper
tooling arranges to run this in an ephemeral $HOME and a private
network namespace.

AFAICT the only practical way to get geckodriver is to download the
binary from Mozilla.
