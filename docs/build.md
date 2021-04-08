Building
========

Otter is mostly written in Rust.  The web UI frontend is written in
Typescript.  The shape libraries are SVGs and need SVG manipulation
libraries.  The main documentation is done with Sphnix.

Setup
-----

Install the packaged build dependencies:


```
     sudo apt install build-essential cpio git curl     \
                      pkg-config libssl-dev             \
                      node-typescript inkscape bubblewrap \
                      netpbm imagemagick \
                      python3-sphinx python3-recommonmark
```


Install Rust.  This is most easily done with [rustup](https://rustup.rs)):

```
     curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

   and then follow the instructions about your `PATH`.  If this rune
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
```
     rustup default nightly-2021-01-26
```

4. Install the `usvg` SVG launderer, which we need for shape libraries

```
     cargo install usvg
```

   This will put it in `~/.cargo/bin`, which you presumably have on
   your `PATH` (or the above `rustup` and `cargo` runes wouldn't work).


** If you just want to edit and preview the shape libraries
   (ie the piece shapes) you can stop here **


5. Install some more build tools:

```
     cargo install bundle-sources
```


Build
-----

```
     git clone https://salsa.debian.org/iwj/otter
     cd otter
     make -j8 all bundled-sources
```

Or if you just want to edit the piece libraries:

```
    make -j8 shapelib
```
And then open `./templates/shapelib.html` in your browser


