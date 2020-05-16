# Building

Noblit is a native library written in Rust. It will come with official client
libraries for Haskell, Python, and Rust.

_The Haskell and Python client are under construction; the Rust client does not
yet exist, but the Noblit crate itself can be used._

## Build tools

[Nix][nix] can set up a build environment in which all required build tools are
available. The repository contains a `default.nix` file that defines the
environment. All build tools are pinned for reproducibility. The Nix environment
is used for <abbr>CI</abbr>, so it is actively tested.

There are three ways to use the build environment:

 * Enter a shell in which all build tools are available with `nix run -c $SHELL`.
 * Prefix all commands with `nix run -c`.
 * Bring the binaries into your <abbr>PATH</abbr> with
    `export PATH=$(nix-build --no-out-link)/bin:$PATH`.

Using Nix is convenient, but not a requirement. You can source your build tools
elsewhere if you like.

[nix]: https://nixos.org/nix/

## Noblit

Noblit builds with Rust’s build tool Cargo. Noblit is developed and tested
against Rust 1.28.0, because this was the Rust version that the latest two
Ubuntu LTSes as well as Debian Testing shipped at the time of its inception.
Later versions of Rust may work. Noblit has no dependencies apart from the Rust
standard library.

To build:

    $ cargo build --release
    $ ls target/release/libnoblit*

This will have produced three libraries:

 * `libnoblit.so`, for dynamic linking against the C interface.
 * `libnoblit.a`, for static linking against the C interface.
 * `libnoblit.rlib`, for use in Rust programs.

If you need a header file, a script can generate one from the
[C interface documentation](reference/c.md):

    $ tools/gen_header.py > noblit.h

## Haskell client

The Haskell client is located in `client/haskell` and builds with Stack.
Currently the library supports Stackage LTS 13 (GHC 8.6). To build:

    $ cd client/haskell
    $ stack setup
    $ stack build

TODO: How does it find the Rust lib?
TODO: How to use in an application?

## Python client

The Python client is located in `client/python`. It loads `libnoblit.so` using
Python’s `ctypes` module. If loading fails for the unqualified path, the library
tries to load from `target/debug` and `target/release` to aid local development.
Python code does not need to be compiled, but it can be typechecked by
[Mypy][mypy]:

    $ mypy --strict client/python

[mypy]: https://mypy.readthedocs.io/en/stable/

## Rust client

Rust programs can use the `noblit` crate in the `noblit` directory directly,
although that exposes internals, and building queries is neithere convenient
nor type safe. It would be nice to add a layer on top that hides most of the
internals and exposes a more user-focused interface, but such a Rust client
does not yet exist.
