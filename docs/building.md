# Building

Noblit is a native library written in Rust. It will come with official client
libraries for Haskell, Python, and Rust.

_The Haskell client is under construction; the Rust and Python clients do not
yet exist._

## Build tools

A build environment in which all required build tools are available can be
created with [Nix][nix]. The repository contains a `default.nix` file that
defines the environment. All build tools are pinned for reproducibility. The
Nix environment is used for <abbr>CI</abbr>, so it is actively tested.

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

 * `libnoblit.so`, for dynamic linking against the <abbr>C ABI</abbr>.
 * `libnoblit.a`, for static linking against the <abbr>C ABI</abbr>.
 * `libnoblit.rlib`, for use in Rust programs.

TODO: What about C headers?
TODO: How to use in an application?

## Haskell client

The Haskell client is located in `client/haskell` and builds with Stack.
Currently the library supports Stackage LTS 13 (GHC 8.6). To build:

    $ cd client/haskell
    $ stack setup
    $ stack build

TODO: How does it find the Rust lib?
TODO: How to use in an application?

## Python client

The Python client library does not yet exist.

## Rust client

The Rust client library does not yet exist.

## Fuzz tests

Noblits internals are tested thoroughly through fuzz testing. See the `fuzz`
directory.

TODO: Expand these docs.
TODO: Should this be its own chapter?
