# Building

Noblit is a native library written in Rust. It comes with official client
libraries for Haskell, Python, and Rust.

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
