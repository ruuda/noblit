# Them Orbit Merger

(Working title.)

This is a free, libre, implementation of an immutable append-only database,
inspired by Datomic. The implementation is a clean-room implementation from
scratch.

## Resources

 * [Deconstructing the Database by Rich Hickey][deconstr]
 * [The Datomic Data Model][datamodel]

[deconstr]: https://www.infoq.com/presentations/Deconstructing-Database
[datamodel]: https://docs.datomic.com/cloud/whatis/data-model.html

## Building

Orbit Merger builds with Rust 1.28.0, because this was the Rust version that
the latest two Ubuntu LTSes as well as Debian Testing shipped at the time of
its inception.

    $ cargo build --release
    $ target/release/orbitmerger

## License

Apache 2.0.
