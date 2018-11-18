# Noblit

This is a free, libre, implementation of an immutable append-only database,
inspired by Datomic. The implementation is a clean-room implementation from
scratch.

This is a prototype and a work in progress, there is little to see here.

## Resources

 * [Deconstructing the Database by Rich Hickey][deconstr]
 * [The Datomic Data Model][datamodel]
 * [LevelDB implementation documentation][leveldb]
 * [InfluxDB Storage Engine Internals][influxdb]

[deconstr]:  https://www.infoq.com/presentations/Deconstructing-Database
[datamodel]: https://docs.datomic.com/cloud/whatis/data-model.html
[leveldb]:   https://github.com/google/leveldb/blob/1cb384088184be9840bd59b4040503a9fa9aee66/doc/impl.md
[influxdb]:  https://www.youtube.com/watch?v=rtEalnKT25I

## Building

Noblit builds with Rust 1.28.0, because this was the Rust version that the
latest two Ubuntu LTSes as well as Debian Testing shipped at the time of its
inception.

    $ cargo build --release
    $ target/release/noblit

## License

Apache 2.0.
