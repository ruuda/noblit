# Noblit

**This is vaporware. Much of the content below is hypothetical.**

This is a free, libre, implementation of an immutable append-only database
inspired by Datomic.

## Resources

 * [Deconstructing the Database by Rich Hickey][deconstr]
 * [The Datomic Data Model][datamodel]
 * [LevelDB implementation documentation][leveldb]
 * [InfluxDB Storage Engine Internals][influxdb]
 * [What You Always Wanted to Know About Datalog][datalog]

[deconstr]:  https://www.infoq.com/presentations/Deconstructing-Database
[datamodel]: https://docs.datomic.com/cloud/whatis/data-model.html
[leveldb]:   https://github.com/google/leveldb/blob/1cb384088184be9840bd59b4040503a9fa9aee66/doc/impl.md
[influxdb]:  https://www.youtube.com/watch?v=rtEalnKT25I
[datalog]:   https://www.utdallas.edu/~gupta/courses/acl/papers/datalog-paper.pdf

## Design

Goals:

 * An embeddable library for queries.
 * An embeddable library for transactions.
 * Supporting datasets that do not fit in working memory.
 * Durability, although not at any price.
 * Handling moderate write workloads and moderately sized data.
 * Relational queries and a machine-first query format.
 * Supporting replicas for reads at some point.
 * Simplicity. First make it work, then make it fast. I am writing this down to
   remind myself to not micro-optimize. Complications should be justified by
   profiling measurements.
 * Few or no dependencies.

Non-goals:

 * Being production-ready. For now this is an experiment.
 * Deleting data or updating data in place.
 * Writing large values (say, larger than 64 KiB). A key-value store or document
   store might be better suited for this.
 * High write throughput. If you produce large volumes of data quickly, probably
   not all of it remains valuable for a long time, and storage space might
   become a concern. A time series database or persistent message queue might be
   a better alternative.
 * Portability. Little-endian architectures that run Linux are the only target
   for now.

Implementation:

 * All data is expressed as an (entity, attribute, value, transaction, operation)
   tuple, like in Datomic.
 * There are four indexes that store these tuples in order: EAVT, AEVT, AVET,
   and VAET, like in Datomic.
 * I am not yet sure about the disk format. One possibility is to use one
   LevelDB for each index and to be done with it, but I would like something
   with less dependencies and more control.
 * Alternatively, the disk format would be a log-structured merge tree (like
   LevelDB) based on B-trees. An immutable on-disk format seems simpler to me
   than something like fractal trees.
 * Getting durability right is very hard and not a first priority.
 * Rely on the kernel for caching and scheduling: use mmaps and blocking IO.

Remarks:

 * Consistent reads are free: fix the maximum transaction id at the first query,
   and exclude all tuples with a higher transaction id on subsequent queries.
 * Reads can scale horizontally as there is no coordination required.

## Building

Noblit builds with Rust 1.28.0, because this was the Rust version that the
latest two Ubuntu LTSes as well as Debian Testing shipped at the time of its
inception.

    $ cargo build --release
    $ target/release/noblit

## Disclaimer

The implementation is a clean-room implementation. The author is not familiar
with Datomic internals apart from the publicly available documentation.

## License

Apache 2.0.
