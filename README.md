# Noblit

**This is vaporware. Much of the content below is hypothetical.**

This is a free, libre, implementation of an immutable append-only database
inspired by Datomic. A database is a set of (entity, attribute, value) tuples
called *datoms*. Datoms can be asserted and retracted. A retraction is recorded
as a new fact; it is not a delete.

Below is a demonstration through a film database. Let's first insert some films:

    -- See also example.py for the same example in a Python DSL, rather than in
    -- the made-up query language below.
    assert
      -- Create three new entities, and set the director.name attribute for them.
      -- The variables s, n, and t will refer to these entities henceforth.
      s director.name "Ridley Scott"
      n director.name "Christopher Nolan"
      t director.name "Quentin Tarantino"

      -- Create a new entity with three attributes. The type of the film.title
      -- attribute is string, the type of film.year is integer. The type of
      -- film.director is ref: it references an other entity.
      b film.title "Blade Runner"
      b film.director s
      b film.year 1982

      p film.title "Pulp fiction"
      p film.director t
      p film.year 1994

      m film.title "Memento"
      m film.director n
      m film.year 2000

      d film.title "Django Unchained"
      d film.director t
      d film.year 2012

      k film.title "The Dark Knight Rises"
      k film.director n
      k film.year 2012

Now we can get all films released in 2012:

    where
      -- Find all entities f, that have a film.year attribute with value 2012,
      -- and that have a film.title attribute. We don't restrict the value of
      -- the film.title attribute, but we do bind it to the variable "title".
      f film.year 2012
      f film.title title
    select
      -- Return the entity id of the film, along with the title.
      f, title

All films directed by Christopher Nolan:

    where
      nolan director.name "Christopher Nolan"
      f film.director nolan
      f film.title title
      f film.year year
    select
      year, title

List all known directors:

    where
      d director.name name
    select
      name

## Resources

 * [Deconstructing the Database by Rich Hickey][deconstr]
 * [The Datomic Data Model][datamodel]
 * [LevelDB implementation documentation][leveldb]
 * [InfluxDB Storage Engine Internals][influxdb]
 * [What You Always Wanted to Know About Datalog][datalog]
 * [Unofficial Guide to Datomic Internals][unofficial]

[deconstr]:   https://www.infoq.com/presentations/Deconstructing-Database
[datamodel]:  https://docs.datomic.com/cloud/whatis/data-model.html
[leveldb]:    https://github.com/google/leveldb/blob/1cb384088184be9840bd59b4040503a9fa9aee66/doc/impl.md
[influxdb]:   https://www.youtube.com/watch?v=rtEalnKT25I
[datalog]:    https://www.utdallas.edu/~gupta/courses/acl/papers/datalog-paper.pdf
[unofficial]: http://tonsky.me/blog/unofficial-guide-to-datomic-internals/

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
   become a concern. A time series database or durable message queue might be
   a better alternative.
 * Portability. Little-endian architectures that run Linux are the only target
   for now.

Implementation:

 * All data is expressed as an (entity, attribute, value, transaction, operation)
   tuple, like in Datomic.
 * There are four indexes that store these tuples in order: EAVT, AEVT, AVET,
   and VAET, like in Datomic.
 * The disk format consists of hitchhiker tree indices and a
   heap file for large values that cannot be inlined in the index.
   See also [Disk Format](doc/disk-format.md) and [htree](doc/htree.md).
 * In addition to the data being semantically immutable, trees on disk are also
   immutable, and the index files are append-only. They would need to be
   rewritten periodically if too much unreferenced tree nodes accumulate.
 * Alternatively, I could keep a free list, but that breaks the immutability
   property, and reusing old names (or addresses) for new things always makes
   caching a nightmare.
 * Getting durability right is very hard and not a first priority.
   Never updating things in place does make it easier though.
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

Noblit is licensed under the [Apache 2.0][apache2] license. It may be used in
free software as well as closed-source applications, both for commercial and
non-commercial use under the conditions given in the license. If you want to
use Noblit in your GPLv2-licensed software, you can add an [exception][except]
to your copyright notice. Please do not open an issue if you disagree with the
choice of license.

[apache2]:       https://www.apache.org/licenses/LICENSE-2.0
[except]:        https://www.gnu.org/licenses/gpl-faq.html#GPLIncompatibleLibs
