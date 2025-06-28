# Noblit key-value store

Revisiting this idea after a few years again, I want a key-value store that
supports:

 * Persistent data structure, to enable reading from olders snapshots, and to
   preserve old versions.
 * Append-only disk structure, to enable sync between devices by sending all the
   new pages (though maybe there needs to be GC of old pages at some point).
 * Ability to encrypt the database, but that should be easy with a virtual IO
   layer, we can do AES counter mode.
 * Bytestring keys, probably variable size, but with some reasonable limit.
 * Bytestring values, with larger values.
 * The ability to tombstone keys.

## Base data structure

 * Hithhiker tree, should nodes be fixed-size and do we store keys and values
   out of band? Or do we embed at least the keys? My previous Noblit trick of
   storing all data in arenas, does not work if we need to support deletes.
   Now, we may not _need_ to support deletes, but I want to use this for a
   high-churn system (e.g. short-lived access tokens), so it's better if it
   does not grow unbounded.
 * Idea: Interleave pages that contain tree nodes, with pages that contain key
   data. We can apply the same persistent data structure to such data pages,
   copy on write.
 * We can store values on such pages as well, then we may need to span multiple
   pages. Maybe those are special cases, large value pages.

So we have a single page store, and it can contain 3/4 types of page:

 * A tree node.
 * Strings.
 * Large string (initial page).
 * Large string (continuation page).

If we do a CoW update of a page that contains strings though, then we can have
multiple names for the same string. If we want to be able to GC pages
effectively, then we we would need to rewrite references to those strings, but
how to even find them?

Unless we introduce a virtual page indirection, which may be a good idea anyway
(and may be a bad idea for the append-only nature). Let's say we have a virtual
page number, and virtual pages are immutable, and we can only add new ones. They
are backed by physical pages. Physical pages are in principle immutable, but
they can get GC'd once we no longer need them, and we can use that location to
store other pages later on. If we can do that, we can use it to replace the
backing physical page for a virtual string page. That makes them mutable, but if
the string pages are append-only, that is fine, because the mutation is an
append to a part that we couldn't have referenced previously.

## Page store

We have a "physical" store that has _slots_. A slot is a page-sized byte range
in a file, indetified by page index. It can store multiple pages over time. We
have virtual pages identified by a page number, they are immutable but they can
be GC'd at some point. Then we have a table that maps virtual page numbers to
slots (and when we store a page in a slot, we store the page number as a sanity
check).

The mapping table is not entirely append-only, but entries progress through
stages: not yet stored, backed by physical, and evicted, and they can only go
forward through those stages. Except now for string pages, when we store a new
copy of a string page, we can advance previously stored virtual pages to be
backed by the new one.

How do we store the page store? Well, if we have a hitchhiker tree, and it can
store small i64 values inline, then we can use it to store this table. We can
even have two different hitchhiker trees stored in the same backing store, one
for the system, one for the user. Alternatively we could have a hash table which
is probably a lot faster, but how to persist it? It may not need to be persisted,
we can always rebuild it by walking the entire store. But that makes startup
slow. If we store the table in our own page store though, how do we read from
it? To resolve a virtual page to a physical one we need to have the table. Hmm,
let's store it externally then. Could be an append-only file of inserts into
this table that we replay at startup, and if during the replay we find that more
than half of the entries get overwritten, we abandon that file and we start a
new one. Or maybe we can already decide, when the file size is >2x the table
size, we start a new file.

Invariant of the page store: if we stored a virtual page i in slot k, then if we
reuse slot k to store a new page j, then j > i. Property: it does not matter
whether we update the store page first, or the table first, if they are
consistent then we accept the write, if they are inconsistent then we know the
page write was incomplete and we should discard it.
