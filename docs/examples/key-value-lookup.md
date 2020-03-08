# Key-value lookup example

This example shows how to use Noblit as a key-value store. In particular, we
will be inserting password hashes and counts from a [Have I Been Pwned][hibp]
dump into a database, so we can check whether a given password occurred in a
breach. A dump includes not only the <abbr>SHA1</abbr> hashes of breached
passwords, but also their prevalence count. This is an excellent use case for a
key-value store: the <abbr>SHA1</abbr> hashes will be the keys, and the
prevalence counts will be the values.

A key-value store is a simple example, and not one that Noblit excels at.
We will import the Have I been Pwned dump into a new database in a single run,
and only read from the database after that. As such, this example does not
showcase Noblit’s history-related features. Also, because Noblit is a
general-purpose database, it is not going to be as performant or convenient as
a specialized key-value store. Still, a key-value store is a good example to get
started with, before moving on to more complex schemas or queries.

[hibp]: https://haveibeenpwned.com/

## The code

You can find the [finished example][mainrs] in the _haveibeenpwned_ subdirectory
of the _examples_ directory in the root of the repository.

[mainrs]: https://github.com/ruuda/noblit/blob/hibp/examples/haveibeenpwned/src/main.rs

## Setting up the schema

_TODO: Write this section._

 * We have `pw.sha1` of type `bytes`.
 * We have `pw.count` of type `uint64`.

## Inserting

_TODO: Write this section._

 * Insert in batches, transactions must fit in memory.
 * Some fluff to read the dump file.

## Lookup

_TODO: Write this section._

    where
      pw pw.sha1 :sha1
      pw pw.count count
    select
      count
