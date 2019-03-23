# Bug Tracker Example

*Note: This example is completely hypothetical, constructed to explore syntax
and requirements. You cannot actually run these queries.*

Suppose we would want to build a database for a bug tracker, where users can
file and comment on issues, and set and change certain properties. This is an
excellent use case for an immutable database: we want to record status changes
as new facts, but never lose historic data.

## Schema and Initial Provisioning

We start by setting up attributes to define the schema, similar to how we would
issue `CREATE TABLE` statements in a SQL database. Unlike in SQL, schema is
reified in Noblit. This means that the schema is data, and it can be manipulated
using the same query language as domain data.

    where
      $string_t db.type.name "string"
      $uint64_t db.type.name "uint64"
      $ref_t    db.type.name "ref"

    define
      attribute($a, $name, $type, $unique, $many):
        $a db.attribute.name $name
        $a db.attribute.type $type
        $a db.attribute.unique $unique
        $a db.attribute.many $many

    assert
      attribute($user_name,         "user.name",         $string_t, true,  false)
      attribute($issue_title,       "issue.title",       $string_t, true,  false)
      attribute($issue_description, "issue.description", $string_t, false, false)
      attribute($issue_priority,    "issue.priority",    $uint64_t, false, false)
      attribute($issue_author,      "issue.author",      $ref_t,    false, false)
      attribute($issue_comment,     "issue.comment",     $ref_t,    true,  false)
      attribute($comment_content,   "comment.content",   $string_t, false, false)
      attribute($comment_author,    "comment.author",    $ref_t,    false, false)

    where
      -- Lookp up the data types by name, so we can refer to these types when
      -- defining new attributes.
      $string_t db.type.name "string"
      $uint64_t db.type.name "uint64"
      $ref_t    db.type.name "ref"

    assert
      -- Note: these definitions are verbose. Noblit's query language does not
      -- offer abstractions such as functions to reduce verbosity. Rather, you
      -- can build abstractions on top of a client library. Because Noblit has
      -- a simple data model, and because Noblit accepts data structures rather
      -- than strings as queries, it is easy and safe to build such abstractions
      -- in your programming language, rather than into the query language.

      $author_name db.attribute.name   "author.name"
      $author_name db.attribute.type   $string_t
      $author_name db.attribute.unique true
      $author_name db.attribute.many   false

      $issue_title db.attribute.name   "issue.title"
      $issue_title db.attribute.type   $string_t
      $issue_title db.attribute.unique true
      $issue_title db.attribute.many   false

      $issue_description db.attribute.name   "issue.description"
      $issue_description db.attribute.type   $string_t
      $issue_description db.attribute.unique false
      $issue_description db.attribute.many   false

      $issue_priority db.attribute.name   "issue.priority"
      $issue_priority db.attribute.type   $uint64_t
      $issue_priority db.attribute.unique false
      $issue_priority db.attribute.many   false

      $issue_author db.attribute.name   "issue.author"
      $issue_author db.attribute.type   $ref_t
      $issue_author db.attribute.unique false
      $issue_author db.attribute.many   false
      -- Experimental idea: attribute constraints (like foreign keys).
      -- The author ref must point to an entity that has the "author.name"
      -- attribute.
      $issue_author db.constraint.has_attribute $author_name

      $comment_content db.attribute.name   "comment.content"
      $comment_content db.attribute.type   $string_t
      $comment_content db.attribute.unique false
      $comment_content db.attribute.many   false

      $comment_author db.attribute.name   "comment.author"
      $comment_author db.attribute.type   $ref_t
      $comment_author db.attribute.unique false
      $comment_author db.attribute.many   false
      $comment_author db.constraint.has_attribute author_name

      -- TODO: Add created date_time_offset to issue and comment.

      -- Finally, an issue can have multiple comments associated with it. We
      -- could model this relation in two ways: by giving the comment a
      -- "comment.issue" attribute, or by giving the issue an "issue.comment"
      -- attribute. For the sake of demonstration we will go with the latter.
      -- Note that because Noblit maintains an (attribute, entity, value) index
      -- as well as an (attribute, value, entity) index, finding all comments
      -- associated with an issue is efficient in any case.
      $issue_comment db.attribute.name   "issue.comment"
      $issue_comment db.attribute.type   $ref_t
      $issue_comment db.attribute.unique false
      -- We set "many" to true: an issue can have many comments; the attribute
      -- may be present zero or more times.
      $issue_comment db.attribute.many true
      $issue_comment db.constraint.has_attribute $author_name

    select
      $issue_title, $issue_description, $issue_priority, $issue_author

This will set up thee kinds of entities: authors, issues, and comments. These
kinds are only encoded in the attribute names, there are no tables like in a
traditional database. The attribute ids of `issue.title`, `issue.description`,
`issue.priority`, and `issue.author` will be returned.

Let's insert some initial data:

    assert
      $batty author.name "Roy Batty"

      $i issue.title "Lifespan is too short"
      $i issue.description "Can the maker repair what he makes? I want more life."
      $i issue.priority 0
      $i issue.author $batty

    select
      $i

Now we have one issue. The query returns the entity id of the issue. Let's close
the issue as wontfix. But wait a mintue ... we forgot to add a status attribute!
Let's define one right now.

    where
      $string_t db.type.name "string"
      $ref_t    db.type.name "ref"
    assert
      -- Define an enum for statuses, by defining a single "status.name"
      -- attribute that must have a unique value among all instances of this
      -- attribute.
      $status_name db.attribute.name   "status.name"
      $status_name db.attribute.type   $string_t
      $status_name db.attribute.unique true
      $status_name db.attribute.many   false

      -- Then we define the issue statuses. We reference the new attribute by
      -- variable here. It could not be referenced by name, because at the time
      -- the query is constructed, it does not yet exist.
      $open_unconfirmed $status_name "open/unconfirmed"
      $open_confirmed   $status_name "open/confirmed"
      $open_in_progress $status_name "open/in-progress"
      $closed_fixed     $status_name "closed/fixed"
      $closed_obsolete  $status_name "closed/obsolete"
      $closed_wontfix   $status_name "closed/wont-fix"

      -- Finally, an issue can have a status.
      $issue_status  db.attribute.name   "issue.status"
      $issue_status  db.attribute.type   $ref_t
      $issue_status  db.attribute.unique false
      $issue_status  db.attribute.many   false
      $issue_comment db.constraint.has_attribute $status_name

Note that at this point, although she `issue.status` attribute exists, our first
issue does not have the attribute. TODO: How to deal with implied attributes?
(Sort of like non-nullable, except that there is no null in Noblit, only the
presence or absence of an attribute.) Add a constraint that `issue.title`
implies `issue.status` (and the others)? And then we'd be forced to add a status
to all issues that don't have one in the same transaction that adds the
attribute. In any case, let's set the issue to open/confirmed. We could
reference the issue and status by entity id directly, but for demonstration
purposes, we will look it up by title instead. We marked the `issue.title` and
`status.name` attributes are unique, so this will affect at most one entity.

    where
      $issue issue.title "Lifespan is too short"
      $open status.name "open/confirmed"
    assert
      $issue issue.status $open

Now we can close the issue. Note that because the `issue.status` attribute has
its cardinality `attribute.many` set to false, we can't just assert a new
status. We also have to retract the old status. The change is atomic, in one
transaction.

    where
      $i issue.title "Lifespan is too short"
      $i issue.status $old_status
      $wontfix status.name "closed/wont-fix"

    retract
      $i issue.status $old_status

    assert
      $tyrell author.name "Eldon Tyrell"

      $c comment.content "The light that burns twice as bright burns half as long."
      $c comment.author $tyrell

      $i issue.comment $c
      $i issue.status $wontfix

## A Selection of Selects

Suppose we wanted to list all issues, ordered by priority:

    where
      $i issue.title $title
      $i issue.priority $p
    select
      $i, $p, $title
    order by
      $p asc

This will return triples of issue entity id, priority, and title, ordered by
priority (supposing we took lower integers to indicate more urgent issues).

TODO: How to filter on only open issues? Would need an `in` or `or` clause.

Suppose we wanted to list all issues created by Roy Batty:

    where
      $batty author.name "Roy Batty"
      $i issue.author $batty
      $i issue.title $title
    select
      $i, $title

Now we get the entity ids of the issues created by Roy, as well as their titles.

Suppose we want to find all issues where Rick Deckard left a comment:

    where
      $deckard author.name "Rick Deckard"
      $c comment.author $deckard
      $i issue.comment $c
      $i issue.title $title
    select
      $i, $title

We again get all issues and titles. We will get a cartesian product of issues
and comments by Deckard though: if Deckard commented twice, the issue will be in
the result set twice. TODO: How to address that? `select unique`? Or more
fine-grained control? Or just order by the result and uniq it in the client
library?

## Querying History

After our issue was created, the status changed from `open/unfonfirmed` to
`closed/wont-fix`. However, if we query just the status, we can only see the
current status. The query

    where
      $i issue.title "Lifespan is too short"
      $i issue.status $status
    select
      $status

returns `closed/wont-fix`. If we wanted to construct a timeline of the status
changes that the issue underwent, we could include historic datoms like so:

    where
      $i issue.title "Lifespan is too short"
    where historic
      $i issue.status $prev_status $t retract
      $i issue.status $new_status $t assert
    select
      $t, $prev_status, $new_status
    order by
      $t asc

In a regular `where` query, only datoms that have not been retracted will match.
A `where historic` query works differently: it queries *all* datoms, and it
matches both assertions and retractons alike. To filter these, a historical
query takes 5-tuples rather than 3-tuples. In addition to (entity, attribute,
value), the query takes a transaction id and operation (assert or retract). In
this case, we select `prev_status` and `new_status`, such that the status
`prev_status` was retracted on issue `i` in transaction `t`, and `new_status`
was asserted in the same transaction. This gets us a list of status changes,
ordered by transaction id.

Note that the query to locate the issue by title is not in a `where historic`
clause. If it were, we would get the status changes of all issues that had been
called *Lifespan too short* a t some point, not only the history of the issue
that currently has that title.

Transactions in Noblit are reified. They are entities that can have attributes.
Suppose we had added a `transaction.date_time_offset` attribute to every
transaction, and also a `transaction.initiator`. Then we could also find out
*when* the attribute changed, and *who* made that change.

    where
      $i issue.title "Lifespan is too short"
    where historic
      $i issue.status $prev_status $t retract
      $i issue.status $new_status $t assert
    where
      $t transaction.date_time_offset $time
      $t transaction.initiator $initiator
      $initiator author.name $initiator_name
    select
      $time, $initiator_name, $prev_status, $new_status
    order by
      $time asc, $t asc

Now we get a nice chronological list of status changes, along with the name of
the user who made the change.

TODO: Rename `author.name` to `user.name`?
TODO: Add section define transaction attributes.
