where
  string_t db.type.name "db.type.string"
  uint64_t db.type.name "db.type.uint64"
  ref_t    db.type.name "db.type.ref"

assert
  user_name          db.attribute.name "user.name"
  user_name          db.attribute.type string_t
  user_name          db.attribute.unique true
  user_name          db.attribute.many false

  issue_title        db.attribute.name "issue.title"
  issue_title        db.attribute.type string_t
  issue_title        db.attribute.unique true
  issue_title        db.attribute.many false

  issue_description  db.attribute.name "issue.description"
  issue_description  db.attribute.type string_t
  issue_description  db.attribute.unique false
  issue_description  db.attribute.many false

  issue_priority     db.attribute.name "issue.priority"
  issue_priority     db.attribute.type uint64_t
  issue_priority     db.attribute.unique false
  issue_priority     db.attribute.many false

  issue_author       db.attribute.name "issue.author"
  issue_author       db.attribute.type ref_t
  issue_author       db.attribute.unique false
  issue_author       db.attribute.many false

  issue_comment      db.attribute.name "issue.comment"
  issue_comment      db.attribute.type ref_t
  issue_comment      db.attribute.unique true
  issue_comment      db.attribute.many false

  comment_content    db.attribute.name "comment.content"
  comment_content    db.attribute.type string_t
  comment_content    db.attribute.unique false
  comment_content    db.attribute.many false

  comment_author     db.attribute.name "comment.author"
  comment_author     db.attribute.type ref_t
  comment_author     db.attribute.unique false
  comment_author     db.attribute.many false

-- We selected nothing, so the output is empty.

┌──┐
│  │
├──┤
│  │
└──┘

where
  -- Hack to get a single assertion.
  -- TODO: Support empty where part.
  _ db.type.name "db.type.ref"

assert
  batty user.name "Roy Batty"

  i issue.title "Lifespan is too short"
  i issue.description "Can the maker repair what he makes? I want more life."
  i issue.priority 0
  i issue.author batty

select
  i

┌───────┐
│ i     │
├───────┤
│ # 119 │
└───────┘

where
  string_t db.type.name "db.type.string"
  ref_t    db.type.name "db.type.ref"

assert
  -- Define an enum for statuses, by defining a single "status.name"
  -- attribute that must have a unique value among all instances of this
  -- attribute.
  status_name db.attribute.name   "status.name"
  status_name db.attribute.type   string_t
  status_name db.attribute.unique true
  status_name db.attribute.many   false

┌──┐
│  │
├──┤
│  │
└──┘

where
  ref_t db.type.name "db.type.ref"

assert
  -- Then we define the issue statuses. We could not do this in the same
  -- transaction as before, because the "status.name" attribute did not yet
  -- exist.
  open_unconfirmed status.name "open/unconfirmed"
  open_confirmed   status.name "open/confirmed"
  open_in_progress status.name "open/in-progress"
  closed_fixed     status.name "closed/fixed"
  closed_obsolete  status.name "closed/obsolete"
  closed_wontfix   status.name "closed/wont-fix"

  -- Finally, an issue can have a status.
  issue_status db.attribute.name   "issue.status"
  issue_status db.attribute.type   ref_t
  issue_status db.attribute.unique false
  issue_status db.attribute.many   false

┌──┐
│  │
├──┤
│  │
└──┘

where
  issue issue.title "Lifespan is too short"
  open status.name "open/confirmed"
assert
  issue issue.status open

┌──┐
│  │
├──┤
│  │
└──┘

where
  i issue.title "Lifespan is too short"
  i issue.status old_status
  wontfix status.name "closed/wont-fix"

-- TODO: support retractions.
-- retract
--   i issue.status old_status

assert
  tyrell user.name "Eldon Tyrell"

  c comment.content "The light that burns twice as bright burns half as long."
  c comment.author tyrell

  i issue.comment c
  i issue.status wontfix

┌──┐
│  │
├──┤
│  │
└──┘

where
  i issue.title title
  i issue.priority p
select
  i, p, title

-- TODO: Support order by.
-- order by
--   p asc

┌───────┬───┬─────────────────────────┐
│ i     │ p │ title                   │
├───────┼───┼─────────────────────────┤
│ # 119 │ 0 │ "Lifespan is too short" │
└───────┴───┴─────────────────────────┘

where
  batty user.name "Roy Batty"
  i issue.author batty
  i issue.title title
select
  i, title

┌───────┬─────────────────────────┐
│ i     │ title                   │
├───────┼─────────────────────────┤
│ # 119 │ "Lifespan is too short" │
└───────┴─────────────────────────┘

where
  deckard user.name "Rick Deckard"
  c comment.author deckard
  i issue.comment c
  i issue.title title
select
  i, title

-- Rick left no comments on this issue.

┌───┬───────┐
│ i │ title │
├───┼───────┤
└───┴───────┘

where
  i issue.title "Lifespan is too short"

assert
  gaff user.name "Gaff"

  i issue.comment c
  c comment.content "It's a shame she won't live. But then again, who does?"
  c comment.author gaff

select
  c

┌───────┐
│ c     │
├───────┤
│ # 143 │
└───────┘

where
  i issue.title "Lifespan is too short"
  batty user.name "Roy Batty"

assert
  i issue.comment c1
  c1 comment.content "I've seen things you people wouldn't believe."
  c1 comment.author batty

  i issue.comment c2
  c2 comment.content "All those moments will be lost in time, like tears in rain."
  c2 comment.author batty

select
  c1, c2

┌───────┬───────┐
│ c1    │ c2    │
├───────┼───────┤
│ # 145 │ # 147 │
└───────┴───────┘

where
  i issue.title "Lifespan is too short"
  i issue.comment c
  c comment.author u
  c comment.content comment
  u user.name author
select
  c, author, comment

┌───────┬────────────────┬───────────────────────────────────────────────────────────────┐
│ c     │ author         │ comment                                                       │
├───────┼────────────────┼───────────────────────────────────────────────────────────────┤
│ # 139 │ "Eldon Tyrell" │ "The light that burns twice as bright burns half as long."    │
│ # 143 │ "Gaff"         │ "It\'s a shame she won\'t live. But then again, who does?"    │
│ # 145 │ "Roy Batty"    │ "I\'ve seen things you people wouldn\'t believe."             │
│ # 147 │ "Roy Batty"    │ "All those moments will be lost in time, like tears in rain." │
└───────┴────────────────┴───────────────────────────────────────────────────────────────┘
